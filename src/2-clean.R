# data cleaner
library(osrm)
library(cartography)
library(tmaptools)
library(stringr)
library(tidyverse)
library(janitor)
library(sf)
library(data.table)
library(lubridate)

clip.range <-
  20 #we don't want data beyond 20km from central station

# check objects in environment
if (exists("raw_city_frame")) {
  print("All the necessary objects available in environment.")
} else {
  print("Need to run the read script...")
  source("./1-load.R")
  print("...all the necessary objects now available in environment.")
}

ls()

#functions
csv.2.sf <- function(csv.file) {
  csv.file %>%
    as_tibble() %>%
    filter(longitude != "NA", latitude != "NA") %>%
    st_as_sf(
      coords = c("longitude", "latitude"),
      crs = st_crs(inner_cities),
      remove = FALSE
    )
}
point.shp.2.sf <- function(shp.file) {
  shp.file %>%
    st_transform(st_crs(inner_cities))
}

poly.shp.2.sf <- function(shp.file) {
  shp.file %>%
    st_transform(st_crs(inner_cities)) %>%
    st_buffer(0.000000001)
}

#inner cities
inner_cities <- raw_lga_2016_aust %>%
  filter(lga_name16 == "Sydney (C)" |
           lga_name16 == "Melbourne (C)") %>%
  transmute(
    id = lga_name16,
    city = ifelse(lga_name16 == "Sydney (C)", "City of Sydney", "City of Melbourne"),
    source = "https://www.abs.gov.au/websitedbs/D3310114.nsf/Home/2016%20DataPacks"
  ) %>%
  st_cast("POLYGON") %>% #City of Sydney includes two tiny islands that are irrelvant for parking and wreck labels
  slice(3:n())

inner_cities <- raw_city_frame %>%
  st_transform(crs = st_crs(inner_cities)) %>%
  transmute(id = "BCC Inner City Frame",
            city = "Brisbane City Council",
            source = "Request from Brisbane City Council") %>%
  rbind(inner_cities)

st_write(
  inner_cities,
  "../outputs/inner_cities.shp",
  driver = "ESRI Shapefile",
  delete_layer = TRUE
)

inner_cities <- inner_cities %>%
  select(city)

#greater_cities
greater_cities <- raw_gccsa_2016_aust %>%
  st_transform(crs = st_crs(inner_cities)) %>%
  filter(
    gccsa_name == "Greater Brisbane" |
      gccsa_name == "Greater Sydney" |
      gccsa_name == "Greater Melbourne"
  ) %>%
  transmute(city = gccsa_name,
            source = "https://www.abs.gov.au/websitedbs/D3310114.nsf/Home/2016%20DataPacks")

st_write(
  greater_cities,
  "../outputs/greater_cities.shp",
  driver = "ESRI Shapefile",
  delete_layer = TRUE
)

#centrality
central_stations <- data.frame(
  id = "Central Station",
  longitude = c(153.0261831, 151.2069512, 144.9617319),
  latitude = c(-27.466224,-33.8831733,-37.8109836)
) %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = st_crs(inner_cities)) %>%
  st_join(., inner_cities) %>%
  mutate(source = "Google Maps Query")

#offstreet
sf1 <- poly.shp.2.sf(raw_seq_park_spaces_v2013) %>%
  mutate(
    type = ifelse(
      category_2 == "RESIDENTIAL" |
        category_1 == "RESIDENTIAL" |
        category_3 == "RESIDENTIAL",
      "residential",
      "other"
    )
  ) %>%
  replace_na(list(type = "other")) %>%
  mutate(
    id = address,
    bays = parks_2016,
    bays = ifelse(bays < parks_2008, parks_2008, bays),
    bays = ifelse(bays < parks_2013, parks_2013, bays),
    residential_bays = ifelse(type == "residential", bays, 0),
    other_bays = ifelse(type == "other", bays, 0),
    state = "Queensland",
    source = "Transport and Main Roads"
  ) %>%
  select(id, residential_bays, other_bays, state, source)

sf1 <- sf1 %>%
  st_join(., inner_cities) %>%
  drop_na() %>%
  select(-city)

df1 <-
  raw_fes2012___parking_data %>% #https://opendata.transport.nsw.gov.au/dataset/off-street-parking or https://data.nsw.gov.au/data/dataset?tags=parking
  as_tibble() %>%
  mutate_all(funs(str_replace(., ",", ""))) %>%
  mutate_if(is.character, as.numeric, na.rm = TRUE) %>%
  transmute(
    id = as.character(block),
    residential_bays = sum_of_tennantparkinginternal + sum_of_tennantparkingexternal,
    other_bays = sum_of_publicparkinginternal + sum_of_publicparkingexternal
  )

sf1 <- poly.shp.2.sf(raw_fes2017_employment_zone_data) %>%
  mutate(id = objectid,
         state = "New South Wales",
         source = "https://opendata.transport.nsw.gov.au/dataset/off-street-parking or https://data.nsw.gov.au/data/dataset?tags=parking") %>%
  left_join(df1, by = "id") %>%
  select(id, residential_bays, other_bays, state, source) %>%
  rbind(sf1)

df1 <- raw_off_street_car_parking_2016 %>%
  as_tibble() %>%
  transmute(
    id = as.character(block_id),
    type = ifelse(
      parking_type != "Residential",
      "other_bays",
      "residential_bays"
    ),
    parking_spaces
  ) %>%
  group_by(id, type) %>%
  summarize(parking_spaces = sum(parking_spaces)) %>%
  spread(type, parking_spaces, fill = 0)

sf1 <-
  poly.shp.2.sf(raw_geo_export_3f3122c2_4cfc_489b_8bd1_590a8876ad0b) %>%
  transmute(id = as.character(block_id)) %>%
  distinct(id) %>%
  left_join(df1, by = "id") %>%
  mutate(id,
         residential_bays,
         other_bays,
         state = "Victoria",
         source = "https://data.melbourne.vic.gov.au/Transport/Off-street-car-parks-with-capacity-and-type/krh5-hhjn") %>%
  rbind(sf1) %>%
  replace_na(list(residential_bays = 0,
                  other_bays = 0)) %>%
  mutate(state = factor(state),
         source = factor(source))

off_street_parking <- sf1 %>%
  mutate(total_parking = residential_bays + other_bays)

st_write(
  off_street_parking,
  "../outputs/off_street_parking.shp",
  driver = "ESRI Shapefile",
  delete_layer = TRUE
)

#onstreet
parking_meters <-
  csv.2.sf(raw_od_dataset_resource_parkingmetersdata_31aug2015) %>%
  transmute(
    id = meter_no,
    onstreet_bays = veh_bays,
    state = "Queensland",
    source = "https://data.gov.au/dataset/ds-brisbane-9378944d-2b4c-4b69-bd66-bc78088caab0/details"
  )

parking_meters <- point.shp.2.sf(raw_parking_meters) %>%
  transmute(
    id = meter_id,
    onstreet_bays = approx_pay_s,
    state = "New South Wales",
    source = "https://data.cityofsydney.nsw.gov.au/datasets/parking-meter-status/data"
  ) %>%
  plyr::rbind.fill(parking_meters)

parking_meters <-
  csv.2.sf(raw_on_street_car_parking_meters_with_location) %>%
  transmute(id = meter_id,
            state = "Victoria",
            source = "https://data.melbourne.vic.gov.au/Transport/On-street-Car-Parking-Meters-with-Location/vdsi-4gtj") %>%
  plyr::rbind.fill(parking_meters) %>%
  mutate(state = factor(state),
         source = factor(source))

st_write(
  parking_meters,
  "../outputs/parking_meters.shp",
  driver = "ESRI Shapefile",
  delete_layer = TRUE
)

onstreet_bays <-
  poly.shp.2.sf(raw_geo_export_31ea2fa0_2503_4897_86c3_5067b4a40357) %>%
  transmute(id = bay_id,
            state = "Victoria",
            source = "https://www.melbourne.vic.gov.au/about-council/governance-transparency/open-data/Pages/on-street-parking-data.aspx") %>%
  mutate(state = factor(state),
         source = factor(source))

st_write(
  onstreet_bays,
  "../outputs/onstreet_bays.shp",
  driver = "ESRI Shapefile",
  delete_layer = TRUE
)

onstreet_sensors <-
  csv.2.sf(raw_on_street_parking_bay_sensors2019) %>%
  transmute(
    id = st_marker_id,
    state = "Victoria",
    state = factor(state),
    source = "https://www.melbourne.vic.gov.au/about-council/governance-transparency/open-data/Pages/on-street-parking-data.aspx",
    source = factor(source)
  )

st_write(
  onstreet_sensors,
  "../outputs/onstreet_sensors.shp",
  driver = "ESRI Shapefile",
  delete_layer = TRUE
)

#pnr
pnr <- csv.2.sf(raw_pnr_parking_geocoded) %>%
  transmute(
    id = site,
    pnr_bays = x2016,
    state = "Queensland",
    source = "Request from Transport and Main Roads"
  )

pnr <- csv.2.sf(raw_locationfacilitydata) %>%
  transmute(
    id = location_name,
    park_n_ride = ifelse(grepl("car park", .$facilities), 'yes', 'no'),
    state = "New South Wales",
    source = "https://opendata.transport.nsw.gov.au/dataset/public-transport-location-facilities-and-operators"
  ) %>%
  filter(park_n_ride == 'yes') %>%
  plyr::rbind.fill(pnr) %>%
  st_as_sf()

sf1 <- point.shp.2.sf(raw_ptv_train_carpark) %>%
  transmute(
    id = station,
    pnr_bays = com_capac,
    state = "Victoria",
    source = "https://discover.data.vic.gov.au/dataset/ptv-train-station-car-park"
  ) %>%
  st_centroid(.)

pnr <- cbind(sf1 %>%
               as.tibble() %>%
               select(-geometry),
             sf1 %>%
               st_coordinates() %>%
               as_tibble()) %>%
  group_by(id, state, source) %>%
  summarise(pnr_bays = sum(as.numeric(pnr_bays)),
            X = mean(X),
            Y = mean(Y)) %>%
  st_as_sf(coords = c("X", "Y"), crs = st_crs(inner_cities)) %>%
  plyr::rbind.fill(pnr) %>%
  st_as_sf(crs = st_crs(inner_cities)) %>%
  cbind(., apply(st_distance(., central_stations, by_element = FALSE), 1, FUN =
                   min)) %>%
  rename("km_from_central" = names(.)[length(names(.)) - 1]) %>%
  mutate(km_from_central = km_from_central / 1000) %>%
  filter(km_from_central < clip.range)

st_write(pnr,
         "../outputs/pnr.shp",
         driver = "ESRI Shapefile",
         delete_layer = TRUE)

#Suburbs
##Spatial
sf1 <- raw_sa2_2016_aust %>% # shape
  filter(state_name == "Queensland" |
           state_name == "New South Wales" | state_name == "Victoria") %>%
  transmute(
    sa2_name,
    sa2_main,
    state_name,
    area_ha = as.numeric(st_area(.)) / 1000,
    area_ha = round(area_ha, 2)
  ) %>%
  st_join(greater_cities) %>%
  drop_na(city) %>%
  rename("greater_city" = "city") %>%
  st_join(inner_cities) %>%
  rename("inner_city" = "city") %>%
  cbind(., apply(
    st_distance(st_centroid(.), central_stations, by_element = FALSE),
    1,
    FUN = min
  )) %>%
  rename("km_from_central" = names(.)[length(names(.)) - 1]) %>%
  mutate(km_from_central = km_from_central / 1000,
         km_from_central = round(km_from_central, 2)) %>%
  cbind(., apply(st_distance(st_centroid(.), pnr, by_element = FALSE), 1, FUN =
                   min)) %>%
  rename("km_from_pnr" = names(.)[length(names(.)) - 1]) %>%
  transmute(
    area_ha,
    km_from_central,
    km_from_pnr = km_from_pnr / 1000,
    km_from_pnr = round(km_from_pnr, 2),
    id = sa2_name,
    sa2_maincode_2016 = as.numeric(sa2_main),
    state = factor(state_name),
    greater_city = factor(greater_city),
    inner_city = factor(inner_city),
    source = "www.abs.gov.au",
    source = factor(source)
  )

df1 <- sf1 %>%
  as_tibble() %>%
  select(greater_city, sa2_maincode_2016) %>%
  drop_na()

df2 <- sf1 %>%
  as_tibble() %>%
  select(inner_city, sa2_maincode_2016) %>%
  drop_na()

df3 <- raw_sa2_2011_to_sa2_2016 %>% # concordence
  as_tibble() %>%
  select(sa2_maincode_2011, sa2_maincode_2016, ratio)
##2011
df4 <- raw_sa2_ur_bedd_vehd_2011 %>% # cars per bedroom
  na_if("") %>%
  tidyr::fill(sa2, bedd) %>%
  filter(., count != 0) %>%
  filter(vehd != "Not stated") %>%
  filter(vehd != "Not applicable") %>%
  filter(vehd != "") %>%
  mutate(
    bedrooms = ifelse(
      bedd == "None (includes bedsitters)",
      "one_bedroom_2011",
      ifelse(
        bedd == "1 bedroom",
        "one_bedroom_2011",
        ifelse(
          bedd == "2 bedrooms",
          "two_bedrooms_2011",
          "over_two_bedrooms_2011"
        )
      )
    ),
    cars = stringr::str_extract_all(vehd, "[0-9]+"),
    cars = ifelse(cars == "character(0)", "0", cars),
    cars = as.numeric(cars),
    cars = cars * count,
    sa2_maincode_2011 = as.numeric(sa2)
  ) %>%
  left_join(df3, by = "sa2_maincode_2011") %>% # harmonise
  mutate(cars = cars * ratio,
         count = count * ratio) %>%
  group_by(sa2_maincode_2016, bedrooms) %>%
  summarise(count = sum(count),
            cars = sum(cars)) %>%
  mutate(cars = round(cars, 0),
         count = round(count, 0)) %>%
  filter(count > 9) %>% # taking jitter into account
  mutate(av_cars = round(cars / count, 2)) %>%
  select(-count,-cars) %>%
  spread(bedrooms, av_cars) %>%
  inner_join(df1, by = "sa2_maincode_2016") %>%
  as.tibble() %>%
  select(-greater_city)

df5 <- raw_sa2_ur_sa2_pow_mtwp_2011_bris %>% #mode
  na_if("") %>%
  tidyr::fill(sa2_ur, sa2_pow) %>%
  filter(., count != 0) %>%
  mutate(
    sa2_maincode_2011 = as.numeric(sa2_ur),
    sa2_maincode_2016 = as.numeric(sa2_pow),
    mtwp = stringr::str_to_lower(mtwp)
  ) %>%
  inner_join(df2, by = "sa2_maincode_2016") %>%
  group_by(sa2_maincode_2011, mtwp) %>%
  summarise(count = sum(count))

df5 <- raw_sa2_ur_sa2_pow_mtwp_2011_syd %>%
  na_if("") %>%
  tidyr::fill(sa2_ur, sa2_pow) %>%
  filter(., count != 0) %>%
  mutate(
    sa2_maincode_2011 = as.numeric(sa2_ur),
    sa2_maincode_2016 = as.numeric(sa2_pow),
    mtwp = stringr::str_to_lower(mtwp)
  ) %>%
  inner_join(df2, by = "sa2_maincode_2016") %>%
  group_by(sa2_maincode_2011, mtwp) %>%
  summarise(count = sum(count)) %>%
  rbind(df5)

df5 <- raw_sa2_ur_sa2_pow_mtwp_2011_melb %>%
  na_if("") %>%
  tidyr::fill(sa2_ur, mtwp) %>%
  filter(., count != 0) %>%
  mutate(
    sa2_maincode_2011 = as.numeric(sa2_ur),
    sa2_maincode_2016 = as.numeric(sa2_pow),
    mtwp = stringr::str_to_lower(mtwp)
  ) %>%
  inner_join(df2, by = "sa2_maincode_2016") %>%
  group_by(sa2_maincode_2011, mtwp) %>%
  summarise(count = sum(count)) %>%
  rbind(df5) %>%
  left_join(df3, by = "sa2_maincode_2011") %>% # harmonise
  mutate(count = count * ratio) %>%
  group_by(mtwp, sa2_maincode_2016) %>%
  summarise(count = sum(count)) %>%
  mutate(count = round(count, 0)) %>%
  inner_join(df1, by = "sa2_maincode_2016") %>%
  mutate(
    vehicle_2011 = ifelse(grepl("car|motorbike|taxi", mtwp), 1, 0),
    public_transport_2011 = ifelse(grepl("train|bus|ferry|tram", mtwp), 1, 0),
    active_transport_2011 = ifelse(grepl("bicycle|walk", mtwp), 1, 0),
    pnr_2011 = ifelse(vehicle_2011 == 1 &
                        public_transport_2011 == 1, 1, 0),
    vehicle_2011 = vehicle_2011 * count,
    public_transport_2011 = public_transport_2011 * count,
    active_transport_2011 = active_transport_2011 * count,
    pnr_2011 = pnr_2011 * count
  ) %>%
  select(-mtwp) %>%
  group_by(sa2_maincode_2016, greater_city) %>%
  summarise(
    inner_city_workers_2011 = sum(count),
    vehicle_2011 = sum(vehicle_2011),
    public_transport_2011 = sum(public_transport_2011),
    active_transport_2011 = sum(active_transport_2011),
    pnr_2011 = sum(pnr_2011)
  ) %>%
  transmute(
    sa2_maincode_2016,
    vehicle_2011 = ifelse(
      inner_city_workers_2011 < 10,
      NA,
      round(vehicle_2011 / inner_city_workers_2011 *
              100, 1)
    ),
    public_transport_2011 = ifelse(
      inner_city_workers_2011 < 10,
      NA,
      round(public_transport_2011 / inner_city_workers_2011 *
              100, 1)
    ),
    active_transport_2011 = ifelse(
      inner_city_workers_2011 < 10,
      NA,
      round(active_transport_2011 / inner_city_workers_2011 *
              100, 1)
    ),
    pnr_2011  = ifelse(
      inner_city_workers_2011 < 10,
      NA,
      round(pnr_2011 / inner_city_workers_2011 * 100, 1)
    ),
    inner_city_workers_2011 = ifelse(inner_city_workers_2011 < 10, NA,
                                     inner_city_workers_2011)
  )
##2016
df6 <- raw_sa2_bedrd_vehd_2016 %>%  # cars per bedroom
  na_if("") %>%
  tidyr::fill(sa2, bedrd) %>%
  filter(., count != 0) %>%
  filter(vehd != "Not stated") %>%
  filter(vehd != "Not applicable") %>%
  filter(vehd != "") %>%
  mutate(
    bedrooms = ifelse(
      bedrd == "None (includes bedsitters)",
      "one_bedroom_2016",
      ifelse(
        bedrd == "One bedroom",
        "one_bedroom_2016",
        ifelse(
          bedrd == "Two bedrooms",
          "two_bedrooms_2016",
          "over_two_bedrooms_2016"
        )
      )
    ),
    cars = stringr::str_extract_all(vehd, "[0-9]+"),
    cars = ifelse(cars == "character(0)", "0", cars),
    cars = as.numeric(cars),
    cars = cars * count,
    sa2_maincode_2016 = as.numeric(sa2)
  ) %>%
  group_by(sa2_maincode_2016, bedrooms) %>%
  summarise(count = sum(count),
            cars = sum(cars)) %>%
  filter(count > 9) %>%
  mutate(av_cars = round(cars / count, 2)) %>%
  select(-count,-cars) %>%
  spread(bedrooms, av_cars) %>%
  inner_join(df1, by = "sa2_maincode_2016") %>%
  as.tibble() %>%
  select(-greater_city)

df7 <- raw_sa2_ur_sa2_pow_mtwp_2016_bris %>% #mode
  na_if("") %>%
  tidyr::fill(sa2_ur, sa2_pow) %>%
  filter(., count != 0) %>%
  mutate(sa2_maincode_2016 = as.numeric(sa2_pow),
         mtwp = stringr::str_to_lower(mtwp)) %>%
  inner_join(df2, by = "sa2_maincode_2016") %>%
  mutate(sa2_maincode_2016 = as.numeric(sa2_ur)) %>%
  group_by(sa2_maincode_2016, mtwp) %>%
  summarise(count = sum(count))

df7 <- raw_sa2_ur_sa2_pow_mtwp_2016_syd %>%
  na_if("") %>%
  tidyr::fill(sa2_ur, sa2_pow) %>%
  filter(., count != 0) %>%
  mutate(sa2_maincode_2016 = as.numeric(sa2_pow),
         mtwp = stringr::str_to_lower(mtwp)) %>%
  inner_join(df2, by = "sa2_maincode_2016") %>%
  mutate(sa2_maincode_2016 = as.numeric(sa2_ur)) %>%
  group_by(sa2_maincode_2016, mtwp) %>%
  summarise(count = sum(count)) %>%
  rbind(df7)

df7 <- raw_sa2_ur_sa2_pow_mtwp_2016_melb %>%
  na_if("") %>%
  tidyr::fill(sa2_pow, sa2_ur) %>%
  filter(., count != 0) %>%
  mutate(sa2_maincode_2016 = as.numeric(sa2_pow),
         mtwp = stringr::str_to_lower(mtwp)) %>%
  inner_join(df2, by = "sa2_maincode_2016") %>%
  group_by(sa2_maincode_2016, mtwp) %>%
  mutate(sa2_maincode_2016 = as.numeric(sa2_ur)) %>%
  summarise(count = sum(count)) %>%
  rbind(df7) %>%
  mutate(count = round(count, 0)) %>%
  inner_join(df1, by = "sa2_maincode_2016") %>%
  mutate(
    vehicle_2016 = ifelse(grepl("car|motorbike|taxi", mtwp), 1, 0),
    public_transport_2016 = ifelse(grepl("train|bus|ferry|tram", mtwp), 1, 0),
    active_transport_2016 = ifelse(grepl("bicycle|walk", mtwp), 1, 0),
    pnr_2016 = ifelse(vehicle_2016 == 1 &
                        public_transport_2016 == 1, 1, 0),
    vehicle_2016 = vehicle_2016 * count,
    public_transport_2016 = public_transport_2016 * count,
    active_transport_2016 = active_transport_2016 * count,
    pnr_2011 = pnr_2016 * count
  ) %>%
  select(-mtwp) %>%
  group_by(sa2_maincode_2016, greater_city) %>%
  summarise(
    inner_city_workers_2016 = sum(count),
    vehicle_2016 = sum(vehicle_2016),
    public_transport_2016 = sum(public_transport_2016),
    active_transport_2016 = sum(active_transport_2016),
    pnr_2016 = sum(pnr_2016)
  ) %>%
  transmute(
    sa2_maincode_2016,
    vehicle_2016 = ifelse(
      inner_city_workers_2016 < 10,
      NA,
      round(vehicle_2016 / inner_city_workers_2016 *
              100, 1)
    ),
    public_transport_2016 = ifelse(
      inner_city_workers_2016 < 10,
      NA,
      round(public_transport_2016 / inner_city_workers_2016 *
              100, 1)
    ),
    active_transport_2016 = ifelse(
      inner_city_workers_2016 < 10,
      NA,
      round(active_transport_2016 / inner_city_workers_2016 *
              100, 1)
    ),
    pnr_2016  = ifelse(
      inner_city_workers_2016 < 10,
      NA,
      round(pnr_2016 / inner_city_workers_2016 * 100, 1)
    ),
    inner_city_workers_2016 = ifelse(inner_city_workers_2016 < 10, NA,
                                     inner_city_workers_2016)
  )

SA2_data <- sf1 %>%
  as_tibble() %>%
  select(-geometry) %>%
  left_join(df4, by = "sa2_maincode_2016") %>%
  left_join(df5, by = "sa2_maincode_2016") %>%
  left_join(df6, by = "sa2_maincode_2016") %>%
  left_join(df7, by = "sa2_maincode_2016")

saveRDS(SA2_data, file = "../outputs/SA2_data.RData")

SA2_shape <- sf1 %>%
  select(sa2_maincode_2016)

st_write(SA2_shape,
         "../outputs/SA2_shape.shp",
         driver = "ESRI Shapefile",
         delete_layer = TRUE)

#Upset Plot Data
df8 <- raw_sa2_ur_sa2_pow_mtwp_2011_bris %>%
  rbind(raw_sa2_ur_sa2_pow_mtwp_2011_melb) %>%
  rbind(raw_sa2_ur_sa2_pow_mtwp_2011_syd) %>%
  na_if("") %>%
  tidyr::fill(sa2_pow, sa2_ur, mtwp) %>%
  mutate(year = 2011) %>%
  filter(count != 0)

df8 <- raw_sa2_ur_sa2_pow_mtwp_2016_bris %>%
  rbind(raw_sa2_ur_sa2_pow_mtwp_2016_melb) %>%
  rbind(raw_sa2_ur_sa2_pow_mtwp_2016_syd) %>%
  na_if("") %>%
  tidyr::fill(sa2_pow, sa2_ur, mtwp) %>%
  mutate(year = 2016) %>%
  filter(count != 0) %>%
  rbind(df8) %>%
  mutate(sa2_maincode_2016 = as.numeric(sa2_pow),
         mtwp = stringr::str_to_lower(mtwp)) %>%
  inner_join(df2, by = "sa2_maincode_2016") %>%
  mutate(sa2_maincode_2016 = as.numeric(sa2_ur)) %>%
  inner_join(df1, by = "sa2_maincode_2016") %>%
  group_by(inner_city, year, mtwp) %>%
  summarise(count = sum(count))

upset_data <- df8 %>%
  mutate(
    private_transport = ifelse(grepl("car|motorbike|taxi|truck", mtwp), 1, 0),
    public_transport = ifelse(grepl("train|bus|ferry|tram", mtwp), 1, 0),
    active_transport = ifelse(grepl("bicycle|walk", mtwp), 1, 0),
    pnr_transport = ifelse(private_transport == 1 &
                             public_transport == 1, 1, 0),
    sustainability = ifelse(
      pnr_transport == 1,
      "PnR",
      ifelse(
        private_transport == 1,
        "Private",
        ifelse(
          public_transport == 1,
          "Public",
          ifelse(active_transport == 1, "Active", NA)
        )
      )
    ),
    car = ifelse(grepl("car", mtwp), 1, 0),
    motorbike = ifelse(grepl("motorbike", mtwp), 1, 0),
    taxi = ifelse(grepl("taxi", mtwp), 1, 0),
    train = ifelse(grepl("train", mtwp), 1, 0),
    bus = ifelse(grepl("bus", mtwp), 1, 0),
    ferry = ifelse(grepl("ferry", mtwp), 1, 0),
    tram = ifelse(grepl("tram", mtwp), 1, 0),
    bicycle = ifelse(grepl("bicycle", mtwp), 1, 0),
    walk = ifelse(grepl("walk", mtwp), 1, 0),
    truck = ifelse(grepl("truck", mtwp), 1, 0)
  ) %>%
  drop_na(sustainability) %>%
  select(inner_city:year, count, sustainability:truck)

saveRDS(upset_data, file = "../outputs/upset_data.RData")

# Licence Plate Survey
signage <- raw_signage %>%
  transmute(
    parking_bay = toupper(parking_bay),
    sign = gsub("  ", " ", signage_for_the_day, fixed = TRUE),
    sign = gsub("( ", "(", sign, fixed = TRUE),
    sign = factor(sign)
  )

licence_plates <- raw_licenceplates %>%
  pivot_longer(!timestamp, names_to = "parking_bay", values_to = "licence_plate") %>%
  na.omit() %>%
  mutate(
    parking_bay = toupper(parking_bay),
    licence_plate = as.character(licence_plate),
    licence_plate = toupper(licence_plate),
    licence_plate = gsub("\\?", " ", licence_plate),
    licence_plate = gsub("VIC", "", licence_plate, fixed = TRUE),
    licence_plate = gsub("TAXI", "", licence_plate, fixed = TRUE),
    licence_plate = gsub("SA", "", licence_plate, fixed = TRUE),
    licence_plate = gsub("NSW", "", licence_plate, fixed = TRUE),
    licence_plate = gsub("MOTOR BIKE", "", licence_plate, fixed = TRUE),
    licence_plate = gsub("ACT", "", licence_plate, fixed = TRUE),
    licence_plate = trimws(licence_plate),
    timestamp = dmy_hm(timestamp)
  ) %>%
  filter(licence_plate != "NA") %>%
  arrange(., licence_plate) %>%
  left_join(signage, by = "parking_bay")

saveRDS(licence_plates, file = "../outputs/licence_plates.RData")

unique_licence_plates <- licence_plates %>%
  select(licence_plate) %>%
  unique()

write.csv(unique_licence_plates, file = "../unique_licence_plates.csv")

# OD distance
unique_poa <-
  unique(raw_rqr32121_registrationplates_suburb_postcode$postcode) %>%
  as_tibble() %>%
  mutate(postcode = value) %>%
  select(postcode)

poa <- raw_poa_2016_aust %>%
  st_transform(crs = st_crs(inner_cities)) %>%
  transmute(postcode = as.numeric(poa_code16)) %>%
  st_centroid() %>%
  inner_join(unique_poa, by = "postcode")

boundary_st  <- data.frame(
  id = "Boundary St",
  longitude = c(153.012355),
  latitude = c(-27.479546)
) %>%
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = st_crs(inner_cities))

plate_route <- st_sf(st_sfc(crs = st_crs(inner_cities)))

for (i in unique(poa$postcode)) {
  print(i)
  sf1 <- poa %>%
    filter(postcode == i)
  sf2 <- osrmRoute(
    src = sf1,
    dst = boundary_st,
    overview = "full",
    returnclass = "sf"
  ) %>%
    mutate(postcode = i)
  plate_route <- sf2 %>%
    rbind(plate_route)
  Sys.sleep(0.1)
}

st_write(
  plate_route,
  "../outputs/plate_route.shp",
  driver = "ESRI Shapefile",
  delete_layer = TRUE
)

route_distance <- plate_route %>%
  as_tibble() %>%
  transmute(network_km = round(distance, 0),
            postcode)

write.csv(route_distance, file = "../route_distance.csv")

# combine licence plates and route distance
df1 <- raw_unique_licence_plates_matched %>%
  transmute(licence_plate,
            network_km = as.numeric(network_km))

licence_plates_dist <- licence_plates %>%
  mutate(duration = 15,
         day = wday(timestamp, label = TRUE, abbr = FALSE)) %>%
  group_by(licence_plate, parking_bay, day) %>%
  summarise(timestamp = min(timestamp),
            duration = sum(duration),
            sign = first(sign)) %>%
  left_join(df1, by = "licence_plate") %>%
  mutate(time = paste0(as.character(hour(timestamp)), ":", minute(timestamp)),
         hour = hour(timestamp))

saveRDS(licence_plates_dist, file = "../outputs/licence_plates_dist.RData")

# Intercept Survey
intercept_survey <- raw_interceptsurvey %>%
  mutate_if(is.character, str_to_sentence) %>%
  mutate_if(is.character, str_trim) %>%
  mutate(
    ans_survey_id = as.numeric(ref_num),
    ans_q1_car = ifelse(by_private_car == "Y", "Yes", "No"),
    ans_q1_car_passenger = ifelse(
      ans_q1_car == "No",
      NA,
      ifelse(x_if_so_were_you_dropped_off == "Y", "Yes", "No")
    ),
    ans_q1_car_parked = ifelse(
      ans_q1_car == "No",
      NA,
      ifelse(did_you_park == "Off-street", "Off-street", "On-street")
    ),
    ans_q1_car_regulation = ifelse(
      ans_q1_car == "No",
      NA,
      ifelse(
        was_the_parking == "Free/open",
        "Free/open",
        ifelse(was_the_parking == "Timed", "Timed", "Metered")
      )
    ),
    ans_q1_car_located = ifelse(
      ans_q1_car == "No",
      NA,
      ifelse(
        or_located == "Along boundary st",
        "Boundary St",
        ifelse(or_located == "Under coles", "Coles", "Elsewhere")
      )
    ),
    ans_q1_car_located_elsewhere = ifelse(ans_q1_car_located != "Elsewhere", NA, x_if_elsewhere),
    ans_q1_public_transport = ifelse(by_public_transport == "Y", "Yes", "No"),
    ans_q1_public_transport_train = ifelse(
      ans_q1_public_transport == "No",
      NA,
      ifelse(x_if_so_did_the_journey_include_train == "Y", "Yes", "No")
    ),
    ans_q1_public_transport_ferry = ifelse(
      ans_q1_public_transport == "No",
      NA,
      ifelse(x_ferry == "Y", "Yes", "No")
    ),
    ans_q1_public_transport_bus = ifelse(
      ans_q1_public_transport == "No",
      NA,
      ifelse(x_buses == "Y", "Yes", "No")
    ),
    ans_q1_public_transport_bus_n = ifelse(ans_q1_public_transport_bus != "Yes", NA, x_if_buses),
    ans_q1_taxi = ifelse(by_taxi == "Y", "Yes", "No"),
    ans_q1_ride_hail = ifelse(by_uber_or_similar_ride_hailing_services == "Y", "Yes", "No"),
    ans_q1_bicycle = ifelse(by_bicycle == "Y", "Yes", "No"),
    ans_q1_bicycle_ownership = ifelse(
      ans_q1_bicycle == "No",
      NA,
      if_else(
        x_if_so_did_you_use_a == "CityCycle",
        "CityCycle",
        "Private Bicycle"
      )
    ),
    ans_q1_bicycle_parked = ifelse(
      ans_q1_bicycle == "No",
      NA,
      if_else(
        x_if_by_private_bike_did_you_park_at == "Bike Rack",
        "Bike Rack",
        "Elsewhere"
      )
    ),
    ans_q1_bicycle_parked_elsewhere = ifelse(ans_q1_bicycle_parked != "Elsewhere", NA, x_if_elsewhere_2),
    # nobody used a scooter
    ans_q1_walk = ifelse(by_walking == "Y", "Yes", "No"),
    ans_q2_multimodal_1st = x1st_mode,
    ans_q2_multimodal_2nd = x2nd_mode,
    ans_q2_multimodal_3rd = x3rd_mode,
    #nobody used > 3 modes
    ans_q3_modal_preference = q3_why_was_this_your_preferred_mode_today,
    ans_q4_parking_preference = ifelse(
      ans_q1_car == "No" | ans_q1_car_passenger == "Yes",
      NA,
      q4_if_you_needed_to_park_what_factors_influenced_your_parking_location
    ),
    ans_q5_passing_through = ifelse(passing_through_to_suburb_or_location == "Y", "Yes", "No"),
    ans_q5_passing_through_to = ifelse(
      ans_q5_passing_through == "No",
      NA,
      passing_through_to_suburb_or_location
    ),
    ans_q5_working = ifelse(working_today == "Y", "Yes", "No"),
    ans_q5_working_start = ifelse(ans_q5_working == "No", NA, x_if_working_today_start),
    ans_q5_working_start = gsub(":", " ",  ans_q5_working_start),
    ans_q5_working_start = hms(ans_q5_working_start),
    ans_q5_working_finish = ifelse(ans_q5_working == "No", NA, x_if_working_today_finish),
    ans_q5_working_finish = gsub(":", " ",  ans_q5_working_finish),
    ans_q5_working_finish = hms(ans_q5_working_finish),
    ans_q5_shopping = ifelse(shopping == "Y", "Yes", "No"),
    ans_q5_shopping_n = ifelse(ans_q5_shopping == "No", NA, as.numeric(x_if_shopping)),
    ans_q5_dining = ifelse(dining_at_cafes_restaurants == "Y", "Yes", "No"),
    ans_q5_dining_n = ifelse(ans_q5_dining == "No", NA, as.numeric(x_if_dining)),
    ans_q5_visiting = ifelse(visiting_professional_services == "Y", "Yes", "No"),
    ans_q5_visiting_n = ifelse(
      ans_q5_visiting == "No",
      NA,
      as.numeric(x_if_professional_services)
    ),
    ans_q5_other = ifelse(other == "Y", "Yes", "No"),
    ans_q5_other_reason = ifelse(ans_q5_other == "No", NA, x_if_other),
    ans_q6_duration = as.numeric(
      q6_approximately_how_long_do_you_think_you_will_be_visiting_boundary_st_today_in_minute
    ),
    ans_q7_spending = as.numeric(
      q7_approximately_how_much_do_you_think_you_will_spend_while_visiting_boundary_st_today
    ),
    ans_q8_student = ifelse(q8_are_you_a_student == "Y", "Yes", "No"),
    ans_q8_student_at = ifelse(ans_q8_student == "No", NA, x_if_yes),
    ans_q8_employment_status = ifelse(
      q9_which_is_your_employment_status_and_profession == "Employed full-time",
      "Full Time",
      ifelse(
        q9_which_is_your_employment_status_and_profession == "Employed part-time",
        "Part Time",
        ifelse(
          q9_which_is_your_employment_status_and_profession == "Retired",
          "Retired",
          "Unemployed"
        )
      )
    ),
    ans_q8_profession = x_profession_university_and_anything_else,
    ans_q9_reside = str_to_title(q10_where_do_you_reside),
    ans_q9_reside = ifelse(ans_q9_reside == "City Cbd", "Brisbane CBD", ans_q9_reside),
    ans_q9_reside = ifelse(
      ans_q9_reside == "Highgate Hill",
      "Highgate Hill Park",
      ans_q9_reside
    ),
    ans_q9_reside = ifelse(
      ans_q9_reside == "Sunshine Coast (Mountain Creek)",
      "Sunshine Coast",
      ans_q9_reside
    ),
    ans_q9_reside = ifelse(ans_q9_reside == "Sunny Bank", "Sunnybank", ans_q9_reside),
    ans_q9_reside = ifelse(ans_q9_reside == "Towong", "Toowong", ans_q9_reside),
    ans_q9_reside = ifelse(
      ans_q9_reside == "West End (West Village)",
      "West Village",
      ans_q9_reside
    ),
    ans_q9_reside = ifelse(
      ans_q9_reside == "West End/ Highgate Hill",
      "Highgate Hill Park",
      ans_q9_reside
    ),
    ans_q9_reside = ifelse(
      ans_q9_reside == "West End/ South Brisbane",
      "South Brisbane",
      ans_q9_reside
    ),
    ans_q9_reside = ifelse(
      ans_q9_reside == "West Village, Boundary St",
      "West Village",
      ans_q9_reside
    ),
    ans_q9_reside = paste0(ans_q9_reside, ", Queensland, Australia"),
    ans_q9_reside = ifelse(
      ans_q9_reside == "NA, Queensland, Australia" |
        ans_q9_reside == "Canberra (Dickson), Queensland, Australia",
      NA,
      ans_q9_reside
    ),
    ans_q9_reside = ifelse(
      ans_q9_reside == "Red Hill, Queensland, Australia",
      "Red Hill, Queensland, 4059, Australia",
      ans_q9_reside
    ),
    ans_q10_dependents = q11_if_any_how_many_dependents_reside_within_your_household,
    ans_q11_household_cars = as.numeric(q12_how_many_cars_does_your_household_own),
    ans_q12_perceived_drove_percent = as.numeric(
      q13_what_percentage_of_current_boundary_st_visitors_do_you_estimate_arrived_by_car
    ) * 100,
    ans_q13_closing_comments = q14_in_closing_do_you_have_any_further_comments_about_access_to_boundary_st,
    ans_q14_date = dmy(date),
    q16_what_is_the_current_time = ifelse(q16_what_is_the_current_time == "23:50:00", "11:50:00", q16_what_is_the_current_time),
    q16_what_is_the_current_time = gsub(":", " ",  q16_what_is_the_current_time),
    ans_q15_time = hms(q16_what_is_the_current_time),
    ans_q16_sex = ifelse(
      q17_which_was_their_sex == "Female",
      "Female",
      ifelse(q17_which_was_their_sex == "Male", "Male", NA)
    ),
    ans_q17_age = ifelse(
      q18_which_was_their_approximate_age == "<25",
      "<25",
      ifelse(
        q18_which_was_their_approximate_age == "25-45",
        "25-45",
        ifelse(
          q18_which_was_their_approximate_age == "45-65",
          "45-65",
          ifelse(q18_which_was_their_approximate_age == ">65", ">65", NA)
        )
      )
    ),
    ans_collector = if_on_evelope_who_conducted_the_survey_we_may_wish_to_look_at_street_side_and_or_bias
  ) %>%
  select(starts_with("ans_")) %>%
  mutate(
    q1_primary_mode = ifelse(
      ans_q1_bicycle == "Yes",
      "Cyclist",
      ifelse(
        ans_q1_taxi == "Yes" | ans_q1_ride_hail == "Yes",
        "Ride Hailer",
        ifelse(
          ans_q1_public_transport == "Yes",
          "Transit Rider",
          ifelse(ans_q1_car == "Yes", "Car Driver", "Walker/Jogger")
        )
      )
    ),
    day = lubridate::wday(ans_q14_date, label = TRUE, abbr = FALSE),
    mode = ifelse(
      q1_primary_mode == "Car Driver" |
        q1_primary_mode == "Ride Hailer",
      "Private",
      ifelse(q1_primary_mode == "Transit Rider", "Public", "Active")
    ),
    mode = factor(mode, levels = c("Active", "Public", "Private"))
  ) %>%
  mutate_if(is.character, list( ~ na_if(., ""))) %>%
  mutate_if(is.character, factor) %>%
  mutate(ans_q9_reside = as.character(ans_q9_reside))

df1 <- intercept_survey %>%
  select(ans_q9_reside) %>%
  unique()

df1 <- geocode_OSM(df1$ans_q9_reside, as.sf = TRUE) %>%
  st_transform(crs = st_crs(inner_cities)) %>%
  drop_na(query)

intercept_route <- st_sf(st_sfc(crs = st_crs(inner_cities)))

for (i in unique(df1$query)) {
  print(i)
  sf1 <- df1 %>%
    filter(query == i)
  sf2 <- osrmRoute(
    src = sf1,
    dst = boundary_st,
    overview = "full",
    returnclass = "sf"
  ) %>%
    mutate(ans_q9_reside = i)
  intercept_route <- sf2 %>%
    rbind(intercept_route)
  Sys.sleep(0.5)
}

st_write(
  intercept_route,
  "../outputs/intercept_route.shp",
  driver = "ESRI Shapefile",
  delete_layer = TRUE
)

df1 <- intercept_route %>%
  as_tibble() %>%
  transmute(ans_q9_reside,
            distance = round(distance, 0))

intercept_survey <- intercept_survey %>%
  left_join(df1, by = "ans_q9_reside")

saveRDS(intercept_survey, file = "../outputs/intercept_survey.RData")

df1 <- intercept_survey
colnames(df1) <- gsub('ans_', '', colnames(df1))

for_Iraphne <- df1 %>%
  select(-collector) %>%
  rename("q9_distance" = "distance",
         "q1_primary_mode" = "q1_primary_mode")

write.csv(for_Iraphne, file = "../for_Iraphne.csv")
