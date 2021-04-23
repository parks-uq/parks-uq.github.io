#data loader
library(sf)
library(tidyverse)
library(stringr)
library(rgdal)
library(janitor)

# unzip
for (file in list.files(path = "./../data", pattern = "*.zip")) {
  print(paste("./../data/", file, sep = ""))
  unzip(
    zipfile = paste("./../data/", file, sep = ""),
    exdir = "./../data",
    overwrite = TRUE
  )
}

for (file in list.files(path = "./../data", pattern = "*.shp")) {
  path <- paste("./../data/", file, sep = "")
  name <-
    paste0("raw_", gsub("-", "_", gsub(" ", "_", str_to_lower(
      gsub(".shp", "", file)
    ))))
  assign(name, readOGR(dsn = path))
  assign(name, st_as_sf(get(name)))
  assign(name, clean_names(get(name)))
}

for (file in list.files(path = "./../data", pattern = "*.csv")) {
  path <- paste("./../data/", file, sep = "")
  name <-
    paste0("raw_", gsub("-", "_", gsub(" ", "_", str_to_lower(
      gsub(".csv", "", file)
    ))))
  assign(name, read.csv(path))
  assign(name, clean_names(get(name)))
}