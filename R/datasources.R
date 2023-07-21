library(tidyverse)
library(sf)

source("R/properaty_data.R")
source("R/utils.R")
datasource <- read_config(path = "config/config.yaml")
ppp_data_local <- read_local_properaty(path = datasource$data_path)

build_shp_zone <- function(comunes, ppp_geo) {
  neighborhoods <- st_read(dsn = datasource$neighbors)
  # Area of interest
  aoi <- neighborhoods %>%
    filter(COMUNA %in% comunes)
  aoi_wgs <- st_transform(aoi, 4326)
  # Building types
  offer_area <- st_intersection(ppp_geo, aoi_wgs)
  return(offer_area)
}

json_char <- "{\"type\":\"Polygon\",\"coordinates\":[[[-58.50141949394735,-34.67256921692672],[-58.4683904133488,-34.645763294241725],[-58.45627975046284,-34.64594444443433],[-58.42853532276003,-34.664419684866886],[-58.46156440335859,-34.703711150046814],[-58.50141949394735,-34.67256921692672]]]}"
build_json_zone <- function(str_geom, ppp_geo) {
  # Area of interest
  aoi <- st_read(str_geom)
  aoi_wgs <- st_transform(aoi, 4326)
  # Building types
  offer_area <- st_intersection(ppp_geo, aoi_wgs)
  return(offer_area)
}