library(tidyverse)

# DATASOURCE FUNCTIONS
# TODO: Starts the "query_properaty_data" here to load remote dataset


read_local_properaty <- function(path){
  stopifnot("Input must be a string" = is.character(path))
  df <- read_csv(path)
}

select_building_types <- function(input_data, currency, btypes, operation){
  stopifnot("Currency must be specified as USD or ARS" = is.character(currency))
  stopifnot("Building types must be specified as a char vector" = is.vector(btypes))
  stopifnot("Operation must be specified as a char vector with operation types" = is.vector(operation))
  
  pt_data <- input_data %>% 
    filter(l1=="Argentina", #TODO:Test other cities
           l2=="Capital Federal", 
           currency==currency,
           property_type %in% btypes,
           operation_type %in% operation,
           between(surface_covered,11,500), #TODO:Test non residential btypes 
           between(price, 1000, 2e6)) %>% 
    dplyr::select(id,l3,surface_covered,price,lat,lon,
                  rooms,bathrooms,bedrooms,property_type) %>% 
    mutate(pm2=price/surface_covered) %>% 
    rename(precio=price,barrio=l3,sup=surface_covered,ambientes=rooms,
           baths=bathrooms,cuartos=bedrooms,tipo=property_type) %>% na.omit()
}
