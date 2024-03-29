library(tidyverse)

# DATASOURCE FUNCTIONS
# TODO: Starts the "query_properaty_data" here to load remote dataset

# TODO: .Rd file(DOCUMENTATION)

read_local_properaty <- function(path) {
  stopifnot("Input must be a string" = is.character(path))
  df <- read_csv(path)
}

select_building_types <- function(
  input_data, currency, colname, btypes, operation) {
  stopifnot("Currency must be USD or ARS" = is.character(currency))
  stopifnot("Building types must be char vector" = is.vector(btypes))
  stopifnot("Operation must be char vector" = is.vector(operation))
  col_quo <- enquo(colname)

  pt_data <- input_data %>%
    #TODO:Test how to use kwargs approach to pass just one datastructure with many args
    filter(l1 == "Argentina", 
           l2 == "Capital Federal",
           currency == currency,
           !!col_quo %in% btypes,
           operation_type %in% operation,
           between(surface_covered, 11, 500), 
           between(price, 1000, 2e6)) %>%
    dplyr::select(id, l3, surface_covered, price, lat, lon,
                  rooms, bathrooms, bedrooms, !!col_quo, agg_col) %>%
    mutate(
      pm2 = price / surface_covered) %>%
    rename(
      precio = price,
      barrio = l3, 
      sup = surface_covered, 
      ambientes = rooms,
      baths = bathrooms, 
      cuartos = bedrooms, 
      tipo = !!col_quo,
      tipo_agr = agg_col) %>%
    na.omit()
  return(pt_data)
}
