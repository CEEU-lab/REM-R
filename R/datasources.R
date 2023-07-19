library(tidyverse)
library(sf)

source("R/properaty_data.R")
source("R/utils.R")
datasource <- read_config(path = "config/config.yaml")
ppp_data_local <- read_local_properaty(path = datasource["data_path"])
neighborhoods <- st_read(dsn = datasource["neighbors"])
json_char <- datasource["json_char"]