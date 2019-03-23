library(tidyverse)

laus_codes_list = list( prefix = "LA",
                        seasonal_adj = readr::read_csv(file = "./data-raw/seasonal_adjustment_definitions.csv"),
                        state_codes = readr::read_csv(file = "./data-raw/laus_state_codes.csv"),
                        data_type_codes = readr::read_csv(file = "./data-raw/laus_data_type_definitions.csv"),
                        series_id_map = readr::read_csv(file = "./data-raw/laus_series_id_map.csv"))

usethis::use_data(laus_codes_list, overwrite = TRUE)
