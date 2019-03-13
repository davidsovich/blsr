library(tidyverse)

# CES metadata for national-level data --------------------------------------------------------------

ces_national_codes_list = list(prefix = "CE",
                               seasonal_adj = readr::read_csv(file="./data-raw/seasonal_adjustment_definitions.csv"),
                               indu_codes = readr::read_csv(file="./data-raw/ces_industry_definitions.csv"),
                               data_types = readr::read_csv(file="./data-raw/ces_data_type_definitions.csv"),
                               series_id_map = readr::read_csv(file="./data-raw/ces_series_id_map.csv"))

usethis::use_data(ces_national_codes_list, overwrite = TRUE)

# CES metadata for state-level data ----------------------------------------------------------------

ces_state_codes_list = list(prefix = "SM",
                            seasonal_adj = readr::read_csv(file="./data-raw/seasonal_adjustment_definitions.csv"),
                            state_codes = readr::read_csv(file="./data-raw/ces_state_codes.csv"),
                            indu_codes = readr::read_csv(file="./data-raw/ces_industry_definitions.csv"),
                            data_types = readr::read_csv(file="./data-raw/ces_data_type_definitions_states.csv"),
                            series_id_map = readr::read_csv(file="./data-raw/ces_series_id_map_states.csv"))

usethis::use_data(ces_state_codes_list, overwrite = TRUE)
