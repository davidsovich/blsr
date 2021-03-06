library(tidyverse)

jolts_codes_list = list(
   prefix = "JT",
   seasonal_adj = readr::read_csv(file="./data-raw/seasonal_adjustment_definitions.csv"),
   indu_codes = readr::read_csv(file="./data-raw/jolts_industry_definitions.csv"),
   state_codes = readr::read_csv(file="./data-raw/jolts_state_definitions.csv"),
   area_codes = readr::read_csv(file ="./data-raw/jolts_area_definitions.csv"),
   size_codes = readr::read_csv(file ="./data-raw/jolts_size_definitions.csv"),
   rate_level_codes = readr::read_csv(file="./data-raw/jolts_rate_level_definitions.csv"),
   element_codes = readr::read_csv(file="./data-raw/jolts_data_element_definitions.csv"),
   series_id_map = readr::read_csv(file="./data-raw/jolts_series_id_map.csv")
)

usethis::use_data(jolts_codes_list, overwrite = TRUE)

