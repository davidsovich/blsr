library(tidyverse)

naics_industry_mappings = list(naics_supersector_map =
                                  readr::read_csv(file="./data-raw/naics_supersector_map.csv"),
                               jolts_to_naics_map =
                                  readr::read_csv(file="./data-raw/jolts_indu_to_naics_map.csv") %>%
                                  filter(!is.na(naics_supersector_code)),
                               ces_to_naics_map =
                                  readr::read_csv(file="./data-raw/ces_indu_to_naics_map.csv") %>%
                                  filter(!is.na(naics_supersector_code)))

usethis::use_data(naics_industry_mappings, overwrite = TRUE)




