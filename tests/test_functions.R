library(tidyverse)
library(blscrapeR)
library(lubridate)

# CES function tests ------------------------------------------------------------------------------

# Load CES metadata
load("./data/ces_national_codes_list.Rda")
load("./data/ces_state_codes_list.Rda")

# Parameter choices
indu_choice = (ces_national_codes_list$indu_codes %>%
                  filter(level == 2, private_sector_flag == 1))$industry_code
seasonal_adj = "S"
data_types = (ces_national_codes_list$data_types %>%
                 filter(data_type_code %in% c("01", "03", "11")))$data_type_code
state_choices = (ces_state_codes_list$state_codes %>%
                    filter(state_id %in% c("NV", "CA", "UT")))$state_code

# Test series extraction functions
national_seriesid = ces_seriesid(adjustment = seasonal_adj,
                                 industries = indu_choice,
                                 data_types = data_types)

state_series = ces_seriesid(adjustment = seasonal_adj,
                            industries = indu_choice[1:2],
                            data_types = data_types,
                            states = state_choices)

# Test okay series extraction functions
okay_ces_seriesid(adjustment = seasonal_adj,
                  industries = indu_choice,
                  data_types = data_types)

okay_ces_seriesid(adjustment = seasonal_adj,
                  industries = indu_choice[1:2],
                  data_types = data_types,
                  states = state_choices)

# Test data download functions for national data
temp_df = ces_download(bls_key = Sys.getenv("BLS_KEY"),
                       start_year = 2010,
                       end_year = 2015,
                       adjustment = seasonal_adj,
                       industries = indu_choice,
                       data_types = data_types,
                       clean = FALSE)
temp_df_clean = clean_ces_national(temp_df)

# Test data download functions for state data
temp_df = ces_download(bls_key = Sys.getenv("BLS_KEY"),
                       start_year = 2010,
                       end_year = 2015,
                       adjustment = seasonal_adj,
                       industries = indu_choice[1:2],
                       data_types = data_types,
                       states = state_choices,
                       clean = FALSE)
temp_df_clean = clean_ces_state(temp_df)


# JOLTS function tests -----------------------------------------------------------------------------

# Load the JOLTS metadata
load("./data/jolts_codes_list.Rda")

# Parameter Choices
seasonal_adj = "S"
indu_choice = (jolts_codes_list$indu_codes %>%
                  filter(level == 2, private_sector_flag == 1))$industry_code
data_types = c("HI")
data_levels = c("L")
region_series = c("MW", "NE")

#Test series extraction functions
national_seriesid = jolts_seriesid(adjustment = seasonal_adj,
                                   industries = indu_choice,
                                   data_types = data_types,
                                   data_levels = data_levels)

regional_seriesid = jolts_seriesid(adjustment = seasonal_adj,
                                   industries = indu_choice,
                                   data_types = data_types,
                                   data_levels = data_levels,
                                   regions = region_series)

# Download the data
bls_download(seriesid = national_seriesid, start_year = 2006, end_year = 2007, bls_key = Sys.getenv("BLS_KEY"))

bls_download(seriesid = regional_seriesid, start_year = 2006, end_year = 2007, bls_key = Sys.getenv("BLS_KEY"))
