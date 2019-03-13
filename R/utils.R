# Check if an input argument to the series ID string is valid.
okay_series_input = function(inputs, valid_inputs) {
   if(sum(inputs %in% valid_inputs) != length(inputs)){
      FALSE
   } else {
      TRUE
   }
}

# Load some metadata of choice
get_bls_metadata = function(database) {
   if(database == "CES_national") {
      load("./data/ces_national_codes_list.Rda")
      return(ces_national_codes_list)
   } else if (database == "CES_state") {
      load("./data/ces_state_codes_list.Rda")
      return(ces_state_codes_list)
   } else if (database == "JOLTS") {
      load("./data/jolts_codes_list.Rda")
      return(jolts_codes_list)
   } else {
      stop("Error! Must select a valid database.")
   }
}

#' Download data from the Bureau of Labor Statistics
#'
#' \code{bls_downloads} remotely downloads data for a given set of series IDs from the Bureau of
#' Labor Statistics (BLS).
#'
#' This function acts as a wrapper for the function \code{bls_api} from the package
#' \code{blscrapeR}. Please see documentation for \code{bls_api}.
#'
#' @export
#'
#' @param seriesid Character vector. Vector of series IDs for BLS data. Limit of 50 strings.
#' @param start_year Numeric.
#' @param end_year Numeric.
#' @param bls_key Character. BLS API key. Required parameter.
#' @examples
#' # National series for seasonally adjusted private sector employment and earnings
#' series_str = ces_seriesid(adjustment = c("S"), industries = c("05000000"),
#' data_types = c("01", "03"))
#'
#' bls_df = bls_download(seriesid = series_str, start_year = 2006, end_year = 2010,
#' bls_key = Sys.getenv("BLS_KEY"))
bls_download = function(seriesid, start_year, end_year, bls_key){
   if(length(seriesid)>50){
      stop("Error! Cannot process more than 50 series IDs at one time!")
   }
   if(missing(bls_key)){
      stop("Error! Please provide a valid BLS API key.")
   }
   temp_df = blscrapeR::bls_api(seriesid = seriesid,
                                startyear = start_year,
                                endyear = end_year,
                                registrationKey = bls_key)
   temp_df %>% blscrapeR::dateCast()
}

# Cleans the CES national database to a usable format
clean_ces_national = function(bls_df) {
      load("./data/ces_national_codes_list.Rda")
      load("./data/naics_industry_mappings.Rda")
      bls_df = bls_df %>%
         dplyr::mutate(archive = lubridate::year(date)*100 + lubridate::month(date),
                       seasonal_code = substr(seriesID, 3, 3),
                       seasonal_code = ifelse( seasonal_code == "S",
                                               "seasonally adjusted",
                                               "unadjusted"),
                       indu_code = substr(seriesID, 4, 11),
                       data_type_code = substr(seriesID, 12, 13))
      bls_df = bls_df %>%
         dplyr::left_join( y = ces_national_codes_list$indu_codes %>%
                               dplyr::rename(industry_name = description,
                                             industry_level = level) %>%
                               dplyr::select(industry_code,
                                             industry_name,
                                             industry_level,
                                             private_sector_flag,
                                             naics_sector_code),
                           by = c("indu_code" = "industry_code")) %>%
         dplyr::left_join( y = ces_national_codes_list$data_types %>%
                               dplyr::rename(variable_name = description),
                           by = c("data_type_code" = "data_type_code"))
      bls_df = bls_df %>%
         dplyr::left_join( y = naics_industry_mappings$ces_to_naics_map %>%
                               dplyr::select(industry_code,
                                             naics_supersector_code,
                                             naics_supersector_name),
                           by = c("indu_code"="industry_code"))
      bls_df = bls_df %>%
         dplyr::mutate(month = as.numeric(gsub("M", "", period))) %>%
         dplyr::select(archive, month, period, seriesID, variable_name, value, seasonal_code,
                       industry_name, industry_level, private_sector_flag,
                       naics_sector_code, naics_supersector_code, naics_supersector_name )
      bls_df
}

# Cleans the CES state database to a usable format
clean_ces_state = function(bls_df) {
   load("./data/ces_state_codes_list.Rda")
   load("./data/naics_industry_mappings")
   bls_df = bls_df %>%
      dplyr::mutate(archive = lubridate::year(date)*100 + lubridate::month(date),
                    seasonal_code = substr(seriesID, 3, 3),
                    seasonal_code = ifelse( seasonal_code == "S",
                                            "seasonally adjusted",
                                            "unadjusted"),
                    state_code = substr(seriesID, 4, 10),
                    indu_code = substr(seriesID, 11, 18),
                    data_type_code = substr(seriesID, 19, 20))
   bls_df = bls_df %>%
      dplyr::left_join( y = ces_state_codes_list$state_codes %>%
                            dplyr::filter(in_us_flag == 1) %>%
                            dplyr::rename(state_name = description) %>%
                            dplyr::select(state_code, state_name, state_id),
                        by = c("state_code" = "state_code")) %>%
      dplyr::left_join( y = ces_state_codes_list$indu_codes %>%
                           dplyr::rename(industry_name = description,
                                         industry_level = level) %>%
                           dplyr::select(industry_code,
                                         industry_name,
                                         industry_level,
                                         private_sector_flag,
                                         naics_sector_code),
                        by = c("indu_code" = "industry_code")) %>%
      dplyr::left_join( y = ces_state_codes_list$data_types %>%
                           dplyr::rename(variable_name = description),
                        by = c("data_type_code" = "data_type_code"))
   bls_df = bls_df %>%
      dplyr::left_join( y = naics_industry_mappings$ces_to_naics_map %>%
                           dplyr::select(industry_code,
                                         naics_supersector_code,
                                         naics_supersector_name),
                        by = c("indu_code"="industry_code"))
   bls_df = bls_df %>%
      dplyr::mutate(month = as.numeric(gsub("M", "", period))) %>%
      dplyr::select(archive, month, period, seriesID, state_name, state_id,
                    variable_name, value, seasonal_code,
                    industry_name, industry_level, private_sector_flag,
                    naics_sector_code, naics_supersector_code, naics_supersector_name )
      bls_df
}

# Cleans the JOLTS database to a usable format
clean_jolts = function(bls_df) {
   load("./data/jolts_codes_list.Rda")
   load("./data/naics_industry_mappings")
   bls_df = bls_df %>%
      dplyr::mutate( archive = lubridate::year(date)*100+lubridate::month(date),
                     seasonal_code = substr(seriesID, 3, 3),
                     seasonal_code = ifelse( seasonal_code == "S", "seasonally adjusted", "unadjusted"),
                     indu_code = substr(seriesID, 4, 9),
                     element_code = substr(seriesID, 12, 13),
                     region_code = substr(seriesID, 10, 11) ) %>%
      dplyr::left_join( y = jolts_codes$region_codes %>%
                           dplyr::rename(region_name = description),
                        by = c("region_code"="region_code") ) %>%
      dplyr::left_join( y = jolts_codes$element_codes %>%
                           dplyr::rename(variable_name = description) %>%
                           dplyr::select(dataelement_code, variable_name),
                        by = c("element_code"="dataelement_code") ) %>%
      dplyr::left_join( y = jolts_codes$indu_codes %>%
                           dplyr::rename(industry_name = description) %>%
                           dplyr::select(industry_code, industry_name),
                        by = c("indu_code"="industry_code") )
   jolts_df = jolts_df %>%
      dplyr::left_join( y = get_jolts_naics_map(bls_dir) %>%
                           dplyr::select(-one_of("level", "description")),
                        by = c("indu_code"="industry_code") )

   #Keep subset of variables
   jolts_df = jolts_df %>%
      dplyr::select( archive, year, period, seriesID, value, variable_name,
                     industry_name, region_name, seasonal_code, private_sector_flag,
                     naics_supersector_code, naics_supersector_name )
}
