# ---- General utilities --------------------------------------------------------------------------

#' Checks series ID inputs.
#'
#' \code{okay_series_input} returns whether a series ID input is valid.
#'
#' @param inputs Character vector. List of inputs to series ID string.
#' @param valid_inputs Character vector. List of valid inputs to series ID string.
#' @return TRUE if the series inputs are valid. FALSE otherwise.
okay_series_input = function(inputs, valid_inputs) {
   if(sum(inputs %in% valid_inputs) != length(inputs)){
      FALSE
   } else {
      TRUE
   }
}

#' Displays metadata from the Bureau of Labor Statistics
#'
#' \code{get_bls_metadata} displays metatadata for a given Bureau of Labor Statistics (BLS)
#' database.
#'
#' This function displays metadata for a BLS database.
#'
#' @export
#'
#' @param database Character. Database name. Either "JOLTS", "CES", or "CESstate".
#' @return Data frame containing BLS metadata.
#' @examples
#' get_bls_metadata("JOLTS")
get_bls_metadata = function(database) {
   if(database == "CES") {
      return(ces_national_codes_list)
   } else if (database == "CESstate") {
      return(ces_state_codes_list)
   } else if (database == "JOLTS") {
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
      stop("Error! BLS API cannot return more than 50 series IDs in one request!")
   }
   if((end_year - start_year) > 19) {
      stop("Error! BLS API cannot return more than 20 years of data in one request!")
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

# ---- Naitonal CES utilities ---------------------------------------------------------------------

#' Series IDs for Current Employment Statistics (CES) data
#'
#' \code{ces_seriesid} constructs series IDs for downloading data from the Current Employment
#' Statistics (CES) database.
#'
#' The Bureau of Labor Statistics (BLS) stores data in the form of series IDs. The structure of
#' series IDs varies by the underlying database (e.g., CES, JOLTS, etc.). Users must input
#' the correct series IDs to download their desired data. This function generates CES series IDs
#' for a given set of inputs.
#'
#' @export
#'
#' @param adjustment Character vector. Seasonal adjustment ("S") or not ("U") or both.
#' @param industries Character vector. See vignette. List of available
#' industries given in \code{ces_national_codes_list} or \code{ces_state_codes_list} datasets.
#' @param data_types Character vector. Desired output. See series vignette. List of available
#' data_types given in \code{ces_national_codes_list} or \code{ces_state_codes_list} datasets.
#' @param states Optional character vector. See vignette. Leave blank for total US. List of
#' states given in \code{ces_national_codes_list} or \code{ces_state_codes_list} datasets.
#' @return Vector of CES series IDs.
#' @examples
#' # National series for seasonally adjusted private sector employment and earnings
#' ces_seriesid(adjustment = c("S"), industries = c("05000000"), data_types = c("01", "03"))
ces_seriesid = function(adjustment, industries, data_types, states) {
   if(missing(states)) {
      temp_str = apply(
         expand.grid(c("CE"), adjustment, industries, data_types),
         1,
         FUN = function(x) { paste(x, collapse = "", sep = "") }
      )
   } else {
      temp_str = apply(
         expand.grid(c("SM"), adjustment, states, industries, data_types),
         1,
         FUN = function(x) { paste(x, collapse = "", sep = "") }
      )
   }
   if(length(temp_str) > 50){
      message("Warning! BLS API only allows 50 series per query with an API key. Please reduce!")
   }
   if(
      (sum(data_types %in% c("03","11", "56", "57"))!=0) &
      (sum(industries %in% c("00000000","90000000"))!=0)
   ) {
      message(paste(
         "Warning! BLS only has wage and earnings statistics for the private sector.",
         "Government sectors and total nonfarm sectors will not return such statistics."
      ))
   }
   return(temp_str)
}

#' Check whether CES series ID is valid.
#'
#' \code{okay_ces_seriesid} checks whether the inputs yield a valid CES series ID string.
#'
#' General error checking for CES data downloads.
#'
#' @export
#'
#' @param adjustment Character vector. Seasonal adjustment ("S") or not ("U") or both.
#' @param industries Character vector. See vignette. List of available
#' industries given in \code{ces_national_codes_list} or \code{ces_state_codes_list} datasets.
#' @param data_types Character vector. Desired output. See series vignette. List of available
#' data_types given in \code{ces_national_codes_list} or \code{ces_state_codes_list} datasets.
#' @param states Optional character vector. See vignette. Leave blank for total US. List of
#' states given in \code{ces_national_codes_list} or \code{ces_state_codes_list} datasets.
#' @return TRUE if the series IDs are valid. FALSE otherwise.
#' @examples
#' okay_ces_seriesid("S", "0000000", "01")
okay_ces_seriesid = function(adjustment, industries, data_types, states) {
   if(missing(adjustment) | missing(industries) | missing(data_types)) {
      message("Error! Adjustment, industry, and data are required for series!")
      return(FALSE)
   }
   if(missing(states)){
      if(!(okay_series_input(adjustment, ces_national_codes_list$seasonal_adj$seasonal_code))){
         message("Error! Seasonal adjustment must be S or U.")
         return(FALSE)
      }
      if(!okay_series_input(industries, ces_national_codes_list$indu_codes$industry_code)){
         message("Error! Invalid industry code.")
         return(FALSE)
      }
      if(!okay_series_input(data_types, ces_national_codes_list$data_types$data_type_code)){
         message("Error! Invalid data value.")
         return(FALSE)
      }
   } else {
      if(!(okay_series_input(adjustment, ces_state_codes_list$seasonal_adj$seasonal_code))){
         message("Error! Seasonal adjustment must be S or U.")
         return(FALSE)
      }
      if(!okay_series_input(industries, ces_state_codes_list$indu_codes$industry_code)){
         message("Error! Invalid industry code.")
         return(FALSE)
      }
      if(!okay_series_input(data_types, ces_state_codes_list$data_types$data_type_code)){
         message("Error! Invalid data value.")
         return(FALSE)
      }
      if(!okay_series_input(states, ces_state_codes_list$state_codes$state_code)){
         message("Error! Invalid state codes.")
         return(FALSE)
      }
      if((sum(data_types %in% c("03","11"))>=1) & (sum(adjustment !="U")>=1)){
         message("Error! Wage data can only be run with seasonal adjustment = U. Exiting function.")
         return(FALSE)
      }
   }
   return(TRUE)
}

#' Cleans the results of a query to the CES database.
#'
#' \code{clean_ces_national} cleans the results of a query to the CES national database and appends
#' additional information.
#'
#' General data cleaning.
#'
#' @param bls_df Data frame containing results of query to the CES national database.
#' @return Cleaned data frame from CES national database.
#' @examples
#' clean_ces_national(ces_df)
clean_ces_national = function(bls_df) {
   bls_df = bls_df %>%
      dplyr::mutate(
         archive = lubridate::year(date)*100 + lubridate::month(date),
         seasonal_code = substr(seriesID, 3, 3),
         seasonal_code = ifelse(
            seasonal_code == "S",
            "seasonally adjusted",
            "unadjusted"
         ),
         indu_code = substr(seriesID, 4, 11),
         data_type_code = substr(seriesID, 12, 13)
      ) %>%
      dplyr::left_join(
         y = ces_national_codes_list$indu_codes %>%
            dplyr::distinct(
               industry_code,
               .keep_all = TRUE
            ) %>%
            dplyr::rename(
               industry_name = description
            ) %>%
            dplyr::select(
               industry_code,
               industry_name,
               private_sector_flag,
               bls_sector_code
            ),
         by = c("indu_code" = "industry_code")
      ) %>%
      dplyr::left_join(
         y = ces_national_codes_list$data_types %>%
            dplyr::rename(
               variable_name = description
            ),
         by = c("data_type_code" = "data_type_code")
      ) %>%
      dplyr::left_join(
         y = naics_industry_mappings$ces_to_naics_map %>%
            dplyr::select(
               industry_code,
               naics_sector_code,
               naics_sector_name,
               naics_supersector_code,
               naics_supersector_name
            ),
            by = c("indu_code" = "industry_code")
      ) %>%
      dplyr::mutate(
         month = as.numeric(gsub("M", "", period))
      ) %>%
      dplyr::select(
         archive,
         month,
         period,
         seriesID,
         variable_name,
         value,
         seasonal_code,
         industry_name,
         private_sector_flag,
         bls_sector_code,
         naics_sector_code,
         naics_sector_name,
         naics_supersector_code,
         naics_supersector_name
      )
   bls_df
}

# ---- State CES utilities ------------------------------------------------------------------------

# Cleans the CES state database to a usable format
clean_ces_state = function(bls_df) {
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

# ---- National JOLTS utilities -------------------------------------------------------------------

#' Series IDs for Job Openings and Labor Turnover Survey (JOLTS) data
#'
#' \code{jolts_seriesid} constructs series IDs for downloading data from the Job Openings and Labor
#' Turnover Survey (JOLTS) database.
#'
#' The Bureau of Labor Statistics (BLS) stores data in the form of series IDs. The structure of
#' series IDs varies by the underlying database (e.g., CES, JOLTS, etc.). Users must input
#' the correct series IDs to download their desired data. This function helps overcome this problem
#' by automatically generating JOLTS series IDs for a given set of inputs. Note that only total
#' non-farm data series IDs can be extracted for different regions of the United States. Reflects
#' the October 2020 updates to JOLTS data series.
#'
#'
#' @param adjustment Character vector. Seasonal adjustment ("S") or not ("U") or both.
#' @param industries Character vector. See vignette. List of available
#' industries given in \code{jolts_codes_list} dataset.
#' @param data_types Character vector. Desired output. See series vignette. List of available
#' data_types given in \code{jolts_codes_list} dataset.
#' @param data_levels Character vector. Levels ("L") or rates ("R") or both.
#' @param states Option character vector. See vignette. Leave blank for total US. List of
#' states given in \code{jolts_codes_list} dataset.
#' @param areas Optional character vector. See vignette. Leave blank for total US. List of
#' areas in \code{jolts_codes_list} dataset.
#' #' @param sizes Optional character vector. See vignette. Leave blank for all. List
#' in \code{jolts_codes_list} dataset.
#' @return Vector of JOLTS series IDs.
#' @examples
#' # National series for seasonally adjusted private sector hires and job openings
#' jolts_seriesid(adjustment = c("S"), industries = c("100000"), data_types = c("HI", "JO"),
#' data_levels = c("L"))
jolts_seriesid = function(adjustment, industries, data_types, data_levels, states, areas, sizes) {
   if(missing(states)) {
      states = c("00")
   }
   if(missing(areas)) {
      areas = c("00000")
   }
   if(missing(sizes)) {
      sizes = c("00")
   }
   temp_str = apply(
      expand.grid(c("JT"), adjustment, industries, states, areas, sizes, data_types, data_levels),
      1,
      FUN = function(x) { paste(x, collapse = "", sep = "") }
   )
   if(length(temp_str)>50){
      message("Warning! BLS API only allows 50 series per query with an API key. Please reduce!")
   }
   return(temp_str)
}

#' Check whether JOLTS series ID is valid.
#'
#' \code{okay_jolts_seriesid} checks whether the inputs yield a valid JOLTS series ID string.
#'
#' General error checking for JOLTS data downloads. Reflects the October 2020 update to JOLTS
#' data series.
#'
#' @export
#'
#' @param adjustment Character vector. Seasonal adjustment ("S") or not ("U") or both.
#' @param industries Character vector. See vignette. List of available
#' industries given in \code{jolts_codes_list} dataset.
#' @param data_types Character vector. Desired output. See series vignette. List of available
#' data_types given in \code{jolts_codes_list} dataset.
#' @param data_levels Character vector. Levels ("L") or rates ("R") or both.
#' @param states Option character vector. See vignette. Leave blank for total US. List of
#' states given in \code{jolts_codes_list} dataset.
#' @param areas Optional character vector. See vignette. Leave blank for total US. List of
#' areas in \code{jolts_codes_list} dataset.
#' @param sizes Optional character vector. See vignette. Leave blank for all. List
#' in \code{jolts_codes_list} dataset.
#' @return TRUE if the series IDs are valid. FALSE otherwise.
#' @examples
#' NA
okay_jolts_seriesid = function(adjustment, industries, data_types, data_levels, states, areas, sizes){
   if(missing(adjustment) | missing(industries) | missing(data_types) | missing(data_levels)){
      message("Error! Adjustment, industry, and data are required for series!")
      return(FALSE)
   }
   if(!(okay_series_input(adjustment, jolts_codes_list$seasonal_adj$seasonal_code))){
      message("Error! Seasonal adjustment must be S or U.")
      return(FALSE)
   }
   if(!okay_series_input(industries, jolts_codes_list$indu_codes$industry_code)){
      message("Error! Invalid industry code.")
      return(FALSE)
   }
   if(!okay_series_input(data_types, jolts_codes_list$element_codes$dataelement_code)){
      message("Error! Invalid data value.")
      return(FALSE)
   }
   if(!okay_series_input(data_levels, jolts_codes_list$rate_level_codes$ratelevel_code)){
      message("Error! Level must be L or R.")
      return(FALSE)
   }
   if(!missing(states)) {
      if(!okay_series_input(states, jolts_codes_list$state_codes$state_code)){
         message("Error! Invalid region codes.")
         return(FALSE)
      }
   }
   if(!missing(areas)) {
      if(!okay_series_input(areas, jolts_codes_list$area_codes$area_code)){
         message("Error! Invalid region codes.")
         return(FALSE)
      }
   }
   if(!missing(sizes)) {
      if(!okay_series_input(sizes, jolts_codes_list$size_codes$size_code)){
         message("Error! Invalid region codes.")
         return(FALSE)
      }
   }
   return(TRUE)
}

#' Cleans the results of a query to the CES database.
#'
#' \code{clean_jolts} cleans the results of a query to the JOLTS national database and appends
#' additional information.
#'
#' General data cleaning.
#'
#' @param bls_df Data frame containing results of query to the JOLTS national database.
#' @return Cleaned data frame from JOLTS national database.
#' @examples
#' clean_jolts(jolts_df)
clean_jolts = function(bls_df) {
   bls_df = bls_df %>%
      dplyr::mutate(
         archive = lubridate::year(date)*100+lubridate::month(date),
         seasonal_code = substr(seriesID, 3, 3),
         seasonal_code = ifelse( seasonal_code == "S", "seasonally adjusted", "unadjusted"),
         indu_code = substr(seriesID, 4, 9),
         element_code = substr(seriesID, 19, 20),
         state_code = substr(seriesID, 10, 11),
         area_code = substr(seriesID, 12, 16),
         size_code = substr(seriesID, 17, 18),
         level_name = substr(seriesID, 21, 21),
         level_name = ifelse(level_name == "L", "Level", "Rate")
      ) %>%
      dplyr::left_join(
         y = jolts_codes_list$state_codes %>%
            dplyr::rename(
               state_name = description
            ),
         by = c("state_code"="state_code")
      ) %>%
      dplyr::left_join(
         y = jolts_codes_list$area_codes %>%
            dplyr::rename(
               area_name = description
            ),
         by = c("area_code"="area_code")
      ) %>%
      dplyr::left_join(
         y = jolts_codes_list$size_codes %>%
            dplyr::rename(
               size_name = description
            ),
         by = c("size_code"="size_code")
      ) %>%
      dplyr::left_join(
         y = jolts_codes_list$element_codes %>%
            dplyr::rename(
               variable_name = description
            ) %>%
            dplyr::select(
               dataelement_code,
               variable_name
            ),
         by = c("element_code"="dataelement_code")
      ) %>%
      dplyr::left_join(
         y = jolts_codes_list$indu_codes %>%
            dplyr::distinct(
               industry_code,
               .keep_all = TRUE
            ) %>%
            dplyr::rename(
               industry_name = description,
               industry_level = level
            ) %>%
            dplyr::select(
               industry_code,
               industry_name,
               private_sector_flag
            ),
         by = c("indu_code"="industry_code")
      ) %>%
      dplyr::left_join(
         y = naics_industry_mappings$jolts_to_naics_map %>%
            dplyr::select(
               industry_code,
               naics_sector_code,
               naics_sector_name,
               naics_supersector_code,
               naics_supersector_name
            ),
         by = c("indu_code" = "industry_code")
      ) %>%
      dplyr::mutate(
         month = as.numeric(gsub("M", "", period))
      ) %>%
      dplyr::select(
         archive,
         month,
         period,
         seriesID,
         state_name,
         state_code,
         area_name,
         area_code,
         size_name,
         size_code,
         variable_name,
         value,
         seasonal_code,
         level_name,
         industry_name,
         private_sector_flag,
         naics_sector_code,
         naics_sector_name,
         naics_supersector_code,
         naics_supersector_name
      )
   bls_df
}

# ---- LAUS utilities -----------------------------------------------------------------------------

#' Series IDs for Local Area Unemployment Statistics (LAUS) data
#'
#' \code{laus_seriesid} constructs series IDs for downloading data from the Local Area
#' Unemployment Statistics (LAUS) database.
#'
#' The Bureau of Labor Statistics (BLS) stores data in the form of series IDs. The structure of
#' series IDs varies by the underlying database (e.g., CES, JOLTS, LAUS, etc.). Users must input
#' the correct series IDs to download their desired data. This function generates LAUS series IDs
#' given a set of user inputs.
#'
#' @export
#'
#' @param adjustment Character vector. Seasonal adjustment ("S") or not ("U") or both.
#' @param states Character vector. See vignette and \code{laus_codes_list} object.
#' @param data_types Character vector. See vignette and \code{laus_codes_list} object.
#' @return Vector of LAUS series IDs.
#' @examples
#' laus_seriesid(adjustment = "U", states = c("ST0100000000000", "ST0200000000000"), data_types = c("03"))
laus_seriesid = function(adjustment, states, data_types) {
   temp_str = apply(expand.grid(c("LA"), adjustment, states, data_types),
                    1,
                    FUN = function(x) { paste(x, collapse = "", sep = "") })
   if(length(temp_str)>50) {
      message("Warning! BLS API only allows 50 series per query. Please break up your queries!")
   }
   return(temp_str)
}

#' Check whether LAUS series ID is valid.
#'
#' \code{okay_laus_seriesid} checks whether the given inputs yield a valid LAUS series ID string.
#'
#' General error checking for LAUS data downloads.
#'
#' @export
#'
#' @param adjustment Character vector. Seasonal adjustment ("S") or not ("U") or both.
#' @param states Character vector. See vignette and \code{laus_codes_list} object.
#' @param data_types Character vector. See vignette and \code{laus_codes_list} object.
#' @return TRUE if the series IDs are valid. FALSE otherwise.
#' @examples
#' okay_laus_seriesid(adjustment = "U", states = c("ST0100000000000"), data_types = c("03"))
okay_laus_seriesid = function(adjustment, states, data_types) {
   if(missing(adjustment) | missing(states) | missing(data_types)) {
      message("Error! Adjustment, states, and data_types are required inputs.")
      return(FALSE)
   }
   if(!okay_series_input(adjustment, laus_codes_list$seasonal_adj$seasonal_code)) {
      message("Error! Seasonal adjustment must be S or U.")
   }
   if(!okay_series_input(states, laus_codes_list$state_codes$state_code)){
      message("Error! Invalid data value for states argument.")
      return(FALSE)
   }
   if(!okay_series_input(data_types, laus_codes_list$data_type_codes$data_type_code)){
      message("Error! Invalid data value for data type argument.")
      return(FALSE)
   }
   return(TRUE)
}

#' Cleans the results of a query to the LAUS database.
#'
#' \code{clean_laus} cleans the results of a query to the LAUS state database and appends
#' additional information.
#'
#' General data cleaning.
#'
#' @param bls_df Data frame containing results of query to the LAUS national database.
#' @return Cleaned data frame from LAUS national database.
#' @examples
#' clean_laus(laus_df)
clean_laus = function(bls_df) {
   bls_df = bls_df %>%
      dplyr::mutate(
         archive = lubridate::year(date)*100 + lubridate::month(date),
         seasonal_code = substr(seriesID, 3, 3),
         seasonal_code = ifelse(
            seasonal_code == "S",
            "seasonally adjusted",
            "unadjusted"
         ),
         state_code = substr(seriesID, 4, 18),
         data_type_code = substr(seriesID, 19, 20)
      ) %>%
      dplyr::left_join(
         y = laus_codes_list$state_codes %>%
            dplyr::filter(
               in_us_flag == 1
            ),
         by = c("state_code" = "state_code")
      ) %>%
      dplyr::left_join(
         y = laus_codes_list$data_type_codes %>%
            dplyr::rename(variable_name = description),
         by = c("data_type_code" = "data_type_code")
      ) %>%
      dplyr::mutate(
         month = as.numeric(gsub("M", "", period))
      ) %>%
      dplyr::select(
         archive,
         month,
         period,
         seriesID,
         state_name,
         state_id,
         variable_name,
         value,
         seasonal_code
      )
   bls_df
}

