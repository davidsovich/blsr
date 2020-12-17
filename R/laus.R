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

#' Download data from the Local Area Unemployment Statistics (LAUS) database
#'
#' \code{laus_download} retrieves and downloads data from the Local Area Unemployment
#' Statistics (LAUS) database.
#'
#' This function constructs BLS series IDs for a chosen set of parameters and then downloads
#' the data from the LAUS database. This function also cleans the data and appends on identifiers
#' for easy panel formatting.
#'
#' @export
#'
#' @param bls_key BLS API key for the user. See vignette.
#' @param start_year Year to start data download.
#' @param end_year Year to end data download.
#' @param adjustment Character vector. Seasonal adjustment ("S") or not ("U") or both.
#' @param states Character vector. See vignette and \code{laus_codes_list} object.
#' @param data_types Character vector. See vignette and \code{laus_codes_list} object.
#' @param clean Optional logical. Whether to clean the data into usable format or not.
#' Defaults to TRUE.
#' @return Cleaned data frame of LAUS data.
#' @examples
#' laus_download(bls_key = "inputs", start_year = 2010, end_year = 2015,
#' adjustment = "U", states = c("ST0100000000000", "ST0200000000000"), data_types = c("03"))
laus_download = function(bls_key, start_year, end_year, adjustment, states, data_types, clean = TRUE) {
   if(okay_laus_seriesid(adjustment = adjustment, states = states, data_types = data_types)) {
      seriesid = laus_seriesid(adjustment = adjustment, states = states, data_types = data_types)
   } else {
      stop("Error! Invalid inputs. Run function okay_laus_seriesid.")
   }
   bls_df = bls_download(seriesid, start_year, end_year, bls_key)
   if(clean == TRUE) {
      clean_laus(bls_df)
   } else {
      bls_df
   }
}

#' Download state unemployment rates from the Local Area Unemployment Statistics (LAUS) database
#'
#' \code{laus_download} downloads state unemployment rates from the Local Area Unemployment
#' Statistics (LAUS) database.
#'
#' This function downloads and cleans pre-packaged state unemployment rate data from the LAUS
#' database. The default setting is to download unemployment rates for all states in the U.S.
#' excluding D.C. The user can shorten or lengthen the list of states but is constrained by the
#' BLS 50 series limit.
#'
#' @export
#'
#' @param bls_key BLS API key for the user. See vignette.
#' #' @param states Character vector. All states ("all") or list of state codes. See vignette and
#' \code{laus_codes_list} object.
#' @param start_year Year to start data download.
#' @param end_year Year to end data download.
#' @param adjustment Character vector. Seasonal adjustment ("S") or not ("U") or both.
#' @return Cleaned data frame of LAUS data.
#' @examples
#' laus_urate(Sys.getenv("BLS_KEY"), "nfp", 2010, 2015, "U")
laus_urate = function(bls_key, states = "all", start_year, end_year, adjustment) {
   if(states = "all") {
      states = laus_codes_list$state_codes %>%
         dplyr::filter(
            in_us_flag == 1,
            state_id != "DC"
         )
      states = states$state_code
   }
   laus_download(
      bls_key = bls_key,
      start_year = start_year,
      end_year = end_year,
      adjustment = adjustment,
      states = states,
      data_types = c("03")
   )
}








