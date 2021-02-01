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
#' \code{laus_urate} downloads state unemployment rates from the Local Area Unemployment
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
#' #' @param states Character vector. All states ("all"), Washington D.C. ("DC"), or list of state
#' codes. See vignette and \code{laus_codes_list} object.
#' @param start_year Year to start data download.
#' @param end_year Year to end data download.
#' @param adjustment Character vector. Seasonal adjustment ("S") or not ("U") or both.
#' @return Cleaned data frame of LAUS data.
#' @examples
#' laus_urate(Sys.getenv("BLS_KEY"), "all", 2010, 2015, "U")
laus_urate = function(bls_key, states = "all", start_year, end_year, adjustment) {
   if(states == "all") {
      states = laus_codes_list$state_codes %>%
         dplyr::filter(
            in_us_flag == 1,
            state_id != "DC"
         )
      states = states$state_code
   } else if (states == "DC") {
      states = laus_codes_list$state_codes %>%
         dplyr::filter(
            state_id == "DC"
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

#' Download state unemployment numbers from the Local Area Unemployment Statistics (LAUS) database
#'
#' \code{laus_unemp} downloads state unemployment numbers from the Local Area Unemployment
#' Statistics (LAUS) database.
#'
#' This function downloads and cleans pre-packaged state unemployment levels data from the LAUS
#' database. The default setting is to download unemployment levels for all states in the U.S.
#' excluding D.C. The user can shorten or lengthen the list of states but is constrained by the
#' BLS 50 series limit.
#'
#' @export
#'
#' @param bls_key BLS API key for the user. See vignette.
#' #' @param states Character vector. All states ("all"), Washington D.C. ("DC"), or list of state
#' codes. See vignette and \code{laus_codes_list} object.
#' @param start_year Year to start data download.
#' @param end_year Year to end data download.
#' @param adjustment Character vector. Seasonal adjustment ("S") or not ("U") or both.
#' @return Cleaned data frame of LAUS data.
#' @examples
#' laus_unemp(Sys.getenv("BLS_KEY"), "all", 2010, 2015, "U")
laus_unemp = function(bls_key, states = "all", start_year, end_year, adjustment) {
   if(states == "all") {
      states = laus_codes_list$state_codes %>%
         dplyr::filter(
            in_us_flag == 1,
            state_id != "DC"
         )
      states = states$state_code
   } else if (states == "DC") {
      states = laus_codes_list$state_codes %>%
         dplyr::filter(
            state_id == "DC"
         )
      states = states$state_code
   }
   laus_download(
      bls_key = bls_key,
      start_year = start_year,
      end_year = end_year,
      adjustment = adjustment,
      states = states,
      data_types = c("04")
   )
}

#' Download state empoyment numbers from the Local Area Unemployment Statistics (LAUS) database
#'
#' \code{laus_emp} downloads state empoyment numbers from the Local Area Unemployment
#' Statistics (LAUS) database.
#'
#' This function downloads and cleans pre-packaged state empoyment levels data from the LAUS
#' database. The default setting is to download empoyment levels for all states in the U.S.
#' excluding D.C. The user can shorten or lengthen the list of states but is constrained by the
#' BLS 50 series limit.
#'
#' @export
#'
#' @param bls_key BLS API key for the user. See vignette.
#' #' @param states Character vector. All states ("all"), Washington D.C. ("DC"), or list of state
#' codes. See vignette and \code{laus_codes_list} object.
#' @param start_year Year to start data download.
#' @param end_year Year to end data download.
#' @param adjustment Character vector. Seasonal adjustment ("S") or not ("U") or both.
#' @return Cleaned data frame of LAUS data.
#' @examples
#' laus_emp(Sys.getenv("BLS_KEY"), "all", 2010, 2015, "U")
laus_emp = function(bls_key, states = "all", start_year, end_year, adjustment) {
   if(states == "all") {
      states = laus_codes_list$state_codes %>%
         dplyr::filter(
            in_us_flag == 1,
            state_id != "DC"
         )
      states = states$state_code
   } else if (states == "DC") {
      states = laus_codes_list$state_codes %>%
         dplyr::filter(
            state_id == "DC"
         )
      states = states$state_code
   }
   laus_download(
      bls_key = bls_key,
      start_year = start_year,
      end_year = end_year,
      adjustment = adjustment,
      states = states,
      data_types = c("05")
   )
}

#' Download state labor force numbers from the Local Area Unemployment Statistics (LAUS) database
#'
#' \code{laus_labor_force} downloads state labor force numbers from the Local Area Unemployment
#' Statistics (LAUS) database.
#'
#' This function downloads and cleans pre-packaged state labor force levels data from the LAUS
#' database. The default setting is to download labor force levels for all states in the U.S.
#' excluding D.C. The user can shorten or lengthen the list of states but is constrained by the
#' BLS 50 series limit.
#'
#' @export
#'
#' @param bls_key BLS API key for the user. See vignette.
#' #' @param states Character vector. All states ("all"), Washington D.C. ("DC"), or list of state
#' codes. See vignette and \code{laus_codes_list} object.
#' @param start_year Year to start data download.
#' @param end_year Year to end data download.
#' @param adjustment Character vector. Seasonal adjustment ("S") or not ("U") or both.
#' @return Cleaned data frame of LAUS data.
#' @examples
#' laus_labor_force(Sys.getenv("BLS_KEY"), "all", 2010, 2015, "U")
laus_labor_force = function(bls_key, states = "all", start_year, end_year, adjustment) {
   if(states == "all") {
      states = laus_codes_list$state_codes %>%
         dplyr::filter(
            in_us_flag == 1,
            state_id != "DC"
         )
      states = states$state_code
   } else if (states == "DC") {
      states = laus_codes_list$state_codes %>%
         dplyr::filter(
            state_id == "DC"
         )
      states = states$state_code
   }
   laus_download(
      bls_key = bls_key,
      start_year = start_year,
      end_year = end_year,
      adjustment = adjustment,
      states = states,
      data_types = c("06")
   )
}






