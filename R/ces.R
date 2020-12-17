#' Series IDs for Current Employment Statistics (CES) data
#'
#' \code{ces_seriesid} constructs series IDs for downloading data from the Current Employment
#' Statistics (CES) database.
#'
#' The Bureau of Labor Statistics (BLS) stores data in the form of series IDs. The structure of
#' series IDs varies by the underlying database (e.g., CES, JOLTS, etc.). Users must input
#' the correct series IDs to download their desired data. This function helps overcome this problem
#' by automatically generating CES series IDs for a given set of inputs.
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
      temp_str = apply(expand.grid(c("CE"), adjustment, industries, data_types),
                       1,
                       FUN = function(x) { paste(x, collapse = "", sep = "") })
   } else {
      temp_str = apply(expand.grid(c("SM"), adjustment, states, industries, data_types),
                       1,
                       FUN = function(x) { paste(x, collapse = "", sep = "") })
   }
   if(length(temp_str)>50){
      message("Warning! BLS API only allows 50 series per query with an API key. Please reduce!")
   }
   if((sum(data_types %in% c("03","11", "56", "57"))!=0) &
      (sum(industries %in% c("00000000","90000000"))!=0)) {
      message(paste("Warning! BLS only has wage and earnings statistics for the private sector.",
                    "Government sectors and total nonfarm sectors will not return such statistics."))
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
okay_ces_seriesid = function(adjustment, industries, data_types, states){
   if(missing(adjustment) | missing(industries) | missing(data_types)){
      message("Error! Adjustment, industry, and data are required for series!")
      return(FALSE)
   } else {
      #load("./data/ces_national_codes_list.Rda")
      #load("./data/ces_state_codes_list.Rda")
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

#' Download data from the Current Employment Statistics (CES) survey
#'
#' \code{ces_download} retrieves and downloads data from the Current Employment
#' Statistics (CES) database.
#'
#' This function constructs BLS series IDs for a chosen set of parameters and then downloads
#' the data from the CES database. This function also cleans the data and appends on identifiers
#' for easy panel formatting.
#'
#' @export
#'
#' @param bls_key BLS API key for the user. See vignette.
#' @param start_year Year to start data download.
#' @param end_year Year to end data download.
#' @param adjustment Character vector. Seasonal adjustment ("S") or not ("U") or both.
#' @param industries Character vector. See vignette. List of available
#' industries given in \code{ces_national_codes_list} or \code{ces_state_codes_list} datasets.
#' @param data_types Character vector. Desired output. See series vignette. List of available
#' data_types given in \code{ces_national_codes_list} or \code{ces_state_codes_list} datasets.
#' @param states Optional character vector. See vignette. Leave blank for total US. List of
#' states given in \code{ces_national_codes_list} or \code{ces_state_codes_list} datasets.
#' @param clean Optional logical. Whether to clean the data into usable format or not.
#' Defaults to TRUE.
#' @return Cleaned data frame of CES data.
#' @examples
#' ces_df = ces_download(bls_key = Sys.getenv("BLS_KEY"), start_year = 2010, end_year = 2015,
#' adjustment = "U", industries = "05000000", data_types = c("01", "03", "11"), states = "1900000")
#'
ces_download = function(bls_key, start_year, end_year, adjustment, industries, data_types, states,
                        clean = TRUE){
   if(missing(states)){
      if(okay_ces_seriesid(adjustment, industries, data_types)){
         seriesid = ces_seriesid(adjustment, industries, data_types)
      } else {
         stop("Error! Invalid inputs. See function okay_ces_seriesid.")
      }
   } else {
      if(okay_ces_seriesid(adjustment, industries, data_types, states)){
         seriesid = ces_seriesid(adjustment, industries, data_types, states)
      } else {
         stop("Error! Invalid inputs. See function okay_ces_seriesid.")
      }
   }
   bls_df = bls_download(seriesid, start_year, end_year, bls_key)
   if(clean == TRUE){
      if(missing(states)){
         clean_ces_national(bls_df)
      } else {
         clean_ces_state(bls_df)
      }
   } else {
      bls_df
   }
}

#' Download employment data from Current Employment Statistics (CES) survey
#'
#' \code{ces_emp} downloads pre-packaged employment data from the CES database.
#'
#' This function downloads and cleans pre-packaged employment data from the CES database. The user
#' has three choices of employment data: non-farm payrolls, private payrolls, or super sector
#' payrolls.
#'
#' @export
#'
#' @param bls_key BLS API key for the user. See vignette.
#' @param start_year Numeric. Year to start data download.
#' @param end_year Numeric. Year to end data download.
#' @param adjustment Character vector. Seasonal adjustment ("S") or not ("U") or both.
#' @param series Character. Data series. Either non-farm payrolls ("nfp"), private payrolls
#' ("private"), or supersector payrolls ("super"). Defaults to non-farm payrolls.
#' @examples
#' ces_df = ces_emp(Sys.getenv("BLS_KEY"), "nfp", 2010, 2015, "U")
#'
ces_emp = function(bls_key, series = "nfp", start_year, end_year, adjustment) {
   if(series == "nfp") {
      industries = "00000000"
   } else if (series == "private") {
      industries = "05000000"
   } else if (series == "super") {
      industries = dplyr::filter(ces_national_codes_list$indu_codes, level == 2)$industry_code
   } else {
      stop("Error! Invalid series input.")
   }
   ces_download(
      bls_key = bls_key,
      start_year = start_year,
      end_year = end_year,
      adjustment = adjustment,
      industries = industries,
      data_types = "01"
   )
}



