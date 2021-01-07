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

#' Download national employment data from Current Employment Statistics (CES) survey
#'
#' \code{ces_emp} downloads pre-packaged national employment data from the CES database.
#'
#' This function downloads and cleans pre-packaged employment data from the CES database. The user
#' has four choices of employment data: non-farm payrolls, private payrolls, super sector payrolls,
#' or sector payrolls. The employment data is at the national level.
#'
#' @export
#'
#' @param bls_key BLS API key for the user. See vignette.
#' @param start_year Numeric. Year to start data download.
#' @param end_year Numeric. Year to end data download.
#' @param adjustment Character vector. Seasonal adjustment ("S") or not ("U") or both. Defaults to
#' seasonally adjusted ("S").
#' @param series Character. Data series. Either non-farm payrolls ("nfp"), private payrolls
#' ("private"), supersector payrolls ("super"), or sector payrolls ("sector"). Defaults to non-farm
#' payrolls ("nfp").
#' @examples
#' ces_df = ces_emp(Sys.getenv("BLS_KEY"), "nfp", 2010, 2015, "U")
#'
ces_emp = function(bls_key, series = "nfp", start_year, end_year, adjustment = "S") {
   if(series == "nfp") {
      industries = "00000000"
   } else if (series == "private") {
      industries = "05000000"
   } else if (series == "super") {
      industries = dplyr::filter(ces_national_codes_list$indu_codes, level == 2)$industry_code
   } else if (series == "sector") {
      industries = dplyr::filter(ces_national_codes_list$indu_codes, level == 5)$industry_code
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



