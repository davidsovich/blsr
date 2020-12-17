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
   temp_str = apply(expand.grid(c("JT"), adjustment, industries, states, areas, sizes, data_types, data_levels),
                                1,
                                FUN = function(x) { paste(x, collapse = "", sep = "") })
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
   } else {
      #load("./data/jolts_codes_list.Rda")
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

#' Download data from the Job Opening and Labor Turnover survey (JOLTS)
#'
#' \code{jolts_download} retrieves and downloads data from the Job Opening and Labor Turnover
#' survey (JOLTS) database.
#'
#' This function constructs BLS series IDs for a chosen set of parameters and then downloads
#' the data from the JOLTS database. This function also cleans the data and appends on identifiers
#' for easy panel formatting. Reflects the October 2020 update to JOLTS data series.
#'
#' @export
#'
#' @param bls_key BLS API key for the user. See vignette.
#' @param start_year Year to start data download.
#' @param end_year Year to end data download.
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
#' @param clean Optional logical. Whether to clean the data into usable format or not.
#' Defaults to TRUE.
#' @return Cleaned data frame of JOLTS data.
#' @examples
#' Add in from our other exercise.
#'
jolts_download = function(bls_key, start_year, end_year, adjustment, industries,
                          data_types, data_levels, states, areas, sizes, clean = TRUE) {
   if(missing(states)) {
      states = c("00")
   }
   if(missing(areas)) {
      areas = c("00000")
   }
   if(missing(sizes)) {
      sizes = c("00")
   }
   if(okay_jolts_seriesid(adjustment, industries, data_types, data_levels, states, areas, sizes)){
      seriesid = jolts_seriesid(adjustment, industries, data_types, data_levels, states, areas, sizes)
   } else {
      stop("Error! Invalid inputs. See function okay_ces_seriesid.")
   }
   bls_df = bls_download(seriesid, start_year, end_year, bls_key)
   if(clean == TRUE){
      clean_jolts(bls_df)
   } else {
      bls_df
   }
}

#' Download hiring data from the Job Opening and Labor Turnover survey (JOLTS).
#'
#' \code{jolts_hires} downloads pre-packaged hiring data from the JOLTS database.
#'
#' This function downloads and cleans pre-packaged hiring data from the JOLTS database. The user
#' has three choices of hiring data: non-farm hiring, private hiring, or super sector hiring.
#' The data is formatted in terms of hiring rates similar to the quoted JOLTS series. The
#' function reflects the October 2020 update to the JOLTS data series.
#'
#' @export
#'
#' @param bls_key BLS API key for the user. See vignette.
#' @param start_year Numeric. Year to start data download.
#' @param end_year Numeric. Year to end data download.
#' @param adjustment Character vector. Seasonal adjustment ("S") or not ("U") or both.
#' @param series Character. Data series. Either non-farm hiring ("nfp"), private hiring
#' ("private"), or supersector hiring ("super"). Defaults to non-farm hiring.
#' @examples
#' jolts_df = ces_hires(Sys.getenv("BLS_KEY"), "nfp", 2010, 2015, "U")
#'
jolts_hires = function(bls_key, series = "nfp", start_year, end_year, adjustment) {
   if(series == "nfp") {
      industries = "000000"
   } else if (series == "private") {
      industries = "100000"
   } else if (series == "super") {
      industries = dplyr::filter(jolts_codes_list$indu_codes, level == 2)$industry_code
   } else {
      stop("Error! Invalid series input.")
   }
   jolts_download(
      bls_key = bls_key,
      start_year = start_year,
      end_year = end_year,
      adjustment = adjustment,
      industries = industries,
      data_types = "HI",
      data_levels = "R"
   )
}








