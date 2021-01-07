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
jolts_download = function(bls_key, start_year, end_year, adjustment, industries, data_types,
                          data_levels, states = "00", areas = "00000", sizes = "00", clean = TRUE) {
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
#' has four choices for hiring data: non-farm hiring, private hiring, super sector or sector hiring.
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
#' ("private"), supersector hiring ("super"), or sector hiring ("sector"). Defaults to non-farm
#' hiring.
#' @examples
#' jolts_df = jolts_hires(Sys.getenv("BLS_KEY"), "nfp", 2010, 2015, "U")
#'
jolts_hires = function(bls_key, series = "nfp", start_year, end_year, adjustment) {
   if(series == "nfp") {
      industries = "000000"
   } else if (series == "private") {
      industries = "100000"
   } else if (series == "super") {
      industries = dplyr::filter(jolts_codes_list$indu_codes, level == 2)$industry_code
   } else if (series == "sector") {
      industries = dplyr::filter(jolts_codes_list$indu_codes, level == 5)$industry_code
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

#' Download separations data from the Job Opening and Labor Turnover survey (JOLTS).
#'
#' \code{jolts_seps} downloads pre-packaged separations data from the JOLTS database.
#'
#' This function downloads and cleans pre-packaged separations data from the JOLTS database. The
#' user has three choices for separations data: non-farm separations, private separations, or super
#' sector hiring. The data is formatted in terms of rates similar to the quoted JOLTS
#' series. Separations is equal to the sum of layoffs, quits, and other separations. The function
#' reflects the October 2020 update to the JOLTS data series.
#'
#' @export
#'
#' @param bls_key BLS API key for the user. See vignette.
#' @param start_year Numeric. Year to start data download.
#' @param end_year Numeric. Year to end data download.
#' @param adjustment Character vector. Seasonal adjustment ("S") or not ("U") or both.
#' @param series Character. Data series. Either non-farm separations ("nfp"), private separations
#' ("private"), supersector separations ("super"), or sector separations ("sector"). Defaults to
#' non-farm separations.
#' @examples
#' jolts_df = jolts_seps(Sys.getenv("BLS_KEY"), "nfp", 2010, 2015, "U")
#'
jolts_seps = function(bls_key, series = "nfp", start_year, end_year, adjustment) {
   if(series == "nfp") {
      industries = "000000"
   } else if (series == "private") {
      industries = "100000"
   } else if (series == "super") {
      industries = dplyr::filter(jolts_codes_list$indu_codes, level == 2)$industry_code
   } else if (series == "sector") {
      industries = dplyr::filter(jolts_codes_list$indu_codes, level == 5)$industry_code
   } else {
      stop("Error! Invalid series input.")
   }
   jolts_download(
      bls_key = bls_key,
      start_year = start_year,
      end_year = end_year,
      adjustment = adjustment,
      industries = industries,
      data_types = "TS",
      data_levels = "R"
   )
}

#' Download layoffs data from the Job Opening and Labor Turnover survey (JOLTS).
#'
#' \code{jolts_seps} downloads pre-packaged layoffs data from the JOLTS database.
#'
#' This function downloads and cleans pre-packaged layoffs data from the JOLTS database. The
#' user has three choices for layoffs data: non-farm layoffs, private layoffs, or super
#' sector hiring. The data is formatted in terms of rates similar to the quoted JOLTS
#' series. The function reflects the October 2020 update to the JOLTS data series.
#'
#' @export
#'
#' @param bls_key BLS API key for the user. See vignette.
#' @param start_year Numeric. Year to start data download.
#' @param end_year Numeric. Year to end data download.
#' @param adjustment Character vector. Seasonal adjustment ("S") or not ("U") or both.
#' @param series Character. Data series. Either non-farm layoffs ("nfp"), private layoffs
#' ("private"), supersector layoffs ("super"), or sector layoffs ("sector"). Defaults to non-farm
#' layoffs.
#' @examples
#' jolts_df = jolts_layoffs(Sys.getenv("BLS_KEY"), "nfp", 2010, 2015, "U")
#'
jolts_layoffs = function(bls_key, series = "nfp", start_year, end_year, adjustment) {
   if(series == "nfp") {
      industries = "000000"
   } else if (series == "private") {
      industries = "100000"
   } else if (series == "super") {
      industries = dplyr::filter(jolts_codes_list$indu_codes, level == 2)$industry_code
   } else if (series == "sector") {
      industries = dplyr::filter(jolts_codes_list$indu_codes, level == 5)$industry_code
   } else {
      stop("Error! Invalid series input.")
   }
   jolts_download(
      bls_key = bls_key,
      start_year = start_year,
      end_year = end_year,
      adjustment = adjustment,
      industries = industries,
      data_types = "LD",
      data_levels = "R"
   )
}

#' Download quits data from the Job Opening and Labor Turnover survey (JOLTS).
#'
#' \code{jolts_seps} downloads pre-packaged quits data from the JOLTS database.
#'
#' This function downloads and cleans pre-packaged quits data from the JOLTS database. The
#' user has three choices for quits data: non-farm quits, private quits, or super
#' sector hiring. The data is formatted in terms of rates similar to the quoted JOLTS
#' series. The function reflects the October 2020 update to the JOLTS data series.
#'
#' @export
#'
#' @param bls_key BLS API key for the user. See vignette.
#' @param start_year Numeric. Year to start data download.
#' @param end_year Numeric. Year to end data download.
#' @param adjustment Character vector. Seasonal adjustment ("S") or not ("U") or both.
#' @param series Character. Data series. Either non-farm quits ("nfp"), private quits
#' ("private"), supersector quits ("super"), or sector quits ("sector"). Defaults to non-farm quits.
#' @examples
#' jolts_df = jolts_quits(Sys.getenv("BLS_KEY"), "nfp", 2010, 2015, "U")
#'
jolts_quits = function(bls_key, series = "nfp", start_year, end_year, adjustment) {
   if(series == "nfp") {
      industries = "000000"
   } else if (series == "private") {
      industries = "100000"
   } else if (series == "super") {
      industries = dplyr::filter(jolts_codes_list$indu_codes, level == 2)$industry_code
   } else if (series == "sector") {
      industries = dplyr::filter(jolts_codes_list$indu_codes, level == 5)$industry_code
   } else {
      stop("Error! Invalid series input.")
   }
   jolts_download(
      bls_key = bls_key,
      start_year = start_year,
      end_year = end_year,
      adjustment = adjustment,
      industries = industries,
      data_types = "QU",
      data_levels = "R"
   )
}

#' Download other separations data from the Job Opening and Labor Turnover survey (JOLTS).
#'
#' \code{jolts_seps} downloads pre-packaged other data from the JOLTS database.
#'
#' This function downloads and cleans pre-packaged other separations data from the JOLTS database.
#' The user has three choices for other separations data: non-farm, private, or super
#' sector. The data is formatted in terms of rates similar to the quoted JOLTS
#' series. The function reflects the October 2020 update to the JOLTS data series.
#'
#' @export
#'
#' @param bls_key BLS API key for the user. See vignette.
#' @param start_year Numeric. Year to start data download.
#' @param end_year Numeric. Year to end data download.
#' @param adjustment Character vector. Seasonal adjustment ("S") or not ("U") or both.
#' @param series Character. Data series. Either non-farm other ("nfp"), private other
#' ("private"), supersector other ("super"). or sector other ("sector"). Defaults to non-farm other.
#' @examples
#' jolts_df = jolts_others(Sys.getenv("BLS_KEY"), "nfp", 2010, 2015, "U")
#'
jolts_others = function(bls_key, series = "nfp", start_year, end_year, adjustment) {
   if(series == "nfp") {
      industries = "000000"
   } else if (series == "private") {
      industries = "100000"
   } else if (series == "super") {
      industries = dplyr::filter(jolts_codes_list$indu_codes, level == 2)$industry_code
   } else if (series == "sector") {
      industries = dplyr::filter(jolts_codes_list$indu_codes, level == 5)$industry_code
   } else {
      stop("Error! Invalid series input.")
   }
   jolts_download(
      bls_key = bls_key,
      start_year = start_year,
      end_year = end_year,
      adjustment = adjustment,
      industries = industries,
      data_types = "OS",
      data_levels = "R"
   )
}

#' Download job openings data from the Job Opening and Labor Turnover survey (JOLTS).
#'
#' \code{jolts_seps} downloads pre-packaged openings data from the JOLTS database.
#'
#' This function downloads and cleans pre-packaged job openings data from the JOLTS database. The
#' user has three choices for openings data: non-farm openings, private openings, or super
#' sector hiring. The data is formatted in terms of levels similar to the quoted JOLTS
#' series. The function reflects the October 2020 update to the JOLTS data series.
#'
#' @export
#'
#' @param bls_key BLS API key for the user. See vignette.
#' @param start_year Numeric. Year to start data download.
#' @param end_year Numeric. Year to end data download.
#' @param adjustment Character vector. Seasonal adjustment ("S") or not ("U") or both.
#' @param series Character. Data series. Either non-farm openings ("nfp"), private openings
#' ("private"), supersector openings ("super"), or sector openings ("sector"). Defaults to non-farm
#' openings.
#' @examples
#' jolts_df = jolts_openings(Sys.getenv("BLS_KEY"), "nfp", 2010, 2015, "U")
#'
jolts_openings = function(bls_key, series = "nfp", start_year, end_year, adjustment) {
   if(series == "nfp") {
      industries = "000000"
   } else if (series == "private") {
      industries = "100000"
   } else if (series == "super") {
      industries = dplyr::filter(jolts_codes_list$indu_codes, level == 2)$industry_code
   } else if (series == "sector") {
      industries = dplyr::filter(jolts_codes_list$indu_codes, level == 5)$industry_code
   } else {
      stop("Error! Invalid series input.")
   }
   jolts_download(
      bls_key = bls_key,
      start_year = start_year,
      end_year = end_year,
      adjustment = adjustment,
      industries = industries,
      data_types = "JO",
      data_levels = "L"
   )
}












