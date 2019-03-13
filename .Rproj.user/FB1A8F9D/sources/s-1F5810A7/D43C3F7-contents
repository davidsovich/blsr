#' Series IDs for Job Openings and Labor Turnover Survey (JOLTS) data
#'
#' \code{jolts_seriesid} constructs series IDs for downloading data from the Job Openings and Labor
#' Turnover Survey (JOLTS) database.
#'
#' The Bureau of Labor Statistics (BLS) stores data in the form of series IDs. The structure of
#' series IDs varies by the underlying database (e.g., CES, JOLTS, etc.). Users must input
#' the correct series IDs to download their desired data. This function helps overcome this problem
#' by automatically generating JOLTS series IDs for a given set of inputs. Note that only total
#' non-farm data series IDs can be extracted for different regions of the United States.
#'
#'
#' @param adjustment Character vector. Seasonal adjustment ("S") or not ("U") or both.
#' @param industries Character vector. See vignette. List of available
#' industries given in \code{jolts_codes_list} dataset.
#' @param data_types Character vector. Desired output. See series vignette. List of available
#' data_types given in \code{jolts_codes_list} dataset.
#' @param data_levels Character vector. Levels ("L") or rates ("R") or both.
#' @param regions Optional character vector. See vignette. Leave blank for total US. List of
#' regions given in \code{jolts_codes_list} dataset. Only total non-farm data_types available for
#' non-total U.S. series IDs.
#' @return Vector of JOLTS series IDs.
#' @examples
#' # National series for seasonally adjusted private sector hires and job openings
#' jolts_seriesid(adjustment = c("S"), industries = c("100000"), data_types = c("HI", "JO"),
#' data_levels = c("L"))
#'
#' # Region series for seasonally and non-seasonally adjusted total hires
#' jolts_seriesid(adjustment = c("S","U"), industries = "c("000000"), data_types = c("HI"),
#' data_levels = c("L"), regions = c("MW", "NE", "SO", "WE"))

jolts_seriesid = function(adjustment, industries, data_types, data_levels, regions) {
   if(missing(regions)) {
      regions = c("00")
   } else if(sum((regions != "00"))>0) {
      message("Warning! Only total nonfarm series ID is available for JOLTS regional data. Adjusting!")
      industries = "000000"
   }
   temp_str = apply(expand.grid(c("JT"), adjustment, industries, regions, data_types, data_levels),
                                1,
                                FUN = function(x) { paste(x, collapse = "", sep = "") })
   if(length(temp_str)>50){
      message("Warning! BLS API only allows 50 series per query with an API key. Please reduce!")
   }
   return(temp_str)
}


