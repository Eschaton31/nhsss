#' @title NHSSS Dataset Retreiver
#'
#' @description
#' This function retreives the datasets of the NHSSS Unit based on the naming
#' convention established and in-use.
#'
#' @param sys coded name of surveillance system
#'   * `"harp_dx"`: HARP Diagnosis
#'   * `"harp_tx"`: HARP Treatmentt
#'   * `"harp_dead"`: HARP Mortality
#'   * `"harp_vl"`: HARP VL Subsmission
#'   * `"prep"`: PrEP
#' @param type type of dataset to be returned, By default, returns registries.
#'   * `"reg"`: line-list registry
#'   * `"outcome"`: line-list outcomes/status
#' @param yr reporting year of data
#' @param mo reporting month of data
#' @param file_type file extension of the data to retreived
#' @return returns the full path of the latest version of the monthly dataset
#' @export
hs_data <- function(sys = NULL, type = "reg", yr = NULL, mo = NULL, file_type = "dta") {
   # ! internal functions
   # format string path
   format_path <- function(yr, mo) {
      yr      <- as.character(yr)
      mo      <- ifelse(nchar(mo) == 1, paste0("0", mo), mo)
      pattern <- glue("*{sys_prefix}[-a-z]*_{yr}\\-{mo}.*\\.{file_type}")
   }

   # find the latest file
   get_latest <- function(path, pattern) sort(list.files(path = path, pattern = pattern, full.names = TRUE), decreasing = TRUE)

   # summary preparations for yr mo data
   if (!is.numeric(yr) || !is.numeric(mo)) {
      yr <- as.numeric(yr)
      mo <- as.numeric(mo)
   }

   if (!StrIsNumeric(yr) || !StrIsNumeric(mo)) {
      log_error("The {red('yr')} and {red('mo')} parameters must be numeric.")
      stop()
   }

   # systems w/ alternate names
   sys_altname <- case_when(
      sys == "harp_tx" ~ "art",
      sys == "harp_dead" ~ "mort",
      sys == "dead" ~ "mort",
      TRUE ~ sys
   )
   sys_prefix  <- case_when(
      sys == "harp_dx" ~ "reg",
      sys == "harp_vl" ~ "vl_ml",
      type == "reg" ~ paste0("reg-", sys_altname),
      type == "outcome" ~ paste0("on", sys_altname),
      TRUE ~ sys_altname
   )

   # get full path
   path    <- Sys.getenv(toupper(sys))
   pattern <- format_path(yr, mo)

   file <- get_latest(path, pattern)[1]
   while (is.na(file)) {
      # for months feb-dec, check previous month
      # for jan, check previous year & month of dec
      if (mo > 1) {
         mo <- mo - 1
      } else {
         mo <- 12
         yr <- yr - 1
      }

      pattern <- format_path(yr, mo)
      file    <- get_latest(path, pattern)[1]
   }

   log_info(r"(GET: {red(basename(file[1]))})")
   return(file)
}