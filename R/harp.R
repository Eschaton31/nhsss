#' @title HARP Diaggregations
#'
#' @description
#' These functions help create the necessary disaggregations used for the various
#' HARP datasets.
#'
#' @param previous_outcome column for existing/old reference outcome variable
#' @param last_pill_date date of last know pill for the client
#' @param report_end_date end of the reporting period for reference of the new outcome
#' @param cutoff number in days/months to calculate the cutoff date against
#' @param units type of date unit to calculate the cutoff date against
#'   * `"days"`
#'   * `"months"`
#' @return returns a vector containing the updated hiv treatment outcomes
#' @export
hiv_tx_outcome <- function(previous_outcome, last_pill_date, report_end_date, cutoff, units = "days") {
   # cutoff date generated from art guidelines reference. needs the end of reporting
   # and units, based on cutoff param
   cutoff_date <- switch(
      units,
      days   = as.Date(report_end_date) %m-% days(cutoff),
      months = as.Date(report_end_date) %m-% months(cutoff) %>% ceiling_date(unit = "months")
   )

   # new outcome needs an old outcome variable as reference. this will contain
   # tagging for dead, stopped, transout, etc.
   new_outcome <- case_when(
      previous_outcome == "dead" ~ "dead",
      last_pill_date >= cutoff_date ~ "alive on arv",
      grepl("stopped", previous_outcome) & last_pill_date < cutoff_date ~ previous_outcome,
      grepl("transout", previous_outcome) & last_pill_date < cutoff_date ~ previous_outcome,
      last_pill_date < cutoff_date ~ "lost to follow up",
      previous_outcome == "ltfu" ~ "lost to follow up",
      TRUE ~ "(no data)"
   )

   return(new_outcome)
}
