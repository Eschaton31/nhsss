#' @title Stata-like Tabulation
#'
#' @description
#' This function transforms a data.frame into an aggregate table. The main idea
#' was to replicate the \code{tab} command from Stata.
#'
#' @param data the input data.frame
#' @param ... columns to aggregate with. This takes a tidyselect specification.
#' @param as_df should output be returned as a data.frame object. By default, the data is printed with formats.
#' @return Returns an aggregated summary data.frame.
#' @export
#' @examples
#'
#' # uni-variate tabulation
#' mtcars %>% tab(am)
#' tab(mtcars, am)
#'
#' # multi-variate tabulation
#' mtcars %>% tab(am, cyl)
#' tab(mtcars, am, cyl)
tab <- function(data, ..., as_df = FALSE) {
   # create summary frame
   tab_df <- data %>%
      dplyr::group_by(...) %>%
      dplyr::summarise(
         `Freq.` = n()
      ) %>%
      ungroup() %>%
      dplyr::mutate(
         `Cum. Freq.`   = cumsum(`Freq.`),
         `Percent`      = round((`Freq.` / sum(`Freq.`)), 10),
         `Cum. Percent` = round(cumsum(freq = `Freq.` / sum(`Freq.`)), 10),
      )

   # get max data for cumulative cols
   cum_freq <- max(tab_df$`Cum. Freq.`)
   cum_perc <- max(tab_df$`Cum. Percent`)
   tab_df   <- tab_df %>%
      adorn_totals()

   tab_df[nrow(tab_df), "Cum. Freq."]   <- cum_freq
   tab_df[nrow(tab_df), "Cum. Percent"] <- cum_perc

   # format frame
   if (!as_df) {
      tab_df <- tab_df %>%
         mutate(
            `Freq.`        = num(`Freq.`, notation = "dec"),
            `Cum. Freq.`   = num(`Cum. Freq.`, notation = "dec"),
            `Percent`      = num(`Percent`, notation = "dec", label = "%", digits = 2, scale = 100),
            `Cum. Percent` = num(`Cum. Percent`, label = "%", digits = 2, scale = 100),
         ) %>%
         as_tibble() %>%
         print(n = 1e7)
   } else {
      return(tab_df)
   }
}

#' @title Stata-like Statistics Tabulation
#'
#' @description
#' This function transforms a data.frame into a summary table indicating the `min`,
#' `median`, `max`, and `NAs` of the columns specified.
#'
#' @param data the input data.frame
#' @param ... columns to summarise. This takes a tidyselect specification.
#' @return Returns an summarised statistics data.frame.
#' @export
#' @examples
#'
#' # uni-variate tabulation
#' mtcars %>% tabstat(am)
#' tabstat(mtcars, am)
#'
#' # multi-variate tabulation
#' mtcars %>% tabstat(am, cyl)
#' tabstat(mtcars, am, cyl)
tabstat <- function(data, ...) {
   tab_data <- select(data, ...)
   tab_vars <- names(tab_data)

   tab_data <- tab_data %>%
      summarise_all(
         list(
            MIN    = ~suppress_warnings(min(., na.rm = TRUE), "returning [\\-]*Inf"),
            MEDIAN = ~suppress_warnings(median(., na.rm = TRUE), "returning [\\-]*Inf"),
            MAX    = ~suppress_warnings(max(., na.rm = TRUE), "returning [\\-]*Inf"),
            NAs    = ~sum(if_else(is.na(.), 1, 0, 0))
         )
      ) %>%
      mutate_all(~as.character(.)) %>%
      pivot_longer(
         cols      = names(select_at(., vars(ends_with(c("MEDIAN", "MAX", "MIN", "NAs", ignore.case = FALSE))))),
         names_to  = "VARIABLE",
         values_to = "VAL"
      ) %>%
      mutate(
         STAT     = case_when(
            stri_detect_regex(VARIABLE, "MEDIAN$") ~ "MEDIAN",
            stri_detect_regex(VARIABLE, "MIN$") ~ "MIN",
            stri_detect_regex(VARIABLE, "MAX$") ~ "MAX",
            stri_detect_regex(VARIABLE, "NAs$") ~ "NAs",
         ),
         VARIABLE = if (length(tab_vars) == 1) {
            tab_vars[1]
         } else {
            stri_replace_last_regex(VARIABLE, paste0("_", STAT, "$"), "")
         }
      ) %>%
      pivot_wider(
         id_cols     = VARIABLE,
         names_from  = STAT,
         values_from = VAL
      ) %>%
      select(
         VARIABLE,
         MIN,
         MEDIAN,
         MAX,
         NAs
      )

   return(tab_data)
}
