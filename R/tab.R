#' @title Stata-like Tabulation
#'
#' @description
#' This function transforms a data.frame into an aggregate table. The main idea
#' was to replicate the \code{tab} command from Stata.
#'
#' @param data the input data.frame
#' @param ... columns to aggregate with. This takes a tidyselect specification.
#' @param cross_tab columns to aggregate across groups from `...`. Defaults to `NULL`.
#' @param cross_return should output be sorted by frequency. Defaults to `all`. Ignored if cross_tab = `NULL`
#'   * `"freq"`: only frequencies
#'   * `"col"` only column percentages
#'   * `"row"` only row percentages
#'   * `"freq+col"` frequencies and column percentages
#'   * `"freq+row"` frequencies and row percentages
#'   * `"all"` returns all data
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
#'
#' # cross tabulation
#' mtcars %>% tab(am, cross_tab = cyl)
#' tab(mtcars, am, cross_tab = cyl)
tab <- function(data, ..., cross_tab = NULL, cross_return = "all") {
   # pull only needed columns
   data <- data %>%
      dplyr::select(..., {{cross_tab}}) %>%
      mutate_if(
         .predicate = is.character,
         ~coalesce(na_if(str_squish(.), ""), "(no data)")
      )

   # create summary frame
   tab_df <- data %>%
      dplyr::group_by_all() %>%
      dplyr::summarise(
         `Freq.` = n()
      ) %>%
      ungroup()

   # check if cross tabulation required
   cross_var <- deparse(substitute(cross_tab))
   vars      <- unique(data[[cross_var]])
   if (cross_var != "NULL") {
      # create frequency cross tabulation
      tab_df <- tab_df %>%
         pivot_wider(
            names_from  = {{cross_tab}},
            values_from = Freq.
         ) %>%
         adorn_totals(c("row", "col"))

      # get percentages across rows
      if (cross_return == "all" || str_detect(cross_return, "row")) {
         row_perc_df <- tab_df %>%
            mutate_at(
               .vars = vars(vars),
               ~. / Total
            ) %>%
            rename_at(
               .vars = vars(vars),
               ~stri_c("row_% ", .)
            ) %>%
            select(-Total) %>%
            mutate_at(
               .vars = vars(starts_with("row_%")),
               ~num(., notation = "dec", label = "Row %", digits = 2, scale = 100)
            )

         tab_df <- tab_df %>%
            left_join(row_perc_df, join_by(...))
      }

      # get percentages across columns
      if (cross_return == "all" || str_detect(cross_return, "col")) {
         col_perc_df <- tab_df %>%
            slice(-nrow(.)) %>%
            mutate_at(
               .vars = vars(vars),
               ~(. / sum(.))
            ) %>%
            rename_at(
               .vars = vars(vars),
               ~stri_c("col_% ", .)
            ) %>%
            select(-Total) %>%
            untabyl() %>%
            adorn_totals("row") %>%
            mutate_at(
               .vars = vars(starts_with("col_%")),
               ~num(., notation = "dec", label = "Column %", digits = 2, scale = 100)
            )

         tab_df <- tab_df %>%
            left_join(col_perc_df, join_by(...))
      }

      # combine into one and format
      tab_df <- tab_df %>%
         as_tibble() %>%
         mutate_if(
            .predicate = is.integer,
            ~num(., notation = "dec")
         )

      # return type
      if (cross_return == "freq+row") {
         tab_df <- tab_df %>%
            select(-starts_with("col_%"))
      } else if (cross_return == "freq+col") {
         tab_df <- tab_df %>%
            select(-starts_with("row_%"))
      } else if (cross_return == "freq") {
         tab_df <- tab_df %>%
            select(-contains("%"))
      } else if (cross_return == "col") {
         tab_df <- tab_df %>%
            select(..., starts_with("col_%"))
      } else if (cross_return == "row") {
         tab_df <- tab_df %>%
            select(..., starts_with("row_%"))
      }
   } else {
      tab_df <- tab_df %>%
         dplyr::mutate(
            `Percent` = (`Freq.` / sum(`Freq.`)),
         ) %>%
         # get max data for cumulative cols
         janitor::adorn_totals("row") %>%
         # format frame
         as_tibble() %>%
         mutate(
            `Freq.`   = num(`Freq.`, notation = "dec"),
            `Percent` = num(`Percent`, notation = "dec", label = "%", digits = 2, scale = 100),
         )
   }

   return(tab_df)
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
