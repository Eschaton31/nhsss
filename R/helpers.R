#' @title Suppress specific warnings
#'
#' @description
#' This function suppresses specific warnings/messages in the console based on
#' the provided regular expression.
#'
#' @param .expr expression to be evaluated
#' @param .f regular expression string to find. if found, suppress
#' @return returns the evaluated expression without the warnings/messages
#' @export
suppress_warnings <- function(.expr, .f, ...) {
   eval.parent(substitute(
      withCallingHandlers(.expr, warning = function(w) {
         cm   <- conditionMessage(w)
         cond <-
            if (is.character(.f)) grepl(.f, cm) else rlang::as_function(.f)(cm, ...)
         if (cond) {
            invokeRestart("muffleWarning")
         }
      })
   ))
}