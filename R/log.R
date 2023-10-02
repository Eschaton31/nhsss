#' @title Info logger
#'
#' @import glue::glue
#' @import crayon
#'
#' @description
#' These functions help provided formatted messages for better logging.
#'
#' @param log_type type of logging to be done
#' @param msg message to be printed in the console
#' @return prints a formatted message in the console
#' @export
log <- function(msg, log_type = "info") {
   log_type <- tolower(log_type)
   switch(
      log_type,
      info    = log_info(msg),
      success = log_success(msg),
      warn    = log_warn(msg),
      error   = log_error(msg)
   )
}

#' @export
log_info <- function(msg = NULL) {
   log_type <- "INFO" %>% stri_pad_right(7, " ")
   log      <- crayon::bold(crayon::blue(log_type)) %+% crayon::magenta(glue::glue(' [{format(Sys.time(), "%Y-%m-%d %H:%M:%S")}]'))
   msg      <- glue::glue(msg, .envir = parent.frame(1))
   cat(log, msg, "\n")
}

#' @export
log_success <- function(msg = NULL) {
   log_type <- "SUCCESS" %>% stri_pad_right(7, " ")
   log      <- crayon::bold(crayon::green(log_type)) %+% crayon::magenta(glue::glue(' [{format(Sys.time(), "%Y-%m-%d %H:%M:%S")}]'))
   msg      <- glue::glue(msg, .envir = parent.frame(1))
   cat(log, msg, "\n")
}

#' @export
log_warn <- function(msg = NULL) {
   log_type <- "WARN" %>% stri_pad_right(7, " ")
   log      <- crayon::bold(crayon::yellow(log_type)) %+% crayon::magenta(glue::glue(' [{format(Sys.time(), "%Y-%m-%d %H:%M:%S")}]'))
   msg      <- glue::glue(msg, .envir = parent.frame(1))
   cat(log, msg, "\n")
}

#' @export
log_error <- function(msg = NULL) {
   log_type <- "ERROR" %>% stri_pad_right(7, " ")
   log      <- crayon::bold(crayon::red(log_type)) %+% crayon::magenta(glue::glue(' [{format(Sys.time(), "%Y-%m-%d %H:%M:%S")}]'))
   msg      <- glue::glue(msg, .envir = parent.frame(1))
   cat(log, msg, "\n")
}