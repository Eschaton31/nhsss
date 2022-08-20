#' @title Info logger
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

log_info <- function(msg = NULL) {
   log_type <- "INFO" %>% stri_pad_right(7, " ")
   log      <- bold(blue(log_type)) %+% magenta(glue(' [{format(Sys.time(), "%Y-%m-%d %H:%M:%S")}]'))
   msg      <- glue(msg, .envir = parent.frame(1))
   cat(log, msg, "\n")
}

log_success <- function(msg = NULL) {
   log_type <- "SUCCESS" %>% stri_pad_right(7, " ")
   log      <- bold(green(log_type)) %+% magenta(glue(' [{format(Sys.time(), "%Y-%m-%d %H:%M:%S")}]'))
   msg      <- glue(msg, .envir = parent.frame(1))
   cat(log, msg, "\n")
}

log_warn <- function(msg = NULL) {
   log_type <- "WARN" %>% stri_pad_right(7, " ")
   log      <- bold(yellow(log_type)) %+% magenta(glue(' [{format(Sys.time(), "%Y-%m-%d %H:%M:%S")}]'))
   msg      <- glue(msg, .envir = parent.frame(1))
   cat(log, msg, "\n")
}

log_error <- function(msg = NULL) {
   log_type <- "ERROR" %>% stri_pad_right(7, " ")
   log      <- bold(red(log_type)) %+% magenta(glue(' [{format(Sys.time(), "%Y-%m-%d %H:%M:%S")}]'))
   msg      <- glue(msg, .envir = parent.frame(1))
   cat(log, msg, "\n")
}