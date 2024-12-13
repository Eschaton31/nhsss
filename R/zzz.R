.onAttach <- function(libname, pkgname) {
   options(
      pillar.print_min = 1e7,
      pillar.print_max = 1e7
   )
}

.onLoad <- function(libname, pkgname) {
   if (Sys.getenv("DB_USER") != "" && pacman::p_loaded(pool)) {

      if (!("oh-live" %in% ls(envir = .GlobalEnv))) {
         .GlobalEnv$`oh-live` <- pool::dbPool(
            RMariaDB::MariaDB(),
            user     = Sys.getenv("DB_USER"),
            password = Sys.getenv("DB_PASS"),
            host     = Sys.getenv("DB_HOST"),
            port     = Sys.getenv("DB_PORT"),
            timeout  = -1
         )
      }

      if (!("oh-lw" %in% ls(envir = .GlobalEnv))) {
         .GlobalEnv$`oh-lw` <- pool::dbPool(
            RMariaDB::MariaDB(),
            user     = Sys.getenv("LW_USER"),
            password = Sys.getenv("LW_PASS"),
            host     = Sys.getenv("LW_HOST"),
            port     = Sys.getenv("LW_PORT"),
            timeout  = -1
         )
      }

      reg.finalizer(
         e      = .GlobalEnv,
         f      = function() {
            pool::poolClose(.GlobalEnv$`oh-live`)
            pool::poolClose(.GlobalEnv$`oh-lw`)
         },
         onexit = TRUE
      )

      invisible(TRUE)
   }

   invisible(TRUE)
}

.onUnload <- function(libpath) {
   pool::poolClose(.GlobalEnv$`oh-live`)
   pool::poolClose(.GlobalEnv$`oh-lw`)
}