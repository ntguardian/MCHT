################################################################################
# StartupMessage.R
################################################################################
# 2018-09-20
# Curtis Miller
################################################################################
# Package startup message functions, for fancy loading.
################################################################################

#' Create Package Startup Message
#'
#' Makes package startup message.
#'
#' @import utils
#' @examples
#' MCHT:::MCHT_startup_message()
MCHT_startup_message <- function() {
  c(paste0(".------..------..------..------.\n|M.--. ||C.--. ||H.--. ||T.--.",
           " |\n| (\\/) || :/\\: || :/\\: || :/\\: |\n| :\\/: || :\\/: || (_",
           "_) || (__) |\n| '--'M|| '--'C|| '--'H|| '--'T|\n`------'`------'",
           "`------'`------' v. ",
           utils::packageVersion("MCHT")),
     "\nType citation(\"MCHT\") for citing this R package in publications")
}

#' Package Attach Hook Function
#'
#' Hook triggered when package attached
#'
#' @param lib a character string giving the library directory where the package
#'            defining the namespace was found
#' @param pkg a character string giving the name of the package 
#' @examples
#' MCHT:::.onAttach(.libPaths()[1], "MCHT")
.onAttach <- function(lib, pkg) {
  msg <- MCHT_startup_message()
  if (!interactive())
    msg[1] <- paste("Package 'MCHT' version", packageVersion("MCHT"))
  packageStartupMessage(msg)
  invisible()
}
