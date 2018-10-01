################################################################################
# Utils.R
################################################################################
# 2018-08-27
# Curtis Miller
################################################################################
# Functions and other extras used throughout the package
################################################################################

################################################################################
# ROXYGEN2 TAGS
################################################################################

################################################################################
# OPERATORS
################################################################################

#' Concatenate (With Space)
#'
#' Concatenate and form strings (with space separation)
#'
#' @param x One object
#' @param y Another object
#' @return A string combining \code{x} and \code{y} with a space separating them
#' @examples
#' `%s%` <- CPAT:::`%s%`
#' "Hello" %s% "world"
`%s%` <- function(x, y) {paste(x, y)}

#' Concatenate (Without Space)
#'
#' Concatenate and form strings (no space separation)
#'
#' @inheritParams %s%
#' @return A string combining \code{x} and \code{y}
#' @examples
#' `%s0%` <- CPAT:::`%s0%`
#' "Hello" %s0% "world"
`%s0%` <- function(x, y) {paste0(x, y)}

################################################################################
# PARAMETER CHECKING
################################################################################

#' Check That Parameters Are In Functions
#'
#' Test that certain parameters are arguments for certain functions via
#' \pkg{testthat} functions.
#'
#' @param params Character vector with parameter names
#' @param func_list List of functions to check
#' @examples
#' check_params_in_functions(c("x"), list(mean))
check_params_in_functions <- function(params, func_list) {
  testthat::expect_is(params, "character")
  testthat::expect_is(func_list, "list")
  for (f in func_list) {
    testthat::expect_is(f, "function")
    testthat::expect_true(all(params %in% names(formals(f))))
  }
}
