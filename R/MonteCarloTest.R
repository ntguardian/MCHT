################################################################################
# MonteCarloTest.R
################################################################################
# 2018-09-29
# Curtis Miller
################################################################################
# Defines the MCHTest-class S3 object which defines and handles Monte Carlo
# tests
################################################################################

################################################################################
# OBJECT CREATION
################################################################################

#' Create an MCHTest Object
#'
#' This function creates an \code{MCHTest}-class object, an S3 object that
#' defines a Monte Carlo test.
#'
#' \code{MCHTest}-class objects are effectively functions that accept data and
#' maybe some parameters and return an \code{htest}-class object containing the
#' results of a Monte Carlo statistical test. These object will accept datasets
#' and perhaps some parameters and will return the results of a test.
#'
#' @param test_stat A function that computes the test statistic from input data;
#'                 \code{x} must be a parameter of this function representing
#'                 test data 
#' @param stat_gen A function that generates simulated values of the test
#'                 statistic; \code{n} must be a parameter of this function
#'                 (representing the sample size of the statistic), and this
#'                 function is expected to return one numeric output
#' @param N Integer representing the number of replications of \code{stat_gen}
#'          to generate
#' @param seed The random seed used to generate simulated statistic values; if
#'             \code{NULL}, the seed will be randomly chosen each time the
#'             resulting function is called (unless \code{memoise_sample} is
#'             \code{TRUE})
#' @param memoise_sample If \code{TRUE}, simulated statistic values are saved
#'                       and will be used repeatedly if the inputs to
#'                       \code{stat_gen} don't change (such as the sample size,
#'                       \code{n}); this could be in conflict with \code{seed}
#'                       if \code{seed} is \code{NULL}, so set to \code{FALSE}
#'                       to allow for regeneration of random samples for every
#'                       call to the resulting function
#' @param pval_func A function that computes \eqn{p}-values from the test
#'                  statistic computed by \code{test_stat} using the simulated
#'                  data generated via \code{stat_gen}; see \code{\link{pval}}
#'                  for an example of how this function should be specified
#' @param method A string labelling the test
#' @param test_params A character vector of the names of parameters with values
#'                    specified under the null hypothesis; both \code{test_stat}
#'                    and \code{stat_gen} need to be able to recognize the
#'                    contents of this vector as parameters (for example, if
#'                    this argument is \code{"mu"}, then \code{mu} needs to be
#'                    an argument of both \code{test_stat} and \code{stat_gen})
#' @param lock_alternative If \code{TRUE}, then the resulting function will
#'                         effectively ignore the \code{alternative} parameter,
#'                         while if \code{FALSE}, the resulting function will be
#'                         sensitive to values of \code{alternative}; this
#'                         argument exists to prevent shooting yourself in the
#'                         foot and accidentally computing \eqn{p}-values in
#'                         inappropriate ways
#' @param fixed_params A character vector of the names of parameters treated as
#'                     fixed values; this isn't needed but if these parameters
#'                     are being used then test output is more informative and
#'                     errors will be raised if \code{test_stat} and
#'                     \code{stat_gen} don't accept these parameters, which is
#'                     safer
#' @return A \code{MCHTest}-class object, a function with parameters \code{x},
#'         \code{alternative}, and \code{...}, with other parameters being
#'         passed to functions such as those passed to \code{test_stat} and
#'         \code{stat_gen}, controlling what's tested and how; depending on
#'         \code{lock_alternative}, the \code{alternative} argument may be
#'         ignored
#' @export
#' @examples
#' TODO: curtis: MORE EXAMPLES -- Sun 30 Sep 2018 01:29:04 AM MDT
#' dat1 <- c(0.16, 1.00, 0.67, 1.28, 0.31, 1.16, 1.25, 0.93, 0.66, 0.54)
#' mc.t.test <- MCHTest(test_stat = function(x, mu = 0) {
#'                        sqrt(length(x)) * (mean(x) - mu)/sd(x)
#'                      }, stat_gen = function(n, mu = 0) {
#'                        x <- rnorm(n, mean = mu)
#'                        sqrt(n) *  (mean(x) - mu)/sd(x)
#'                      }, seed = 123, method = "Monte Carlo t-Test",
#'                      test_params = "mu", lock_alternative = FALSE)
#' mc.t.test(dat1)
#' mc.t.test(dat1, mu = 0.1, alternative = "two.sided")
MCHTest <- function(test_stat, stat_gen, N = 10000, seed = NULL,
                    memoise_sample = TRUE, pval_func = pval,
                    method = "Monte Carlo Test", test_params = NULL,
                    fixed_params = NULL, lock_alternative = TRUE) {
  # TODO: curtis: TEST WITH NUISANCE PARAMETERS -- Sun 30 Sep 2018 01:28:33 AM MDT
  test_stat_formals <- names(formals(test_stat))
  stat_gen_formals <- names(formals(stat_gen))
  pval_formals <- names(formals(pval_func))

  # Requirement checking
  testthat::expect_is(test_stat, "function")
  testthat::expect_is(stat_gen, "function")
  testthat::expect_true("n" %in% stat_gen_formals)
  testthat::expect_true("x" %in% test_stat_formals)
  if (!is.null(test_params)) {
    testthat::expect_is(test_params, "character")
    testthat::expect_true(all(test_params %in% test_stat_formals))
    testthat::expect_true(all(test_params %in% stat_gen_formals))
  }
  if (!is.null(fixed_params)) {
    testthat::expect_is(fixed_params, "character")
    testthat::expect_true(all(fixed_params %in% test_stat_formals))
    testthat::expect_true(all(fixed_params %in% stat_gen_formals))
  }
  testthat::expect_true(all(c("S", "sample_S") %in% pval_formals))

  if (is.null(seed)) {
    if (memoise_sample) {
      warning("seed is NULL but memoization is enabled; separate function" %s%
              "calls will be identical with identical inputs")
    }
  }

  sample_gen <- function(...) {
    foreach <- foreach::foreach
    `%dorng%` <- doRNG::`%dorng%`
    `%dopar%` <- foreach::`%dopar%`

    args <- list(...)
    seed <- sample(1:999999999, 1)
    s <- foreach(i = 1:N, .combine = c, .options.RNG = seed) %dorng% {
      do.call(stat_gen, args)
    }
    attr(s, "rng") <- NULL
    as.numeric(s)
  }
  if (memoise_sample) {
    sample_gen <- memoise::memoise(sample_gen)
  }

  # The function to be returned
  f <- function(x, alternative = NULL, ...) {
    stat_args <- list(...)
    test_stat_args <- stat_args; test_stat_args$x <- x
    test_stat_args <- test_stat_args[which(
      names(test_stat_args) %in% test_stat_formals)]
    n <- length(x)
    stat_gen_args <- stat_args; stat_gen_args$n <- n
    stat_gen_args <- stat_gen_args[which(
      names(stat_gen_args) %in% stat_gen_formals)]

    S <- do.call(test_stat, test_stat_args)
    testthat::expect_equal(length(S), 1)
    sample_S <- do.call(sample_gen, stat_gen_args)

    pval_args <- list("S" = S, "sample_S" = sample_S)
    if (!lock_alternative) {
      pval_args$alternative <- alternative
    } else {
      alternative <- NULL
    }
    pval_args <- pval_args[which(names(pval_args) %in% pval_formals)]
    test_pval <- do.call(pval_func, pval_args)[[1]]
    if (test_pval < 0 | test_pval > 1) {
      stop("Bad p-value computed! It was" %s% test_pval)
    }
    testthat::expect_is(test_pval, "numeric")
    
    res <- list(
      "data.name" = deparse(substitute(x)),
      "method" = method,
      "statistic" = S,
      "p.value" = test_pval
    )

    if (!is.null(test_params)){
      params <- stat_args[test_params]
      missing_params <- setdiff(test_params, names(params))
      if (length(missing_params) > 0) {
        for (arg in missing_params) {
          params[[arg]] <- formals(test_stat)[[arg]]
        }
        params <- params[test_params]
      }
      params <- unlist(params)
      res$null.value <- params
    }

    if (!is.null(fixed_params)) {
      f_pars <- stat_args[fixed_params]
      missing_f_params <- setdiff(fixed_params, names(f_pars))
      if (length(missing_f_params) > 0) {
        for (arg in missing_f_params) {
          f_pars[[arg]] <- formals(test_stat)[[arg]]
        }
        f_pars <- f_pars[fixed_params]
      }
      f_pars <- unlist(f_pars)
      res$parameter <- f_pars
    }

    if (!is.null(alternative)) {
      res$alternative <- alternative
    }
    names(res[["statistic"]]) <- "S"
    class(res) <- "htest"

    res
  }
  class(f)  <- "MCHTest"

  f
}

################################################################################
# OBJECT UTILS
################################################################################

#' Is an Object of Type MCHTest?
#'
#' Checks whether its argument is an \code{\link{MCHTest}}-class object.
#'
#' @param x An R object
#' @return \code{TRUE} if \code{x} is an \code{MCHTest}-class objet,
#'         \code{FALSE} otherwise
#' @export
#' @examples
#' is.MCHTest(1)
is.MCHTest <- function(x) {
  inherits(x, "MCHTest")
}

#' Get Attributes of MCHTest Object
#'
#' Get the settings of an \code{\link{MCHTest}}-class object.
#'
#' @param x The \code{MCHTest}-class object
#' @return A list with all the variables relevant to \code{x}
#' @export
#' @examples
#' get_MCHTest_settings()  # TODO: EXAMPLE
get_MCHTest_settings <- function(x) {
  if (!is.MCHTest(x)) {stop(deparse(substitute(x)) %s% "is not an" %s%
                         "MCHTest-class object")}
  res <- as.list(environment(x))
  res$stat_gen_formals <- NULL
  res$sample_gen <- NULL
  res$f <- NULL
  res$pval_formals <- NULL
  res$test_stat_formals <- NULL

  res
}

#' Print \code{MCHTest}-Class Object
#'
#' Print an \code{link{MCHTest}}-class object.
#'
#' @param f The \code{MCHTest}-class object
#' @export
#' @examples
#' MCHT:::print.MCHTest()  # TODO: EXAMPLE
print.MCHTest <- function(f, prefix = "\t") {
  f_info <- get_MCHTest_settings(f)

  cat("\n")
  cat(strwrap("Details for" %s% f_info$method, prefix = prefix), sep = "\n")
  cat("\n")

  if (is.null(f_info$seed)) {
    f_info$seed <- "randomized"
  }
  cat("Seed: ", f_info$seed, "\n")
  cat("Replications: ", f_info$N, "\n")
  if (length(f_info$test_params) > 0) {
    cat("Tested Parameters: ", f_info$test_params, "\n")
    for (param in f_info$test_params) {
      val <- formals(f_info$test_stat)[[param]]
      if (!is.symbol(val) & !is.null(val)) {
        cat("Default", param %s0% ": ", val, "\n")
      }
    }
  }
  cat("\n")

  if (f_info$memoise_sample) {
    cat("Memoisation enabled\n")
  }
  if (f_info$lock_alternative) {
    cat("Argument \"alternative\" is locked\n")
  }

  if (f_info$memoise_sample | f_info$lock_alternative) {
    cat("\n")
  }
}
