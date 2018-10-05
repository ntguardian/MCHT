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
#' defines a bootstrap or Monte Carlo test.
#'
#' \code{MCHTest}-class objects are effectively functions that accept data and
#' maybe some parameters and return an \code{htest}-class object containing the
#' results of a Monte Carlo or bootstrap statistical test. These object will
#' accept datasets and perhaps some parameters and will return the results of a
#' test.
#'
#' Bootstrap tests can be implemented when the dataset is passed as an argument
#' to \code{rand_gen} (which occurs when \code{x} is one of \code{rand_gen}'s
#' parameters). The only difference between a Monte Carlo test and a bootstrap
#' test in the context of this function is that bootstrap tests use information
#' from the original dataset when generating simulated test statistics, while a
#' Monte Carlo test does not. When the default function for computing
#' \eqn{p}-values is used, this function will perform a test similar to that
#' described by \insertCite{mackinnon09;textual}{MCHT}.
#'
#' For Monte Carlo tests, when the default function for computing \eqn{p}-values
#' is used (see \code{\link{pval}}), this is effectively the test described in
#' \insertCite{dufour06;textual}{MCHT}. This includes using simulated annealing
#' to find values of nuisance parameters that maximize the \eqn{p}-value if the
#' null hypothesis is true. Simulated annealing is implemented using
#' \code{\link[GenSA]{GenSA}} from the \pkg{GenSA} package, and the
#' \code{optim_control} parameter is used for controlling \code{GenSA}'s
#' behavior. We highly recommend reading \code{GenSA}'s documentation.
#'
#' The \code{threshold_pval} argument can be used for stopping the optimization
#' procedure when a specified \eqn{p}-value is reached or surpassed.
#' \insertCite{dufour06;textual}{MCHT} showed that \eqn{p}-values found using
#' the procedure implemented here are conservative (in the sense that they are
#' larger than they necessarily need to be). If the algorithm terminates early
#' due to surpassing a prespecified \eqn{p}-value, then the estimated
#' \eqn{p}-value is known to at least be the value returned, but because the
#' \eqn{p}-value is a conservative estimate of the "true" \eqn{p}-value, this
#' latter number could be smaller. Thus we cannot say much about the location of
#' the true \eqn{p}-value if the algorithm terminates early. For this reason, a
#' \code{MCHTest}-class function will, by default, issue a warning if the
#' algorithm terminated early. However, by setting
#' \code{suppress_threshold_warning} to \code{TRUE}, this behavior can be
#' disabled. This recognizes the fact that even though an early termination
#' leads to us not being able to say much about the location of the true
#' \eqn{p}-value, we know that whatever the more accurate estimate is, we would
#' not reject the null hypothesis based on that result.
#'
#' This function uses \code{\link[foreach]{foreach}},
#' \code{\link[doRNG]{\%dorng\%}}, and \code{\link[foreach]{\%dopar\%}} to
#' perform simulations. If the R session is not set up at the start for
#' parallelization, there will be an initial complaint (after which there are no
#' more complaints), then these functions will default to using a single core.
#' The example shows how to set up R to use all available cores.
#'
#' @param test_stat A function that computes the test statistic from input data;
#'                  \code{x} must be a parameter of this function representing
#'                  test data 
#' @param stat_gen A function that generates values of the test statistic when
#'                 given data; \code{x} (representing a sample) must be a
#'                 parameter of this function, and this function is expected to
#'                 return one numeric output, but if \code{n} is a parameter,
#'                 this will be interpreted as sample size information (this
#'                 could be useful for allowing a "burn-in" period in random
#'                 data, as is often the case when working with time series
#'                 data)
#' @param rand_gen A function generating random data, accepting a parameter
#'                 \code{n} (representing the size of the data) or \code{x}
#'                 (which would be the actual data)
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
#'                    an argument of both \code{test_stat} and \code{stat_gen}),
#'                    and the resulting test will try to pass these parameters
#'                    to \code{rand_gen} (but these \emph{do not} need to be
#'                    parameters of \code{rand_gen})
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
#'                     \code{stat_gen} don't accept these parameters—which is
#'                     safer—and the resulting test will try to pass these
#'                     parameters to \code{rand_gen} (but these \emph{do not}
#'                     need to be parameters of \code{rand_gen})
#' @param nuisance_params A character vector of the names of parameters to be
#'                        treated as nuisance parameters which must be chosen
#'                        via optimization (see \insertCite{dufour06}{MCHT});
#'                        must be parameters of \code{test_stat} and
#'                        \code{stat_gen}, but these \emph{will not} be viewed
#'                        as parameters of \code{rand_gen}, and cannot be
#'                        non-\code{NULL} if code{optim_control} is \code{NULL}
#' @param optim_control A list of arguments to be passed to
#'                      \code{\link[GenSA]{GenSA}}, containing at least
#'                      \code{lower} and \code{upper} elements as named vectors,
#'                      with the names being identical to
#'                      \code{nuisance_params}, but could also include other
#'                      arguments to be passed to \code{\link[GenSA]{GenSA}};
#'                      the \code{fn} parameter will be set, and parameters of
#'                      that function will be the parameters mentioned in
#'                      \code{nuisance_params}, and this argument will be
#'                      ignored if \code{nuisance_params} is \code{NULL}
#' @param tiebreaking Break ties using the method as described in
#'                    \insertCite{dufour06;textual}{MCHT}; won't work if
#'                    \code{pval_func} doesn't support it via a \code{unif_gen}
#'                    argument, and should only be used for test statistics not
#'                    computed on continuously-distributed data
#' @param threshold_pval A numeric value that represents a threshold
#'                       \eqn{p}-value that, if surpassed by the optimization
#'                       algorithm, will cause the algorithm to terminate; will
#'                       override the \code{threshold.stop} argument in the
#'                       \code{control} list that's used by
#'                       \code{\link[GenSA]{GenSA}}
#' @param suppress_threshold_warning If \code{TRUE}, user will not be warned if
#'                                   the threshold \eqn{p}-value was surpassed
#'                                   by the optimization algorithm
#' @return A \code{MCHTest}-class object, a function with parameters \code{x},
#'         \code{alternative}, and \code{...}, with other parameters being
#'         passed to functions such as those passed to \code{test_stat} and
#'         \code{stat_gen}, controlling what's tested and how; depending on
#'         \code{lock_alternative}, the \code{alternative} argument may be
#'         ignored
#' @export
#' @examples
#' dat <- c(0.16, 1.00, 0.67, 1.28, 0.31, 1.16, 1.25, 0.93, 0.66, 0.54)
#' # Monte Carlo t-test for exponentially distributed data
#' mc.t.test <- MCHTest(test_stat = function(x, mu = 1) {
#'                        sqrt(length(x)) * (mean(x) - mu)/sd(x)
#'                      }, stat_gen = function(x, mu = 1) {
#'                        x <- x * mu
#'                        sqrt(length(x)) *  (mean(x) - mu)/sd(x)
#'                      }, rand_gen = rexp, seed = 123,
#'                      method = "Monte Carlo t-Test", test_params = "mu",
#'                      lock_alternative = FALSE)
#' mc.t.test(dat)
#' mc.t.test(dat, mu = 0.1, alternative = "two.sided")
#'
#' # Testing for the scale parameter of a Weibull distribution
#' # Two-sided test for location of scale parameter
#' library(MASS)
#' library(fitdistrplus)
#'
#' ts <- function(x, scale = 1) {
#'   fit_null <- coef(fitdist(x, "weibull", fix.arg = list("scale" = scale)))
#'   kt <- fit_null[["shape"]]
#'   l0 <- scale
#'   fit_all <- coef(fitdist(x, "weibull"))
#'   kh <- fit_all[["shape"]]
#'   lh <- fit_all[["scale"]]
#'   n <- length(x)
#' 
#'   # Test statistic, based on the negative-log-likelihood ratio
#'   suppressWarnings(n * ((kt - 1) * log(l0) - (kh - 1) * log(lh) -
#'       log(kt/kh) - log(lh/l0)) - (kt - kh) * sum(log(x)) + l0^(-kt) *
#'       sum(x^kt) - lh^(-kh) * sum(x^kh))
#' }
#' 
#' sg <- function(x, scale = 1, shape = 1) {
#'   x <- qweibull(x, shape = shape, scale = scale)
#'   ts(x, scale = scale)
#' }
#' 
#' mc.wei.shape.test <- MCHTest(ts, sg, seed = 123, test_params = "scale",
#'                              nuisance_params = "shape",
#'                              optim_control = list(
#'                                lower = c("shape" = 0),
#'                                upper = c("shape" = 100),
#'                                control = list("max.time" = 10)
#'                              ), threshold_pval = .2, N = 1000)
#' 
#' mc.wei.shape.test(rweibull(100, scale = 4, shape = 2), scale = 2)
#' 
#' # Bootstrap hypothesis test
#' # Kolmogorov-Smirnov test for Weibull distribution via parametric botstrap
#' # hypothesis test
#' 
#' ts <- function(x) {
#'   param <- coef(fitdist(x, "weibull"))
#'   shape <- param[['shape']]; scale <- param[['scale']]
#'   ks.test(x, pweibull, shape = shape, scale = scale,
#'           alternative = "two.sided")$statistic[[1]]
#' }
#' 
#' rg <- function(x) {
#'   n <- length(x)
#'   param <- coef(fitdist(x, "weibull"))
#'   shape <- param[['shape']]; scale <- param[['scale']]
#'   rweibull(n, shape = shape, scale = scale)
#' }
#' 
#' b.ks.test <- MCHTest(test_stat = ts, stat_gen = ts, rand_gen = rg,
#'                      seed = 123, N = 1000)
#' b.ks.test(rbeta(100, 2, 2))
#'
#' # Permutation test
#' 
#' df <- data.frame(
#'   val = c(rnorm(5, mean = 2, sd = 3), rnorm(10, mean = 1, sd = 2)),
#'   group = rep(c("x", "y"), times = c(5, 10))
#' )
#' 
#' ts <- function(x) {
#'   means <- aggregate(val ~ group, data = x, mean)
#'   vars <- aggregate(val ~ group, data = x, var)
#'   counts <- aggregate(val ~ group, data = x, length)
#' 
#'   (means$val[1] - means$val[2])/sum(vars$val / sqrt(counts$val))
#' }
#' 
#' rg <- function(x) {
#'   x$group <- sample(x$group)
#'   x
#' }
#' 
#' permute.test <- MCHTest(ts, ts, rg, seed = 123, N = 1000,
#'                         lock_alternative = FALSE)
#' 
#' permute.test(df, alternative = "two.sided")
MCHTest <- function(test_stat, stat_gen, rand_gen = stats::runif, N = 10000,
                    seed = NULL, memoise_sample = TRUE, pval_func = MCHT::pval,
                    method = "Monte Carlo Test", test_params = NULL,
                    fixed_params = NULL, nuisance_params = NULL,
                    optim_control = NULL, tiebreaking = FALSE,
                    lock_alternative = TRUE, threshold_pval = 1,
                    suppress_threshold_warning = FALSE) {
  force(pval_func)

  # Vector of names of function formals
  test_stat_formals <- names(formals(test_stat))
  stat_gen_formals <- names(formals(stat_gen))
  rand_gen_formals <- names(formals(rand_gen))
  pval_formals <- names(formals(pval_func))

  threshold_pval <- as.numeric(threshold_pval)
  if (threshold_pval <= 0) {stop("threshold_pval must be non-negative")}
  if (threshold_pval < 0.1 & !is.null(nuisance_params)) {
    warning("The threshold p-value specified does not permit much room for" %s%
            "failure to reject the null hypothesis! Consider picking a" %s%
            "value greater than 0.1.")
  }

  # Requirement checking
  testthat::expect_is(test_stat, "function")
  testthat::expect_is(stat_gen, "function")
  testthat::expect_true(("n" %in% rand_gen_formals) |
                        ("x" %in% rand_gen_formals))
  testthat::expect_true("x" %in% stat_gen_formals)
  testthat::expect_true("x" %in% test_stat_formals)
  for (param in list(test_params, fixed_params, nuisance_params)) {
    if (!is.null(param)) {
      check_params_in_functions(param, list(stat_gen))
    }
  }
  for (param in list(test_params, fixed_params)) {
    if (!is.null(param)) {
      check_params_in_functions(param, list(test_stat))
    }
  }
  testthat::expect_true(all(c("S", "sample_S") %in% pval_formals))
  if (!is.null(nuisance_params)) {
    if (is.null(optim_control)) stop("If nuisance_params is not NULL," %s%
                                     "optim_control must be a proper list")
    testthat::expect_is(optim_control, "list")
    testthat::expect_true(all(c("lower", "upper") %in% names(optim_control)))
    testthat::expect_equal(nuisance_params, names(optim_control$lower))
    testthat::expect_equal(nuisance_params, names(optim_control$upper))
  }
  if (length(intersect(nuisance_params, test_params)) > 0 |
      length(intersect(nuisance_params, fixed_params)) > 0 |
      length(intersect(test_params, fixed_params)) > 0) {
    stop("Parameter vectors cannot have parameters in common!")
  }
  testthat::expect_true(threshold_pval > 0 & threshold_pval <= 1)

  # Because memoisation conflicts with randomness, issue a warning if it appears
  # the user wants randomness but also turned on memoisation
  if (is.null(seed)) {
    if (memoise_sample) {
      warning("seed is NULL but memoization is enabled; separate function" %s%
              "calls will be identical with identical inputs")
    }
  }

  # Prepare p-value function
  memo_runif <- gen_memo_rng(stats::runif, seed = seed)
  if ("unif_gen" %in% names(formals(pval_func)) & tiebreaking) {
    orig_pval_func <- pval_func
    pval_func <- purrr::partial(orig_pval_func, unif_gen = memo_runif)
  }

  # Frequently used external functions
  foreach <- foreach::foreach
  `%dorng%` <- doRNG::`%dorng%`
  `%dopar%` <- foreach::`%dopar%`

  # Function responsible for generating random numbers to be used by stat_gen to
  # generate simulated statistics under H_0
  sample_gen <- function(...) {
    args <- list(...)
    testthat::expect_true(("n" %in% names(args)) |
                          ("x" %in% names(args)))

    if (is.null(seed)) {seed <- sample(1:999999999, 1)}
    s <- foreach(i = 1:N, .options.RNG = seed) %dorng% {
      do.call(rand_gen, args)
    }
    testthat::expect_is(s, "list")
    attr(s, "rng") <- NULL
    s
  }

  # Function responsible for returning simulated statistics under the null
  # hypothesis, accounting for possible nuisance parameters
  stat_sim <- function(sample_gen_args, pval_args, ...) {
    stat_gen_args <- list(...)
    testthat::expect_is(sample_gen_args, "list")
    testthat::expect_true(("n" %in% names(sample_gen_args)) |
                          ("x" %in% names(sample_gen_args)))
    testthat::expect_is(pval_args, "list")

    samples <- do.call(sample_gen, sample_gen_args)
    # Just return a sample of simulated statistics under H0
    if (is.null(nuisance_params)) {
      foreach(x = samples, .combine = c) %dopar% {
        stat_gen_args$x <- x
        do.call(stat_gen, stat_gen_args)
      }
    } else {
      if (!is.null(optim_control$control)) {
        optim_control$control$threshold.stop = -threshold_pval
      } else {
        optim_control$control <- list("threshold.stop" = -threshold_pval)
      }
      # The function to be optimized
      fn <- function(pm, ...) {
        names(pm) <- nuisance_params
        for (np in nuisance_params) {
          stat_gen_args[[np]] <- pm[[np]]
        }
        pval_args$sample_S <- foreach(x = samples, .combine = c) %dopar% {
          stat_gen_args$x <- x
          do.call(stat_gen, stat_gen_args)
        }
        -do.call(pval_func, pval_args)[[1]]
      }

      # Optimize the function
      optim_control$fn <- fn
      optim_res <- do.call(GenSA::GenSA, optim_control)

      # Now that we have adversariallly chosen the nuisance parameters, return
      # the "worst case" sample of simulated statistics
      for (np in nuisance_params) {
        stat_gen_args[[np]] <- optim_res$par[[np]]
      }
      foreach(x = samples, .combine = c) %dopar% {
        stat_gen_args$x <- x
        do.call(stat_gen, stat_gen_args)
      }
    }
  }

  if (memoise_sample) {
    sample_gen <- memoise::memoise(sample_gen)
    stat_sim <- memoise::memoise(stat_sim)
  }

  # The function to be returned
  f <- function(x, alternative = NULL, ...) {
    # Setting up arguments for these functions
    stat_args <- list(...)
    if (!is.null(dim(x))) {
      n <- nrow(x)
    } else {
      n <- length(x)
    }
    test_stat_args <- stat_args
    test_stat_args$x <- x
    test_stat_args <- test_stat_args[which(
      names(test_stat_args) %in% test_stat_formals)]

    rand_gen_args <- stat_args
    rand_gen_args$n <- n
    rand_gen_args$x <- x
    rand_gen_args <- rand_gen_args[which(
      !(names(rand_gen_args) %in% nuisance_params) & 
        (names(rand_gen_args) %in% rand_gen_formals))]

    S <- do.call(test_stat, test_stat_args)
    testthat::expect_equal(length(S), 1)

    pval_args <- list("S" = S)
    if (!lock_alternative) {
      pval_args$alternative <- alternative
    } else {
      alternative <- NULL
    }
    pval_args <- pval_args[which(names(pval_args) %in% pval_formals)]

    stat_gen_args <- stat_args
    stat_gen_args$n <- n
    stat_gen_args <- stat_gen_args[which(
      names(stat_gen_args) %in% stat_gen_formals)]
    stat_gen_args$sample_gen_args <- rand_gen_args
    stat_gen_args$pval_args <- pval_args
    stat_gen_args$pval_args$sample_S <- NULL

    # Get a sample of simulated statistics, and compute a p-value
    testthat::expect_is(stat_gen_args, "list")
    sample_S <- do.call(stat_sim, stat_gen_args)
    pval_args$sample_S <- sample_S
    test_pval <- do.call(pval_func, pval_args)[[1]]
    if (test_pval < 0 | test_pval > 1) {
      stop("Bad p-value computed! It was" %s% test_pval)
    }
    testthat::expect_is(test_pval, "numeric")
    if (test_pval >= threshold_pval & threshold_pval < 1 &
        length(nuisance_params) > 0 & !suppress_threshold_warning) {
      warning("Computed p-value is greater than threshold value (" %s0%
              threshold_pval %s0% "); the optimization algorithm may have" %s%
              "terminated early")
    }
    
    # Set up the htest-class object to be returned
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
#' f <- MCHTest(mean, mean, seed = 100)
#' is.MCHTest(1)
#' is.MCHTest(f)
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
#' f <- MCHTest(mean, mean, seed = 100)
#' get_MCHTest_settings(f)
get_MCHTest_settings <- function(x) {
  if (!is.MCHTest(x)) {stop(deparse(substitute(x)) %s% "is not an" %s%
                         "MCHTest-class object")}
  res <- as.list(environment(x))
  res$stat_gen_formals <- NULL
  res$sample_gen <- NULL
  res$f <- NULL
  res$pval_formals <- NULL
  res$test_stat_formals <- NULL
  res$rand_gen_formals <- NULL
  res$`%dorng%` <- NULL
  res$`%dopar%` <- NULL
  res$foreach <- NULL
  res$stat_sim <- NULL
  res$param <- NULL

  res
}

#' Print \code{MCHTest}-Class Object
#'
#' Print an \code{link{MCHTest}}-class object.
#'
#' @param x The \code{MCHTest}-class object
#' @param ... Other arguments, such as \code{prefix} (a string wrapped around
#'            the first line; by default, \code{"\t"})
#' @export
#' @examples
#' f <- MCHTest(mean, mean, seed = 100)
#' print(f)
print.MCHTest <- function(x, ...) {
  f_info <- get_MCHTest_settings(x)
  args <- list(...)
  if (is.null(args$prefix)) {
    args$prefix <- "\t"
  }
  prefix <- args$prefix

  cat("\n")
  cat(strwrap("Details for" %s% f_info$method, prefix = prefix), sep = "\n")
  cat("\n")

  if (is.null(f_info$seed)) {
    f_info$seed <- "randomized"
  }
  cat("Seed: ", f_info$seed, "\n")
  cat("Replications: ", f_info$N, "\n")

  print_params <- function(params, param_string) {
    if (length(params) > 0) {
      cat(param_string %s0% ": ", params, "\n")
      for (p in params) {
        if (!is.symbol(formals(f_info$test_stat)[[p]])) {
          val <- formals(f_info$test_stat)[[p]]
          if (!is.null(val)) {
            cat("Default", p %s0% ": ", val, "\n")
          }
        }
      }
    }
  }
  
  print_params(f_info$test_params, "Tested Parameters")
  print_params(f_info$fixed_params, "Assumed Parameters")
  if (length(f_info$nuisance_params) > 0) {
    cat("Nuisance Parameters: ", f_info$nuisance_params, "\n")
  }
  cat("\n")

  if (f_info$memoise_sample) {
    cat("Memoisation enabled\n")
  }
  if (f_info$lock_alternative) {
    cat("Argument \"alternative\" is locked\n")
  }
  if (length(f_info$nuisance_params) > 0 & f_info$threshold_pval < 1) {
    cat("Threshold p-Value: ", f_info$threshold_pval, "\n")
    if (f_info$suppress_threshold_warning) {
      cat("Threshold p-value warnings suppressed\n")
    }
  }

  if (f_info$memoise_sample | f_info$lock_alternative) {
    cat("\n")
  }
}
