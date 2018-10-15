################################################################################
# StatisticalFunctions.R
################################################################################
# 2018-09-29
# Curtis Miller
################################################################################
# Functions for statistical tasks.
################################################################################

################################################################################
# P-VALUE COMPUTATION
################################################################################

#' Compute \eqn{p}-Value For a Test Statistic
#'
#' Compute the \eqn{p}-value of a test statistic for Monte Carlo tests.
#'
#' Let \eqn{S} be a test statistic and \eqn{S_i} be simulated values of that
#' test statistic under the null hypothesis, with \eqn{1 \leq i \leq N}. If
#' \code{unif_gen} is not \code{NULL}, this function computes \eqn{p}-values via
#'
#' \deqn{p = \hat{p} = \frac{1}{N}\sum_{i = 1}^{N} I_{\{(S, U_0) \leq
#' (S_i, U_i)\}}}
#'
#' where \eqn{I_{\{S \in A\}} = 1} if \eqn{S \in A} and is 0
#' otherwise, \eqn{U_i} are uniformly distributed random variables, and the
#' ordering over tuples is lexicographical ordering, as described by
#' \insertCite{dufour06;textual}{MCHT}.
#'
#' If \code{unif_gen} is \code{NULL}, then the random variables are not
#' generated and not used to break ties.
#'
#' This function is designed to handle an \code{alternative} parameter similar
#' to what appears in other \pkg{stats} functions like
#' \code{\link[stats]{t.test}}. If \code{alternative} is \code{"less"}, then
#' \eqn{p = \hat{p}}; if \code{alternative} is \code{"greater"}, then
#' \eqn{p = 1 - \hat{p}}; and if \code{alternative} is \code{"two.sided"}, then
#' \eqn{p = 2 \min(\hat{p}, 1 - \hat{p})}. Any other value raises an error.
#'
#' The parameter \code{S} is \eqn{S}, and the vector \code{sample_S} is the
#' vector containing the values \eqn{S_i}.
#'
#' @param S The value of the test statistic
#' @param sample_S Simulated values of the 
#' @param alternative A string specifying the alternative hypothesis, or
#'                    \code{NULL}
#' @param unif_gen If not \code{NULL}, the function generating
#'                 uniformly-distributed random variables for breaking ties; if
#'                 \code{NULL}, no tie breaking is done
#' @return A number representing the \eqn{p}-value.
#' @references
#'   \insertAllCited{}
#' @export
#' @examples
#' sample_S <- rnorm(10)
#' pval(1.01, sample_S)
#' pval(1.01, sample_S, alternative = "greater")
pval <- function(S, sample_S, alternative = NULL, unif_gen = NULL) {
  if (!is.null(unif_gen)) {
    check_params_in_functions("n", list(unif_gen))
  }

  N <- length(sample_S)
  sample_S <- as.numeric(sample_S)
  if (is.null(alternative)) {
    alternative <- "greater"
  }

  empirical_cdf_value <- mean(sample_S <= S)
  if (is.function(unif_gen)) {
    tiebreakers <- unif_gen(N + 1)
    empirical_cdf_value <- empirical_cdf_value + mean(
      (tiebreakers[1:N] <= tiebreakers[N + 1]) & (S == sample_S))
  }
  if (alternative == "greater") {
    1 - empirical_cdf_value
  } else if (alternative == "less") {
    empirical_cdf_value
  } else if (alternative == "two.sided") {
    2 * min(empirical_cdf_value, 1 - empirical_cdf_value)
  } else {
    stop("Don't know how to interpret alternative")
  }
}

################################################################################
# RANDOM VARIABLE GENERATION
################################################################################

#' Memoised Random Variable Generation
#'
#' Creates a function that generates random numbers with memoization
#'
#' This is a function generator, with the returned function being one that can
#' handle a set seed and will remember if it needs to regenerate a new set of
#' random numbers. This allows both for control over random number generation
#' and for faster performance.
#'
#' @param r The random number generator
#' @param seed The seed value, to be passed to \code{\link[base]{set.seed}}
#' @return A function that generates random numbers with a set seed and with
#'         memoization; accepts \code{seed} and all other arguments that could
#'         be passed to the original random number generator
#' @examples
#' memo_runif <- MCHT:::gen_memo_rng(runif)
#' memo_runif(10)
gen_memo_rng <- function(r, seed = NULL) {
  force(r)
  force(seed)
  old_seed <- NULL
  out <- NULL
  old_args <- NULL
  
  f <- function(...) {
    args <- list(...)
    if (!exists(".Random.seed")) {
      stats::runif(1)  # Call a random number that won't be used; initiates RNG
    }
    if (!is.null(seed)) {
      old_seed <<- .Random.seed
      set.seed(seed)
    }
    # Check for change in random state or whether function's been called or
    # whether call itself has changed
    if (is.null(out) | any(.Random.seed != old_seed) |
        !identical(args, old_args)) {
      # Regenerate random data
      out <<- do.call(r, args)
      old_args <<- args
      # Enables memoization even if seed not set
      if (is.null(seed) | is.null(old_seed)) {
        old_seed <<- .Random.seed
      }
    }
    if (!is.null(seed)) {
      .Random.seed <<- old_seed
    }
    out
  }
}

################################################################################
# USEFUL TEST STATISTICS
################################################################################

#' One-Sample t Statistic
#'
#' The one-sample \eqn{t}-statistic.
#'
#' Let \eqn{\mu} correspond to the parameter \code{mu} representing the mean
#' under the null hypothesis. Then the \eqn{t}-statistic is
#'
#' \deqn{\frac{\bar{x} - \mu}{s/\sqrt{n}}}
#'
#' where \eqn{\bar{x}} is the sample mean, \eqn{s} is the sample standard
#' deviation, and \eqn{n} is the sample size. \insertNoCite{student08}{MCHT}
#'
#' @param x The dataset for which to compute the \eqn{t} statistic
#' @param mu The mean under the null hypothesis
#' @return Number representing the value of the \eqn{t}-statistic
#' @references
#'   \insertAllCited{}
#' @export
#' @examples
#' stat_t(rnorm(10), mu = 1)
stat_t <- function(x, mu = 0) {
  sqrt(length(x)) * (mean(x) - mu)/sd(x)
}

#' Two-Sample t Statistic
#'
#' Test statistic for Welch's two-sample \eqn{t}-test
#' \insertCite{welch47}{MCHT}.
#'
#' Let \eqn{\Delta} correspond to the difference in the population means of the
#' two samples under the null hypothesis. Welch's two-sample \eqn{t} statistc is
#'
#' \deqn{\frac{\bar{x} - \bar{y} - \Delta}{\sqrt{\frac{s_x^2}{n_x} +
#' \frac{s_y^2}{n_y}}}}
#'
#' where \eqn{\bar{x}} and \eqn{\bar{y}} are the means of the two respective
#' samples, \eqn{s_x} and \eqn{s_y} are the standard devations of the two
#' respective samples, and \eqn{n_x} and \eqn{n_y} are the sizes of the two
#' samples.
#'
#' The function parameter \code{x} does not correspond to the sample \eqn{x} as
#' described above but to a data frame that contains both samples, with a
#' variable representing the value of the data points of the samples and a
#' variable identifying the sample to which a sample belongs.
#'
#' Which sample is considered the \eqn{x} sample and which the \eqn{y} sample in
#' the above calculation is determined by lexicographical order.
#'
#' @param x A data frame with two columns, \code{id_var} and \code{value_var},
#'          and no others
#' @param delta The difference in population means under the null hypothesis
#' @param id_var The name of the variable identifying which sample an
#'               observation (row of the data frame) belongs to; the column must
#'               either contain character data or a \code{\link[base]{factor}}
#'               with two levels identifying the sample (will be coerced to
#'               \code{factor} if character)
#' @param value_var The numeric variable representing observation values
#' @return Number representing the value of the statistic
#' @references
#'   \insertAllCited{}
#' @examples
#' df <- data.frame("value" = c(rnorm(10), rnorm(5) + 1),
#'                  "group" = rep(c("x", "y"), times = c(10, 5)))
#' stat_t_two_sample(df)
stat_t_two_sample <- function(x, delta = 0, id_var = "group",
                              value_var = "value") {
  testthat::expect_identical(sort(names(x)), sort(c(id_var, value_var)))
  x[[id_var]] <- as.character(x[[id_var]])
  x[[id_var]] <- as.factor(x[[id_var]])
  if (length(levels(x[[id_var]])) != 2) {stop("Not two unique groups in" %s%
                                               id_var)}
  x <- x[, c(value_var, id_var)]
  names(x) <- c("value", "group")

  means <- aggregate(value ~ group, data = x, FUN = mean)
  sigma2s <- aggregate(value ~ group, data = x, FUN = var)
  sizes <- aggregate(value ~ group, data = x, FUN = length)

  means <- means[order(as.character(means$group)), "value"]
  sigma2s <- sigma2s[order(as.character(sigma2s$group)), "value"]
  sizes <- sizes[order(as.character(sizes$group)), "value"]

  (means[1] - means[2] - delta)/sqrt(sum(sigma2s/sizes))
}

# TODO: curtis: CHI-SQUARE STATISTIC INDEPENDENCE -- Mon 15 Oct 2018 01:17:27 PM MDT
# TODO: curtis: CHI-SQUARE STATISTIC VARIANCE -- Mon 15 Oct 2018 01:17:27 PM MDT
# TODO: curtis: F-STATISTIC TWO SAMPLE VARIANCE -- Mon 15 Oct 2018 01:17:27 PM MDT
# TODO: curtis: F-STATISTIC LINEAR REGRESSION -- Mon 15 Oct 2018 01:17:27 PM MDT
# TODO: curtis: CHI-SQUARE STATISTIC GOODNESS-OF-FIT -- Mon 15 Oct 2018 01:17:27 PM MDT
# TODO: curtis: ANOVA STATISTIC -- Mon 15 Oct 2018 01:17:27 PM MDT
