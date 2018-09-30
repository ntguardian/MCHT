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
#' test statistic under the null hypothesis, with \eqn{1 \leq i \leq N}. By
#' default, this function computes \eqn{p}-values via
#'
#' \deqn{p = \hat{p} = \frac{1}{N}\sum_{i = 1}^{N} \mathbb{1}_{\{S \geq S_i\}}}
#'
#' where \eqn{\mathbb{1}_{\left{S \in A\right}} = 1} if \eqn{S \in A} and is 0
#' otherwise. This function is designed to handle an \code{alternative}
#' parameter similar to what appears in other \pkg{stats} functions like
#' \code{\link[stats]{t.test}}. If \code{alternative} is \code{"less"}, then
#' \eqn{p = \hat{p}}; if \code{alternative} is \code{"greater"}, then \eqn{p =
#' 1 - \hat{p}}; and if \code{alternative} is \code{"two.sided"}, then \eqn{p =
#' 2 \min(\hat{p}, 1 - \hat{p})}. Any other value raises an error.
#'
#' The parameter \code{S} is \eqn{S}, and the vector \code{sample_S} is the
#' vector containing the values \eqn{S_i}.
#'
#' @param S The value of the test statistic
#' @param sample_S Simulated values of the 
#' @param alternative A string specifying the alternative hypothesis, or
#'                    \code{NULL}
#' @return A number representing the \eqn{p}-value.
#' @export
#' @examples
#' sample_S <- rnorm(10)
#' pval(1.01, sample_S)
#' pval(1.01, sample_S, alternative = "greater")
pval <- function(S, sample_S, alternative = NULL) {
  sample_S <- as.numeric(sample_S)
  if (is.null(alternative)) {
    alternative = "less"
  }

  if (alternative == "less") {
    mean(S >= sample_S)
  } else if (alternative == "greater") {
    mean(S < sample_S)
  } else if (alternative == "two.sided") {
    2 * min(mean(S < sample_S), mean(S >= sample_S))
  } else {
    stop("Don't know how to interpret alternative")
  }
}
