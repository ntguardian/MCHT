################################################################################
# test-MCHTestChecks.R
################################################################################
# 2018-09-29
# Curtis Miller
################################################################################
# Tests for MCHTest-related functions
################################################################################

library(MCHT)
library(doParallel)
context("MCHTest Checks")

dat <- c(0.09, 1.36, 1.44, 0.71, 0.02, 0.34, 0.14, 0.44, 0.6, 0.16)

test_stat_1 <- function(x) {
  sqrt(length(x)) * mean(x)/sd(x)
}

test_stat_2 <- function(x, mu) {
  sqrt(length(x)) * (mean(x) - mu)/sd(x)
}

test_stat_3 <- function(x, mu, sigma) {
  sqrt(length(x)) * (mean(x) - mu)/sigma
}

test_stat_4 <- function(x, mu) {
  mean(x) - mu
}

test_gen_1 <- function(x) {
  sqrt(length(x)) * mean(x) / sd(x)
}

test_gen_2 <- function(x, mu) {
  x <- x + mu
  sqrt(length(x)) * (mean(x) - mu)/sd(x)
}

test_gen_3 <- function(x, mu, sigma) {
  x <- sigma * x + mu
  sqrt(length(x)) * (mean(x) - mu)/sd(x)
}

test_gen_4 <- function(x, mu, sigma) {
  x <- sigma * x + mu
  mean(x) - mu
}

rand_gen_1 <- function(n) {
  rnorm(n)
}

test_that("MCHTest() functions properly", {
  expect_is(MCHTest(test_stat_1, test_stat_1, rand_gen_1, seed = 100),
            "MCHTest")
  expect_true(is.MCHTest(MCHTest(test_stat_1, test_stat_1, rand_gen_1,
                                 seed = 100)))
  expect_warning(MCHTest(test_stat_1, test_stat_1, rand_gen_1))
  expect_error(MCHTest(test_stat_1, test_stat_1, rand_gen_1, seed = 100,
                       test_params = "mu"))
  expect_error(MCHTest(test_stat_1, test_gen_2, rand_gen_1, seed = 100,
                       test_params = "mu"))
  expect_error(MCHTest(test_stat_2, test_gen_2, rand_gen_1,
                       test_params = c("mu", "sigma"), seed = 100))
  expect_silent(MCHTest(test_stat_2, test_gen_2, rand_gen_1, test_params = "mu",
                        seed = 10))
  expect_silent(MCHTest(test_stat_1, test_stat_1, rand_gen_1,
                        memoise_sample = FALSE))
  expect_error(MCHTest(test_stat_2, test_gen_1, rand_gen_1, seed = 100,
                       test_params = "mu", fixed_params = "sigma"))
  expect_silent(MCHTest(test_stat_3, test_gen_3, rand_gen_1, seed = 100,
                        test_params = "mu", fixed_params = "sigma"))
  expect_error(MCHTest(test_stat_4, test_gen_3, rand_gen_1, seed = 100,
                       test_params = "mu", nuisance_params = "sigma"))
  expect_silent(MCHTest(test_stat_4, test_gen_3, rand_gen_1, seed = 100,
                        test_params = "mu", nuisance_params = "sigma",
                        optim_control = list(lower = c("sigma" = 0),
                                             upper = c("sigma" = 100))))
  expect_error(MCHTest(test_stat_4, test_gen_3, rand_gen_1, seed = 100,
                       test_params = "mu", nuisance_params = "sigma",
                       optim_control = list(lower = c(0),
                                            upper = c(100))))
  expect_error(MCHTest(test_stat_3, test_gen_3, rand_gen_1, seed = 100,
                       test_params = "mu", fixed_params = "sigma",
                       nuisance_params = "sigma", optim_control = list(
                         lower = c("sigma" = 0), upper = c("sigma" = 100)
                       )), "Parameter vectors cannot")
  expect_error(MCHTest(test_stat_1, rnorm, rand_gen_1, seed = 100))
  expect_silent(MCHTest(test_stat_1, test_gen_1, function(x) {x}, seed = 100))
  expect_error(MCHTest(test_stat_1, test_gen_1, function(y) {y}, seed = 100))
})

test_that("An MCHTest-class object functions properly", {
  mc.test.1 <- MCHTest(test_stat_1, test_stat_1, rand_gen_1, seed = 100,
                       N = 1000)
  mc.test.2 <- MCHTest(test_stat_1, test_stat_1, rand_gen_1, N = 1000,
                       memoise_sample = FALSE)
  mc.test.3 <- MCHTest(test_stat_2, test_gen_2, rand_gen_1, N = 100, seed = 100,
                       test_params = "mu", lock_alternative = FALSE)
  mc.test.4 <- MCHTest(test_stat_3, test_gen_3, rand_gen_1, N = 100, seed = 100,
                       test_params = "mu", fixed_params = "sigma",
                       lock_alternative = FALSE)
  mc.test.5 <- MCHTest(test_stat_4, test_gen_4, rand_gen_1, N = 100, seed = 100,
                       test_params = "mu", nuisance_params = "sigma",
                       optim_control = list("lower" = c("sigma" = 0),
                                            "upper" = c("sigma" = 100)),
                       lock_alternative = FALSE, threshold_pval = 0.2)
  mc.test.6 <- MCHTest(test_stat_4, test_gen_4, rand_gen_1, N = 100, seed = 100,
                       test_params = "mu", nuisance_params = "sigma",
                       optim_control = list("lower" = c("sigma" = 0),
                                            "upper" = c("sigma" = 100)),
                       lock_alternative = FALSE, threshold_pval = 0.2,
                       suppress_threshold_warning = TRUE)

  set.seed(1234)
  registerDoParallel(1)
  expect_success(mc.test.1(dat))
  expect_error(mc.test.3(dat), "argument \"mu\" is missing")
  expect_true(mc.test.2(dat - 0.5)$p.value != mc.test.2(dat - 0.5)$p.value)
  expect_equal(mc.test.1(dat)$statistic, c(S = 3.28607799050981))
  expect_equal(mc.test.1(dat)$p.value, 0.005)
  expect_is(mc.test.1(dat), "htest")
  expect_equal(mc.test.1(dat)$data.name, "dat")
  expect_equal(mc.test.1(dat), mc.test.1(dat, alternative = "two.sided"))
  expect_equal(mc.test.3(dat, alternative = "two.sided", mu = 0.5)$p.value,
               0.94)
  expect_equal(mc.test.3(dat, alternative = "greater", mu = 0.5)$p.value,
               0.47)
  expect_equal(mc.test.3(dat, alternative = "greater", mu = 0.5)$alternative,
               "greater")
  expect_error(MCHTest(test_stat_1, test_stat_1, rand_gen_1, seed = 100, N = 5,
                       pval_func = function(S, sample_S) {2})(dat),
               "Bad p-value")
  expect_equal(mc.test.4(dat, alternative = "two.sided", mu = 0.5,
                         sigma = 1)$p.value, 0.96)
  expect_equal(mc.test.4(dat, alternative = "two.sided", mu = 0.5,
                         sigma = 2)$p.value, 0.94)
  expect_warning(mc.test.5(dat, alternative = "two.sided", mu = 0.5),
                 "Computed p-value is greater than threshold value \\(0.2\\);")
  expect_silent(mc.test.6(dat, alternative = "two.sided", mu = 0.5))
  expect_equal(mc.test.6(dat, alternative = "two.sided", mu = 0.5)$p.value,
               0.9)

  ts <- function(x, sigma = 1) {sqrt(length(x)) * mean(x)/sigma}
  sg <- function(x, sigma = 1) {x <- sigma * x; ts(x, sigma = sigma)}
  rg <- function(n) {rnorm(n)}
  mc.test.7 <- MCHTest(ts, sg, rg, seed = 123, N = 100, fixed_params = "sigma")
  mc.test.8 <- MCHTest(ts, sg, rg, seed = 123, N = 100, fixed_params = "sigma",
                       localize_functions = TRUE,
                       imported_objects = list("ts" = ts))
  ts <- function(x) {mean(x)}

  expect_error(mc.test.7(dat, sigma = 2))
  expect_success(mc.test.8(dat, sigma = 2))

  mc.test.9 <- MCHTest(test_stat_1, function(x) {test_stat(x)},
                       function(x) {rnorm(length(x))}, seed = 123, N = 100,
                       localize_functions = TRUE)
  expect_success(mc.test.9(dat))
})

test_that("get_MCHTest_settings() functions properly", {
  f_setting <- get_MCHTest_settings(MCHTest(test_stat_2, test_gen_2, rand_gen_1,
                                            seed = 100, memoise_sample = TRUE,
                                            N = 10000, test_params = "mu",
                                            lock_alternative = FALSE))

  expect_equal(f_setting$seed, 100)
  expect_equal(f_setting$N, 10000)
  expect_true(f_setting$memoise_sample)
  expect_equal(f_setting$method, "Monte Carlo Test")
  expect_equal(f_setting$test_params, "mu")
  expect_false(f_setting$lock_alternative)
  expect_is(f_setting$test_stat, "function")
  expect_is(f_setting$stat_gen, "function")
  expect_is(f_setting$rand_gen, "function")
  expect_is(f_setting$pval_func, "function")
})
