################################################################################
# test-StatisticsChecks.R
################################################################################
# 2018-09-30
# Curtis Miller
################################################################################
# Check that statistics functions work properly
################################################################################

library(MCHT)
context("Check statistical functions")

dat <- c(0.86, 2.62, 0.59, 0.84, 1.55, 4.06, 1.83, 0.67, 0.4, 0.92)
df1 <- data.frame("v1" = c(dat, dat + 2, 1),
                  "v2" = rep(c("x", "y", "z"),
                             times = c(length(dat), length(dat), 1)))

test_that("pval works properly", {
  set.seed(123)
  expect_equal(pval(5, 1:10, unif_gen = runif), 0.4)
  expect_equal(pval(5, 1:10, alternative = "greater", unif_gen = runif), 0.5)
  expect_equal(pval(6, 1:10, alternative = "less", unif_gen = runif), 0.7)
  expect_equal(pval(6, 1:10, alternative = "two.sided", unif_gen = runif), 0.6)
  expect_error(pval(6, 1:10, alternative = "none", unif_gen = runif))
  expect_equal(pval(5, c(1:10, 5, 5, 5),
                    unif_gen = MCHT:::gen_memo_rng(runif)),
               0.230769230769231)
  expect_equal(pval(5, c(1:10, 5, 5, 5),
                    unif_gen = MCHT:::gen_memo_rng(runif, 123)),
               0.307692307692308)
  expect_equal(pval(5, c(1:10, 5, 5, 5),
                    unif_gen = MCHT:::gen_memo_rng(runif, 123)),
               0.307692307692308)
  expect_equal(pval(5, c(1:10, 5, 5, 5),
                    unif_gen = MCHT:::gen_memo_rng(runif)),
               0.153846153846154)
  expect_equal(pval(6, 1:10), 0.4)
})

test_that("Test statistic functions work properly", {
  expect_equal(stat_t(dat, mu = 2), -1.56416733137808)
  expect_error(stat_t_two_sample(df1, id_var = "v2",
                                 value_var = "v1"),
               "Not two unique groups in v2")
  expect_equal(stat_t_two_sample(df1[1:20,], delta = -3, id_var = "v2",
                                 value_var = "v1"), sqrt(5)/sd(dat))
})
