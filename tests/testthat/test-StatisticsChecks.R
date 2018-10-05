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
