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
  expect_equal(pval(5, 1:10), 0.6)
  expect_equal(pval(5, 1:10, alternative = "less"), 0.5)
  expect_equal(pval(6, 1:10, alternative = "greater"), 0.3)
  expect_equal(pval(6, 1:10, alternative = "two.sided"), 0.6)
  expect_error(pval(6, 1:10, alternative = "none"))
  expect_equal(pval(5, c(1:10, 5, 5, 5),
                    unif_gen = MCHT:::gen_memo_rng(runif)),
               0.769230769230769)
  expect_equal(pval(5, c(1:10, 5, 5, 5),
                    unif_gen = MCHT:::gen_memo_rng(runif, 123)),
               0.692307692307692)
  expect_equal(pval(5, c(1:10, 5, 5, 5),
                    unif_gen = MCHT:::gen_memo_rng(runif, 123)),
               0.692307692307692)
  expect_equal(pval(5, c(1:10, 5, 5, 5),
                    unif_gen = MCHT:::gen_memo_rng(runif)),
               0.846153846153846)
})
