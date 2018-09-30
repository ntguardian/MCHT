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
  expect_equal(pval(5, 1:10), 0.5)
  expect_equal(pval(5, 1:10, alternative = "less"), 0.5)
  expect_equal(pval(6, 1:10, alternative = "greater"), 0.4)
  expect_equal(pval(6, 1:10, alternative = "two.sided"), 0.8)
  expect_error(pval(6, 1:10, alternative = "none"))
})
