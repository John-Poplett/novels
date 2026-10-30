# Extracted from test-addInformationRetrievalMetrics.R:35

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "novels", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context('Zero-denominator confusion matrix cells are handled explicitly, not silently')
library(novels)

# test -------------------------------------------------------------------------
data <- data.frame(TP = 0, TN = 10, FP = 0, FN = 0)
result <- addInformationRetrievalMetrics(data)
expect_true(is.na(result$F.Score))
expect_false(is.nan(result$F.Score))
