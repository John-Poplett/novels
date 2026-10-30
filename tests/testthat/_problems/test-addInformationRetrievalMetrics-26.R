# Extracted from test-addInformationRetrievalMetrics.R:26

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "novels", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context('Zero-denominator confusion matrix cells are handled explicitly, not silently')
library(novels)

# test -------------------------------------------------------------------------
data <- data.frame(TP = 5, TN = 0, FP = 0, FN = 2)
result <- addInformationRetrievalMetrics(data)
