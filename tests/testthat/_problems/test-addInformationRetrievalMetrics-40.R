# Extracted from test-addInformationRetrievalMetrics.R:40

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "novels", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context('Zero-denominator confusion matrix cells are handled explicitly, not silently')
library(novels)

# test -------------------------------------------------------------------------
data <- data.frame(TP = 40, TN = 40, FP = 10, FN = 10)
result <- addInformationRetrievalMetrics(data)
