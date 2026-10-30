# Extracted from test-createSplit.R:9

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "novels", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context('Safe, independent construction of training and test sets')
library(novels)

# test -------------------------------------------------------------------------
expect_true(nrow(novels) > 0)
indices <- createSplit(novels)
train = novels[indices$train,]
test = novels[indices$test,]
expect_equal(nrow(intersect(train %>% select(gutenberg_id), test %>% select(gutenberg_id))), 0)
