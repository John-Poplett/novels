# Extracted from test-createSplit.R:10

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "novels", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context('Safe, independent construction of training and test sets')
library(novels)
library(plyr)

# test -------------------------------------------------------------------------
expect_false(empty(novels))
indices <- createSplit(novels)
train = novels[indices$train,]
test = novels[indices$test,]
expect_true(empty(intersect(train %>% select(gutenberg_id), test %>% select(gutenberg_id))))
