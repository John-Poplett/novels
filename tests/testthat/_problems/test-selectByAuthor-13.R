# Extracted from test-selectByAuthor.R:13

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "novels", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context('Author matching handles co-authored books')
library(novels)

# test -------------------------------------------------------------------------
data <- data.frame(
    author = c("Dumas, Alexandre; Maquet, Auguste", "Verne, Jules"),
    stringsAsFactors = FALSE
  )
expect_true(selectByAuthor("Maquet", data)[1])
