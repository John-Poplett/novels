context('Valid construction of novels dataset')
library(novels)
library(plyr)

novels.columns = c("gutenberg_id", "genre", "response", "title", 
                   "author", "language", "download.count", "date", "text")

test_that("novels dataset has an expected subset of columns", {
  # expect_true(setdiff(names(novels), novels.columns) >= 0)
  expect_true(length(setdiff(novels.columns, names(novels))) == 0)
})

test_that("dataset has a complete set of values", {
  expect_true(all(complete.cases(novels)))
})
