context('Valid construction of novels dataset')
library(novels)
library(plyr)

essential.columns = c("gutenberg_id", "genre", "response", "title",
                   "author", "fold", "language", "download.count",
                   "text")

test_that("novels dataset has an expected subset of columns", {
  expect_true(length(setdiff(essential.columns, names(novels))) == 0)
})

test_that("essential columns of the dataset have a complete set of values", {
  expect_true(all(complete.cases(novels[, essential.columns])))
})
