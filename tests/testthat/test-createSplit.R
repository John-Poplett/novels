context('Safe, independent construction of training and test sets')
library(novels)

test_that("training and test sets share no gutenberg_id", {
  expect_true(nrow(novels) > 0)
  indices <- createSplit(novels)
  train = novels[indices$train,]
  test = novels[indices$test,]
  expect_equal(nrow(dplyr::intersect(dplyr::select(train, gutenberg_id), dplyr::select(test, gutenberg_id))), 0)
})

test_that("training and test sets have no authors in common", {
  expect_true(nrow(novels) > 0)
  indices <- createSplit(novels)
  train = novels[indices$train,]
  test = novels[indices$test,]
  expect_equal(nrow(dplyr::intersect(dplyr::select(test, author), dplyr::select(train, author))), 0)
})
