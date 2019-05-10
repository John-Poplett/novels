context('Safe, independent construction of training and test sets')
library(novels)
library(plyr)

test_that("training and test sets have no duplicate files", {
  expect_false(empty(novels))
  indices <- createSplit(novels)
  train = novels[indices$train,]
  test = novels[indices$test,]
  expect_equal(sum(duplicated(train$file.name)), 0)
  expect_equal(sum(duplicated(test$file.name)), 0)
})

test_that("training and test sets have no authors in common", {
  expect_false(empty(novels))
  indices <- createSplit(novels)
  train = novels[indices$train,]
  test = novels[indices$test,]
  expect_true(empty(intersect(test %>% select(author), train %>% select(author))))
})
