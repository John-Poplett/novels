context('Safe, independent construction of training and test sets')
library(novels)
library(plyr)

test_that("training and test sets share no gutenberg_id", {
  expect_false(empty(novels))
  indices <- createSplit(novels)
  train = novels[indices$train,]
  test = novels[indices$test,]
  expect_true(empty(intersect(train %>% select(gutenberg_id), test %>% select(gutenberg_id))))
})

test_that("training and test sets have no authors in common", {
  expect_false(empty(novels))
  indices <- createSplit(novels)
  train = novels[indices$train,]
  test = novels[indices$test,]
  expect_true(empty(intersect(test %>% select(author), train %>% select(author))))
})
