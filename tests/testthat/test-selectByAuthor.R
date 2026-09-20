context('Author matching handles co-authored books')
library(novels)

test_that("selectByAuthor matches a co-author who isn't listed first", {
  data <- data.frame(
    author = c("Dumas, Alexandre; Maquet, Auguste", "Verne, Jules"),
    stringsAsFactors = FALSE
  )
  # Maquet is the second co-author, joined with "; " by tidyRawData()'s
  # group_by/summarize dedup step. This currently FAILS: selectByAuthor()
  # strips everything after the first comma, so "Maquet" is never
  # matchable for this row.
  expect_true(selectByAuthor("Maquet", data)[1])
  expect_false(selectByAuthor("Maquet", data)[2])
})

test_that("selectByAuthor still matches the first-listed author", {
  data <- data.frame(
    author = c("Dumas, Alexandre; Maquet, Auguste", "Verne, Jules"),
    stringsAsFactors = FALSE
  )
  expect_true(selectByAuthor("Dumas", data)[1])
  expect_true(selectByAuthor("Verne", data)[2])
  expect_false(selectByAuthor("Dumas", data)[2])
})
