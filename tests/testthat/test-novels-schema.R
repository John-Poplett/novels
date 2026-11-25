context('The shipped novels dataset matches its own documentation')
library(novels)

test_that("novels has exactly the columns data.R documents", {
  # This is the single most valuable regression guard this project could
  # have added early: nearly every bug found in this codebase's review
  # traced back to some function or doc assuming a schema (file.name,
  # fold.number, date, a bare id column) that tidyRawData() had already
  # moved on from. Update this list deliberately whenever the schema
  # changes on purpose -- a failure here means either the docs or the
  # pipeline drifted without the other being updated.
  documented_columns <- c(
    "text", "gutenberg_id", "genre", "fold", "author",
    "gutenberg_author_id", "response", "download_count",
    "title", "language", "rights", "gutenberg_bookshelf",
    "has_text"
  )
  expect_setequal(names(novels), documented_columns)
})

test_that("novels has no leftover parsing/join artifacts", {
  # is_record and id both made it into the shipped dataset before being
  # caught and removed -- guard against either (or a similar future
  # artifact) silently reappearing.
  expect_false("is_record" %in% names(novels))
  expect_false("id" %in% names(novels))
  expect_false("file.name" %in% names(novels))
})

test_that("novels has no missing download counts, and the correct row count", {
  expect_false(any(is.na(novels$download_count)))
  expect_equal(nrow(novels), 798)
})
