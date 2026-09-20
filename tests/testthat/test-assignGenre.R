context('assignGenre correctly excludes unmapped/NA allow-list rows')
library(novels)

test_that("an excluded category (target_genre == NA, as readr::read_csv() produces from an empty field) never leaks through as a pseudo-genre", {
  allowlist <- data.frame(
    gutenberg_bookshelf_category = c("Adventure", "Politics"),
    target_genre = c("Adventure", NA_character_),
    flag_for_review = c(FALSE, FALSE),
    note = c("", "subject"),
    stringsAsFactors = FALSE
  )
  result <- assignGenre(c("Adventure", "Politics"), allowlist)
  expect_setequal(result, "Adventure")
  expect_false("NA" %in% result)
  expect_false(any(is.na(result)))
})

test_that("an excluded category (target_genre == \"\", the literal-empty-string form) is also excluded", {
  allowlist <- data.frame(
    gutenberg_bookshelf_category = c("Adventure", "Politics"),
    target_genre = c("Adventure", ""),
    flag_for_review = c(FALSE, FALSE),
    note = c("", "subject"),
    stringsAsFactors = FALSE
  )
  result <- assignGenre(c("Adventure", "Politics"), allowlist)
  expect_setequal(result, "Adventure")
})

test_that("a book matching multiple mapped categories returns all of them, deduplicated", {
  allowlist <- data.frame(
    gutenberg_bookshelf_category = c("Historical Fiction", "Historical Novels", "Politics"),
    target_genre = c("Historical Fiction", "Historical Fiction", NA_character_),
    flag_for_review = c(FALSE, FALSE, FALSE),
    note = c("", "", "subject"),
    stringsAsFactors = FALSE
  )
  result <- assignGenre(c("Historical Fiction", "Historical Novels", "Politics"), allowlist)
  expect_setequal(result, "Historical Fiction")
})

test_that("allow_flagged = FALSE drops judgment-call mappings", {
  allowlist <- data.frame(
    gutenberg_bookshelf_category = c("Novels", "Adventure"),
    target_genre = c("Fiction", "Adventure"),
    flag_for_review = c(TRUE, FALSE),
    note = c("too generic to be a confident signal", ""),
    stringsAsFactors = FALSE
  )
  with_flagged <- assignGenre(c("Novels", "Adventure"), allowlist, allow_flagged = TRUE)
  without_flagged <- assignGenre(c("Novels", "Adventure"), allowlist, allow_flagged = FALSE)
  expect_setequal(with_flagged, c("Fiction", "Adventure"))
  expect_setequal(without_flagged, "Adventure")
})
