context("Shipped genre allow-list accessor")

# genreAllowlist() exists so the exported genre API has a canonical allow-list
# without callers having to know where the file lives. These tests pin the
# accessor's contract and a few known mappings; they deliberately do not pin
# the whole table, which is expected to grow as the corpus does.

test_that("genreAllowlist() returns the documented columns", {
  allowlist <- genreAllowlist()

  expect_s3_class(allowlist, "data.frame")
  expect_identical(
    names(allowlist),
    c("gutenberg_bookshelf_category", "target_genre",
      "flag_for_review", "note")
  )
  expect_type(allowlist$gutenberg_bookshelf_category, "character")
  expect_type(allowlist$target_genre, "character")
  expect_type(allowlist$flag_for_review, "logical")
  expect_type(allowlist$note, "character")
  expect_gt(nrow(allowlist), 0)
})

test_that("genreAllowlist() marks every row as include, exclude or review", {
  allowlist <- genreAllowlist()

  # An NA flag would silently survive `!allowlist$flag_for_review`, so the
  # column must be a complete logical vector.
  expect_false(anyNA(allowlist$flag_for_review))
  expect_true(any(allowlist$flag_for_review))
  expect_true(any(!allowlist$flag_for_review))

  # Exclusions are represented by a missing target_genre, never "".
  expect_false(any(allowlist$target_genre == "", na.rm = TRUE))
  expect_true(anyNA(allowlist$target_genre))
})

test_that("genreAllowlist() maps a known category to its genre", {
  allowlist <- genreAllowlist()

  expect_identical(assignGenre("Adventure", allowlist), "Adventure")
  expect_identical(
    assignGenre("Science Fiction", allowlist),
    "Science Fiction"
  )
})

test_that("genreAllowlist() records excluded categories with no genre", {
  allowlist <- genreAllowlist()

  # "Africa" is a geography label, not a genre: present in the table so the
  # decision is recorded, mapped to nothing. Subset with an explicit NA guard:
  # the table's first row has an NA category, and a bare `==` comparison yields
  # an NA index, which inserts an all-NA row rather than dropping it.
  row <- allowlist[
    !is.na(allowlist$gutenberg_bookshelf_category) &
      allowlist$gutenberg_bookshelf_category == "Africa", ,
    drop = FALSE
  ]
  expect_identical(nrow(row), 1L)
  expect_true(is.na(row$target_genre))
  expect_length(assignGenre("Africa", allowlist), 0)
})

test_that("allow_flagged = FALSE drops judgment-call mappings", {
  allowlist <- genreAllowlist()

  flagged <- allowlist[
    allowlist$flag_for_review & !is.na(allowlist$target_genre), ,
    drop = FALSE
  ]
  expect_gt(nrow(flagged), 0)

  category <- flagged$gutenberg_bookshelf_category[[1]]
  genre <- flagged$target_genre[[1]]

  expect_identical(assignGenre(category, allowlist), genre)
  expect_length(assignGenre(category, allowlist, allow_flagged = FALSE), 0)
})
