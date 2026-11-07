context('compareGenreAssignment compares label sets, not single values')
library(novels)

# Two sides: trusted Stonybrook labels (one row per book and genre) and the
# bookshelf entries assignGenre() reads. The fixture keeps a book with two
# genres, a book whose only agreement depends on the rename map, a clean
# disagreement, a flagged-only mapping and a book with no usable shelf.
fixture <- function() {
  allowlist <- data.frame(
    gutenberg_bookshelf_category = c(
      "Adventure", "Novels", "Love", "Science-Fiction & Fantasy"
    ),
    target_genre = c("Adventure", "Fiction", "Romance", "Science Fiction"),
    flag_for_review = c(FALSE, TRUE, FALSE, TRUE),
    note = "",
    stringsAsFactors = FALSE
  )

  novel_metadata <- data.frame(
    gutenberg_id = c(1L, 1L, 2L, 3L, 4L, 5L, 6L),
    genre = c(
      "Adventure", "Fiction", "Love Story", "Poetry",
      "Science Fiction", "Fiction", "Adventure"
    ),
    stringsAsFactors = FALSE
  )

  bookshelf_data <- data.frame(
    gutenberg_id = c(1L, 1L, 2L, 3L, 4L, 5L),
    title = c("One", "One", "Two", "Three", "Four", "Five"),
    Category = c(
      "Adventure", "Novels", "Love", "Adventure",
      "Science-Fiction & Fantasy", ""
    ),
    stringsAsFactors = FALSE
  )

  list(allowlist = allowlist, meta = novel_metadata, shelf = bookshelf_data)
}

test_that("books whose genre sets match exactly agree, including multi-genre books", {
  f <- fixture()
  result <- compareGenreAssignment(f$meta, f$shelf, f$allowlist)

  one <- result[result$gutenberg_id == 1L, ]
  expect_true(one$agrees)
  expect_equal(one$genres_only_in_stonybrook, "")
  expect_equal(one$genres_only_in_assigned, "")
})

test_that("the rename map applies to the trusted side, not the assigned side", {
  f <- fixture()
  result <- compareGenreAssignment(f$meta, f$shelf, f$allowlist)

  # "Love Story" is the shipped corpus's level name for what the allow-list
  # calls Romance. Without the rename this is a false disagreement.
  two <- result[result$gutenberg_id == 2L, ]
  expect_true(two$agrees)
  expect_equal(two$assigned_genres, "Romance")
})

test_that("disagreements name the genres on each side separately", {
  f <- fixture()
  result <- compareGenreAssignment(f$meta, f$shelf, f$allowlist)

  three <- result[result$gutenberg_id == 3L, ]
  expect_false(three$agrees)
  expect_equal(three$genres_only_in_stonybrook, "Poetry")
  expect_equal(three$genres_only_in_assigned, "Adventure")
})

test_that("allow_flagged = FALSE withdraws judgment-call mappings and changes agreement", {
  f <- fixture()
  with_flagged <- compareGenreAssignment(f$meta, f$shelf, f$allowlist)
  without_flagged <- compareGenreAssignment(
    f$meta, f$shelf, f$allowlist, allow_flagged = FALSE
  )

  four_with <- with_flagged[with_flagged$gutenberg_id == 4L, ]
  four_without <- without_flagged[without_flagged$gutenberg_id == 4L, ]
  expect_equal(four_with$assigned_genres, "Science Fiction")
  expect_equal(four_without$assigned_genres, "")
  expect_false(four_without$agrees)
})

test_that("a book with no mapped bookshelf entry is reported, not dropped", {
  f <- fixture()
  result <- compareGenreAssignment(f$meta, f$shelf, f$allowlist)

  five <- result[result$gutenberg_id == 5L, ]
  expect_equal(five$assigned_genres, "")
  expect_equal(five$genres_only_in_stonybrook, "Fiction")
})

test_that("books present on only one side are excluded by the inner join", {
  f <- fixture()
  result <- compareGenreAssignment(f$meta, f$shelf, f$allowlist)

  expect_equal(nrow(result), 5L)
  expect_false(6L %in% result$gutenberg_id)
})
