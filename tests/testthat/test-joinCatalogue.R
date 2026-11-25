context("Joining the parsed metadata to the catalogue")

# Book 1 has two authors and is filed in two genres (the shape of 1184, The
# Count of Monte Cristo); book 2 has one of each; book 3 is not catalogued.
meta <- tibble::tibble(response = c(1L, 1L, 0L, 0L), download_count = c(50L, 50L, 3L, 1L),
                       genre = factor(c("Adventure", "Fiction", "Poetry", "Poetry")),
                       fold = c(2L, 4L, 1L, 3L), gutenberg_id = c(1L, 1L, 2L, 3L))
catalogue <- tibble::tibble(gutenberg_id = c(1L, 1L, 2L), title = c("A", "A", "B"),
                            author = c("Dumas", "Maquet", "Poe"),
                            gutenberg_author_id = c(492L, 3930L, 481L),
                            language = "en")

test_that("a two-author book in two genres gives one row per genre, without a warning", {
  out <- expect_silent(joinCatalogue(meta, catalogue))
  expect_equal(nrow(out), 3L)
  expect_identical(out$author[out$gutenberg_id == 1L], c("Dumas; Maquet", "Dumas; Maquet"))
  expect_identical(out$gutenberg_author_id[out$gutenberg_id == 1L][1], "492; 3930")
  expect_identical(as.character(out$genre[out$gutenberg_id == 1L]), c("Adventure", "Fiction"))
  expect_false(3L %in% out$gutenberg_id)
  expect_identical(names(out)[1:5], c("gutenberg_id", "genre", "fold", "author",
                                      "gutenberg_author_id"))
})
