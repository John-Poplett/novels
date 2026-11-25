context("The patched LibriVox rows' counts")

corpus <- data.frame(
  gutenberg_id = c(1L, 2L, 3L, 4L, 9L),
  genre = factor(c("Poetry", "Poetry", "Poetry", "Poetry", "Poetry")),
  response = c(0L, 0L, 1L, 1L, 0L),
  download_count = c(3L, 9L, 70L, 400L, 5L)
)
counts <- function(n, response) {
  data.frame(gutenberg_id = 9L, download_count = n, response = response)
}

test_that("a count in the success band makes the row a success", {
  out <- applyLibrivoxCounts(corpus, counts(78L, 1L))
  expect_identical(out$download_count[5], 78L)
  expect_identical(out$response[5], 1L)
  expect_identical(out[1:4, ], corpus[1:4, ])
})

test_that("the band edges count, and a failure stays a failure", {
  expect_identical(applyLibrivoxCounts(corpus, counts(70L, 1L))$response[5], 1L)
  expect_identical(applyLibrivoxCounts(corpus, counts(9L, 0L))$response[5], 0L)
})

test_that("a count between the bands, a wrong label or a missing row is refused", {
  expect_error(applyLibrivoxCounts(corpus, counts(40L, 1L)), "between Poetry's")
  expect_error(applyLibrivoxCounts(corpus, counts(78L, 0L)), "success in Poetry")
  expect_error(applyLibrivoxCounts(corpus, transform(counts(78L, 1L), gutenberg_id = 99L)),
               "no such row")
  expect_error(applyLibrivoxCounts(corpus, counts(78L, 1L)[, -3]), "lacks column")
})

test_that("the shipped table covers exactly the patched rows", {
  tab <- readLibrivoxCounts()
  expect_setequal(tab$gutenberg_id, as.integer(names(librivox_replacements)))
  expect_identical(tab$source_id[match(names(librivox_replacements), tab$gutenberg_id)],
                   unname(librivox_replacements))
  expect_true(all(tab$response == 1L))
})
