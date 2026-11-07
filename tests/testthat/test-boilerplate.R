context("Gutenberg boilerplate stripping")

test_that("strips text outside the start/end markers", {
  text <- paste(
    "header junk",
    "*** START OF THIS PROJECT GUTENBERG EBOOK TEST ***",
    "chapter one",
    "chapter two",
    "*** END OF THIS PROJECT GUTENBERG EBOOK TEST ***",
    "footer junk",
    sep = "\n"
  )

  expect_identical(stripGutenbergBoilerplate(text), "chapter one\nchapter two")
})

test_that("leaves text without markers unchanged", {
  text <- "chapter one\nchapter two"

  expect_identical(stripGutenbergBoilerplate(text), text)
})

test_that("uses the last start marker and the first end marker", {
  text <- paste(
    "*** START OF THIS PROJECT GUTENBERG EBOOK A ***",
    "front matter",
    "*** START OF THIS PROJECT GUTENBERG EBOOK B ***",
    "body",
    "*** END OF THIS PROJECT GUTENBERG EBOOK B ***",
    "*** END OF THIS PROJECT GUTENBERG EBOOK A ***",
    sep = "\n"
  )

  expect_identical(stripGutenbergBoilerplate(text), "body")
})

test_that("matches the markers case-insensitively", {
  text <- paste(
    "*** start of this project gutenberg ebook x ***",
    "body",
    "*** end of this project gutenberg ebook x ***",
    sep = "\n"
  )

  expect_identical(stripGutenbergBoilerplate(text), "body")
})

test_that("an end marker before the start marker yields an empty string", {
  text <- paste(
    "*** END OF THIS PROJECT GUTENBERG EBOOK X ***",
    "body",
    "*** START OF THIS PROJECT GUTENBERG EBOOK X ***",
    sep = "\n"
  )

  expect_identical(stripGutenbergBoilerplate(text), "")
})

test_that("a text consisting only of markers yields an empty string", {
  text <- paste(
    "*** START OF THIS PROJECT GUTENBERG EBOOK X ***",
    "*** END OF THIS PROJECT GUTENBERG EBOOK X ***",
    sep = "\n"
  )

  expect_identical(stripGutenbergBoilerplate(text), "")
})
