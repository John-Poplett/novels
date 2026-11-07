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

# Regression: 37 books in the corpus retained an unstarred footer line that
# sits one line ABOVE the starred end marker, so cutting at the starred marker
# kept it. The kept line named the title and the author (id 2727).
test_that("drops the unstarred footer that precedes the end marker", {
  text <- paste(
    "*** START OF THIS PROJECT GUTENBERG EBOOK ALLAN'S WIFE ***",
    "chapter one",
    "chapter two",
    "end of the project gutenberg ebook of allan's wife, by h. rider haggard",
    "*** END OF THIS PROJECT GUTENBERG EBOOK ALLAN'S WIFE ***",
    sep = "\n"
  )

  expect_identical(stripGutenbergBoilerplate(text), "chapter one\nchapter two")
})

# Regression: the production credit can follow the start marker rather than
# precede it (id 2883: marker on line 12, "produced by ..." on line 13), which
# put a Gutenberg line at the head of the kept body.
test_that("drops a production credit that follows the start marker", {
  text <- paste(
    "*** START OF THIS PROJECT GUTENBERG EBOOK THE GOLD BAG ***",
    "produced by an anonymous project gutenberg volunteer",
    "the gold bag",
    "by carolyn wells",
    "*** END OF THIS PROJECT GUTENBERG EBOOK THE GOLD BAG ***",
    sep = "\n"
  )

  expect_identical(
    stripGutenbergBoilerplate(text),
    "the gold bag\nby carolyn wells"
  )
})

# Regression: a third footer shape, possessive and without "ebook" --
# "end of project gutenberg's <title>, by <author>" -- carried the author's
# name at the end of 11 books.
test_that("drops the possessive footer used by older files", {
  text <- paste(
    "*** START OF THIS PROJECT GUTENBERG EBOOK X ***",
    "chapter one",
    "end of project gutenberg's x, by a. author",
    sep = "\n"
  )

  expect_identical(stripGutenbergBoilerplate(text), "chapter one")
})

test_that("drops a header line when a text has no markers at all", {
  text <- paste(
    "the project gutenberg ebook of the gold bag, by carolyn wells",
    "produced by an anonymous volunteer",
    "chapter one",
    sep = "\n"
  )

  expect_identical(stripGutenbergBoilerplate(text), "chapter one")
})

# The fix must not truncate on a mention inside the body: two corpus books
# cite Project Gutenberg in an editorial note, mid-sentence or mid-line.
test_that("keeps an incidental Project Gutenberg mention in the body", {
  body <- c(
    "chapter one",
    "the most reprinted version, it is entered into project gutenberg",
    "chapter two"
  )
  text <- paste(
    c("*** START OF THIS PROJECT GUTENBERG EBOOK X ***", body,
      "*** END OF THIS PROJECT GUTENBERG EBOOK X ***"),
    collapse = "\n"
  )

  expect_identical(stripGutenbergBoilerplate(text), paste(body, collapse = "\n"))
})

# Regression against over-stripping: "produced by" is also ordinary English and
# opens real sentences (ids 13720, 19513, 2275), so the credit strip is bounded
# to the header region rather than matching the phrase anywhere.
test_that("keeps a sentence that happens to begin with 'produced by'", {
  body <- c(
    "chapter one",
    "the river was",
    "produced by the noise of falling waters, the current would increase"
  )
  text <- paste(
    c("*** START OF THIS PROJECT GUTENBERG EBOOK X ***", body,
      "*** END OF THIS PROJECT GUTENBERG EBOOK X ***"),
    collapse = "\n"
  )

  expect_identical(stripGutenbergBoilerplate(text), paste(body, collapse = "\n"))
})

