test_that("mirrorDir follows the mirror's digit layout", {
  expect_equal(mirrorDir(215), "2/1/215")
  expect_equal(mirrorDir(1234), "1/2/3/1234")
  expect_equal(mirrorDir(76), "7/76")
  expect_equal(mirrorDir(5), "0/5")
  expect_equal(mirrorDir(10), "1/10")
  expect_equal(mirrorDir(100000L), "1/0/0/0/0/100000")
})

test_that("mirrorDir refuses anything but one positive whole number", {
  expect_error(mirrorDir(0), "positive whole number")
  expect_error(mirrorDir(2.5), "positive whole number")
  expect_error(mirrorDir(c(1, 2)), "positive whole number")
  expect_error(mirrorDir(NA_real_), "positive whole number")
  expect_error(mirrorDir("215"), "positive whole number")
})

test_that("listingEntries keeps files and directories, drops parent and absolute links", {
  html <- c('<a href="../">../</a>', '<a href="215-h/">215-h/</a>',
            '<a href="old/">old/</a> <a href="215-0.txt">215-0.txt</a>',
            '<a href="?C=M;O=A">sort</a>', '<a href="https://example.org/">x</a>',
            '<a href="/icons/blank.gif">i</a>', '<a href="215-0.txt">again</a>')
  expect_equal(listingEntries(html), c("215-h/", "old/", "215-0.txt"))
  expect_equal(listingEntries(character()), character())
})

test_that("pickTextFile prefers UTF-8, then 8-bit, then ASCII", {
  expect_equal(pickTextFile(c("215.txt", "215-0.txt", "215-8.txt"), 215)$file, "215-0.txt")
  expect_equal(pickTextFile(c("215.txt", "215-8.txt"), 215)$encoding, "latin1")
  expect_equal(pickTextFile(c("old/", "215.txt"), 215)$encoding, "ASCII")
})

test_that("pickTextFile does not take another book's file or a zip", {
  expect_error(pickTextFile(c("2150.txt", "215.zip", "old/"), 215), "no plain-text file")
})

test_that("refetchIds maps patched rows to their replacement texts", {
  ids <- refetchIds(c(215L, 19513L, 215L, 76L), replacements = c("19513" = 18857L))
  expect_equal(ids$gutenberg_id, c(76L, 215L, 19513L))
  expect_equal(ids$source_id, c(76L, 215L, 18857L))
  expect_error(refetchIds(integer()), "Gutenberg ids")
  expect_error(refetchIds(c(1L, NA)), "Gutenberg ids")
})
