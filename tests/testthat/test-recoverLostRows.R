context("Recovering the rows the 2013 metadata lost")

# A two-genre archive: one parsed book in each folder, and an unparsed file
# (2002) beside the Short Stories one.
make_archive <- function() {
  dir <- tempfile("archive-")
  for (p in c("Short_Stories/ss_fold5/failure5", "Poetry/po_fold2/success2")) {
    dir.create(file.path(dir, p), recursive = TRUE)
  }
  file.create(file.path(dir, c("Short_Stories/ss_fold5/failure5/1001.txt",
                               "Short_Stories/ss_fold5/failure5/2002.txt",
                               "Poetry/po_fold2/success2/1002.txt")))
  dir
}
meta <- tibble::tibble(response = c(0L, 1L), download_count = c(5L, 80L),
                       genre = factor(c("Short Stories", "Poetry")),
                       fold = c(5L, 2L), gutenberg_id = c(1001L, 1002L))
row <- function(...) {
  r <- data.frame(gutenberg_id = 2002L, genre = "Short Stories", fold = 5L, response = 0L,
                  download_count = 4L, path = "Short_Stories/ss_fold5/failure5/2002.txt")
  utils::modifyList(r, list(...))
}

test_that("a row that matches its folder is appended with the parser's types", {
  dir <- make_archive(); on.exit(unlink(dir, recursive = TRUE))
  out <- recoverLostRows(meta, dir, row())
  expect_equal(nrow(out), 3L)
  expect_named(out, names(meta))
  expect_identical(out$gutenberg_id[3], 2002L)
  expect_identical(as.character(out$genre[3]), "Short Stories")
  expect_identical(levels(out$genre), levels(meta$genre))
  expect_type(out$download_count, "integer")
})

test_that("an empty table changes nothing", {
  dir <- make_archive(); on.exit(unlink(dir, recursive = TRUE))
  expect_identical(recoverLostRows(meta, dir, row()[0, ]), meta)
})

test_that("a row that disagrees with the archive is refused", {
  dir <- make_archive(); on.exit(unlink(dir, recursive = TRUE))
  expect_error(recoverLostRows(meta, dir, row(gutenberg_id = 1001L)), "already has it")
  expect_error(recoverLostRows(meta, dir, row(path = "Short_Stories/ss_fold5/failure5/9.txt")),
               "no file")
  expect_error(recoverLostRows(meta, dir, row(response = 1L)), "folder is failure5")
  expect_error(recoverLostRows(meta, dir, row(fold = 4L)), "folder is failure5")
  expect_error(recoverLostRows(meta, dir, row(genre = "Poetry")), "books in Short_Stories share")
  expect_error(recoverLostRows(meta, dir, row()[, -2]), "lacks column")
})

test_that("the shipped table reads, and recovers 23177 and 23178 as Short Stories failures", {
  rec <- readRecoveredRows()
  expect_setequal(rec$gutenberg_id, c(23177L, 23178L))
  expect_true(all(rec$genre == "Short Stories" & rec$response == 0L & rec$fold == 5L))
  expect_false(anyNA(rec$download_count))
})

test_that("a book filed in two genres does not change its folder's genre", {
  dir <- make_archive(); on.exit(unlink(dir, recursive = TRUE))
  # 1002 is Poetry and also Short Stories, so it has a copy in each folder.
  file.copy(file.path(dir, "Poetry/po_fold2/success2/1002.txt"),
            file.path(dir, "Short_Stories/ss_fold5/failure5/1002.txt"))
  both <- dplyr::bind_rows(meta, tibble::tibble(response = 1L, download_count = 80L,
    genre = factor("Short Stories", levels = levels(meta$genre)), fold = 5L,
    gutenberg_id = 1002L))
  expect_equal(nrow(recoverLostRows(both, dir, row())), 4L)
  expect_error(recoverLostRows(both, dir, row(genre = "Poetry")), "share the genre Short Stories")
})
