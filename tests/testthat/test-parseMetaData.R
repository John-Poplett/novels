context("Metadata parsing")

# A minimal corpus that satisfies parseMetaData()'s header census: 8 DATASET
# headers and 5 FOLD headers each. Record ids are 1001..1040 and every id has
# a matching .txt file, because parseMetaData() checks metadata against the
# directory tree.
make_corpus_fixture <- function() {
  dir <- tempfile("corpus-")
  dir.create(dir)

  genres <- c("Love Story", "Sci-Fi", "Adventure", "Fiction",
              "Historical Fiction", "Mystery", "Poetry", "Short Stories")
  lines <- character()
  ids <- integer()
  id <- 1000L

  for (g in genres) {
    lines <- c(lines,
               paste0("******************DATASET: ", g, " ***********************************"),
               "")
    for (f in 1:5) {
      id <- id + 1L
      ids <- c(ids, id)
      response <- if (id %% 2L == 0L) "SUCCESS" else "FAILURE"
      lines <- c(
        lines,
        paste0("****************** FOLD", f, " ***********************************"),
        sprintf(
          '%s: FileName: %d.txt ,Title: title %d ,Author: "author, a" ,Language: en ,DownloadCount: %d',
          response, id, id, id
        )
      )
    }
  }

  writeLines(lines, file.path(dir, "novel_meta.txt"))
  invisible(file.create(file.path(dir, paste0(ids, ".txt"))))
  dir
}

test_that("parseMetaData returns the documented grain and types", {
  dir <- make_corpus_fixture()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  parsed <- parseMetaData(dir)
  meta <- parsed$data

  expect_named(parsed, c("data", "census"))
  expect_s3_class(meta, "tbl_df")
  expect_equal(nrow(meta), 40L)
  expect_named(meta, c("response", "download_count", "genre", "fold",
                       "gutenberg_id"))
  expect_type(meta$gutenberg_id, "integer")
  expect_type(meta$fold, "integer")
  expect_type(meta$response, "integer")
  expect_type(meta$download_count, "integer")
  expect_s3_class(meta$genre, "factor")
})

test_that("parseMetaData maps SUCCESS/FAILURE to 1/0 and carries fold down", {
  dir <- make_corpus_fixture()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  meta <- parseMetaData(dir)$data

  expect_equal(meta$response, as.integer(meta$gutenberg_id %% 2L == 0L))
  # The first five records are the first genre's five folds.
  expect_equal(meta$fold[1:5], 1:5)
  expect_equal(meta$download_count, meta$gutenberg_id)
})

test_that("parseMetaData applies the dataset's genre renames", {
  dir <- make_corpus_fixture()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  meta <- parseMetaData(dir)$data

  expect_true(all(c("Romance", "Science Fiction") %in% levels(meta$genre)))
  expect_false(any(c("Love Story", "Sci-Fi") %in% levels(meta$genre)))
})

test_that("parseMetaData's census counts blocks, parseable rows and files", {
  dir <- make_corpus_fixture()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  census <- parseMetaData(dir)$census

  expect_equal(census$n_blocks, 40L)
  expect_equal(census$n_parseable, 40L)
  expect_equal(census$n_files, 40L)
  expect_length(census$truncated, 0L)
  expect_length(census$absent, 0L)
  expect_s3_class(census$ts, "POSIXct")
})

test_that("incomplete record lines are dropped, not partially parsed", {
  dir <- make_corpus_fixture()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  incomplete <- "SUCCESS: FileName: 9999.txt ,Title: incomplete record"
  cat("\n", incomplete, "\n",
      file = file.path(dir, "novel_meta.txt"), append = TRUE, sep = "")

  parsed <- parseMetaData(dir)

  expect_equal(nrow(parsed$data), 40L)
  expect_false(9999L %in% parsed$data$gutenberg_id)
  expect_equal(parsed$census$n_blocks, 41L)
  expect_equal(parsed$census$truncated, incomplete)
})

test_that("a metadata row with no file on disk is an error", {
  dir <- make_corpus_fixture()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  unlink(file.path(dir, "1001.txt"))

  expect_error(parseMetaData(dir), "file conservation failed", fixed = TRUE)
})

test_that("a metadata file with the wrong header census is rejected", {
  dir <- make_corpus_fixture()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  lines <- readLines(file.path(dir, "novel_meta.txt"))
  lines <- lines[!grepl("DATASET: Poetry", lines, fixed = TRUE)]
  writeLines(lines, file.path(dir, "novel_meta.txt"))

  expect_error(parseMetaData(dir))
})
