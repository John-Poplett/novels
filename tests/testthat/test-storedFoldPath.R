context("Stored fold table resolution and fail-loud behaviour")

# The stored table is the source of truth for author folds. These tests use
# options(novels.folds_path = ...) to point at modified copies, which proves
# the code reads the table instead of recomputing the seed -- and keeps the
# test independent of pkgload's system.file() shim.

stored_table <- function() {
  readr::read_csv(storedFoldPath(), show_col_types = FALSE)
}

test_that("storedFoldPath() finds the table in the source tree", {
  path <- storedFoldPath()

  expect_true(file.exists(path))
  expect_identical(basename(path), "author_folds.csv")
})

test_that("use_stored_folds follows the table rather than the seed", {
  table <- stored_table()
  folds <- sort(unique(table$fold))
  a <- table$author[table$fold == folds[1]][[1]]
  b <- table$author[table$fold == folds[2]][[1]]

  # Swap two authors' folds: the seed cannot produce this assignment.
  swapped <- table
  swapped$fold[swapped$author == a] <- folds[2]
  swapped$fold[swapped$author == b] <- folds[1]

  path <- tempfile(fileext = ".csv")
  readr::write_csv(swapped, path)
  old <- options(novels.folds_path = path)
  on.exit(options(old), add = TRUE)

  splits <- createKFoldSplit(novels, k = 5, use_stored_folds = TRUE)
  test_authors <- lapply(splits, function(s) unique(novels$author[s$test]))

  expect_true(a %in% test_authors[[folds[2]]])
  expect_false(a %in% test_authors[[folds[1]]])
  expect_true(b %in% test_authors[[folds[1]]])
})

test_that("a missing stored table is an error, not a silent fallback", {
  missing <- tempfile(fileext = ".csv")
  old <- options(novels.folds_path = missing)
  on.exit(options(old), add = TRUE)

  expect_error(storedFoldPath(), "not found", fixed = TRUE)
  expect_error(
    createKFoldSplit(novels, k = 5, use_stored_folds = TRUE),
    "not found",
    fixed = TRUE
  )
})

test_that("authors missing from the table are named in the error", {
  table <- stored_table()
  dropped <- table$author[[1]]

  path <- tempfile(fileext = ".csv")
  readr::write_csv(table[table$author != dropped, ], path)
  old <- options(novels.folds_path = path)
  on.exit(options(old), add = TRUE)

  expect_error(
    createKFoldSplit(novels, k = 5, use_stored_folds = TRUE),
    dropped,
    fixed = TRUE
  )
})

test_that("duplicate authors in the table are rejected", {
  table <- stored_table()

  path <- tempfile(fileext = ".csv")
  readr::write_csv(rbind(table, table[1, ]), path)
  old <- options(novels.folds_path = path)
  on.exit(options(old), add = TRUE)

  expect_error(
    createKFoldSplit(novels, k = 5, use_stored_folds = TRUE),
    "duplicate",
    fixed = TRUE
  )
})
