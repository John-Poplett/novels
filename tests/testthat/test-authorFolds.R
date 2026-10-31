context('The stored author-fold table is well-formed and reproduces the pipeline folds')

# inst/extdata/author_folds.csv is the author-grouped fold assignment stored as
# data, so any language can read it (R's seeded sample() cannot be reproduced
# elsewhere). It is append-only: see data-raw/author-folds.R.

stored_folds <- function() {
  path <- system.file("extdata", "author_folds.csv", package = "novels")
  readr::read_csv(path, show_col_types = FALSE)
}

test_that("the table has one row per author, one scheme and folds 1 to 5", {
  folds <- stored_folds()

  expect_named(folds, c("author", "scheme", "fold"))
  expect_false(anyDuplicated(folds$author) > 0)
  expect_identical(unique(folds$scheme), "author5_v1")
  expect_setequal(folds$fold, 1:5)
})

test_that("the folds are balanced to within one author", {
  sizes <- table(stored_folds()$fold)

  expect_lte(max(sizes) - min(sizes), 1)
})

test_that("every author in the dataset has a stored fold, and no others are stored", {
  # Fails when the dataset gains authors that have not been appended to the
  # table, which is the guard against growing one without the other.
  expect_setequal(stored_folds()$author, unique(novels::novels$author))
})

test_that("the stored folds reproduce createKFoldSplit() on the shipped dataset", {
  folds <- stored_folds()
  split <- createKFoldSplit(novels::novels, 5)
  stored_fold_of_row <- folds$fold[match(novels::novels$author, folds$author)]

  for (k in 1:5) {
    expect_setequal(which(stored_fold_of_row == k), split[[k]]$test)
  }
})

test_that("assignAuthorFolds is deterministic for a seed", {
  authors <- paste0("author_", 1:20)

  expect_identical(novels:::assignAuthorFolds(authors), novels:::assignAuthorFolds(authors))
  expect_named(novels:::assignAuthorFolds(authors), authors)
})
