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

test_that("the stored folds reproduce createKFoldSplit() on the dataset they were built from", {
  # The table was generated from the corpus before any row was recovered;
  # appending authors since must not have moved any of its rows.
  folds <- stored_folds()
  original <- novels::novels[!novels::novels$gutenberg_id %in% readRecoveredRows()$gutenberg_id, ]
  split <- createKFoldSplit(original, 5)
  stored_fold_of_row <- folds$fold[match(original$author, folds$author)]

  for (k in 1:5) {
    expect_setequal(which(stored_fold_of_row == k), split[[k]]$test)
  }
})

test_that("authors appended for recovered rows follow the balance rule", {
  # inst/extdata/README.md: in gutenberg_id order, each new author goes to
  # the fold with the fewest corpus rows at that moment, ties to the lower.
  folds <- stored_folds()
  ids <- sort(readRecoveredRows()$gutenberg_id)
  shipped <- novels::novels
  current <- shipped[!shipped$gutenberg_id %in% ids, ]
  rows_per_fold <- tabulate(folds$fold[match(current$author, folds$author)], nbins = 5)
  for (id in ids) {
    added <- shipped[shipped$gutenberg_id == id, ]
    author <- unique(added$author)
    expect_length(author, 1L)
    expect_false(author %in% current$author)
    expected <- which.min(rows_per_fold)
    expect_identical(as.integer(folds$fold[folds$author == author]), expected)
    rows_per_fold[expected] <- rows_per_fold[expected] + nrow(added)
    current <- rbind(current, added)
  }
})

test_that("assignAuthorFolds is deterministic for a seed", {
  authors <- paste0("author_", 1:20)

  expect_identical(novels:::assignAuthorFolds(authors), novels:::assignAuthorFolds(authors))
  expect_named(novels:::assignAuthorFolds(authors), authors)
})
