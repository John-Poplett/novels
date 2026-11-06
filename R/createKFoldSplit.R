#' Title createKFoldSplit
#'
#' Generalizes createSplit() from a single 80/20 split into K author-safe
#' folds, for repeated cross-validation rather than one train/test split.
#' Splitting by author (not by row) means no author's books span more than
#' one fold, the same safeguard createSplit() uses.
#'
#' @param data a data frame of literary texts
#' @param k number of folds (default 5)
#' @param seed random seed for reproducibility (ignored when use_stored_folds = TRUE)
#' @param use_stored_folds read fold assignments from `inst/extdata/author_folds.csv`
#'   instead of recomputing them from `seed`. Errors if the table is missing or
#'   does not cover every author: a silent fallback would reshuffle authors
#'   whenever the dataset grows.
#' @returns a list of k elements, each a list with integer vectors `train`
#'   and `test` -- the same shape as createSplit()'s single-split return
#'   value, so existing per-split code can be reused unchanged per fold
#' @export
createKFoldSplit <- function(data, k = 5, seed = 42, use_stored_folds = FALSE) {
  stopifnot('data.frame' %in% class(data) && 'author' %in% names(data))

  if (use_stored_folds) {
    stored_path <- storedFoldPath()
    stored_folds <- readr::read_csv(stored_path, show_col_types = FALSE)

    if (anyDuplicated(stored_folds$author)) {
      stop(
        "Stored fold table has duplicate authors: ", stored_path, ". ",
        "Each author must appear exactly once; remove the duplicate rows ",
        "before running. Fold assignments are append-only.",
        call. = FALSE
      )
    }

    dataset_authors <- unique(data$author)
    matched_folds <- stored_folds$fold[match(dataset_authors, stored_folds$author)]

    missing <- dataset_authors[is.na(matched_folds)]
    if (length(missing) > 0) {
      shown <- paste(utils::head(missing, 10), collapse = ", ")
      if (length(missing) > 10) {
        shown <- paste0(shown, ", ... (", length(missing), " total)")
      }
      stop(
        "Stored fold table does not cover ", length(missing), " author(s): ",
        shown, ". ",
        "Add rows for these authors to inst/extdata/author_folds.csv without ",
        "changing existing rows -- the table is append-only, and regenerating ",
        "it would move most existing authors.",
        call. = FALSE
      )
    }

    names(matched_folds) <- dataset_authors
  } else {
    # Original behavior - generate folds using the seed approach
    all_authors <- unique(data$author)
    matched_folds <- assignAuthorFolds(all_authors, k, seed)
  }

  lapply(seq_len(k), function(i) {
    test_authors <- names(matched_folds[matched_folds == i])
    train_authors <- names(matched_folds[matched_folds != i])
    list(
      train = which(data$author %in% train_authors),
      test = which(data$author %in% test_authors)
    )
  })
}

#' Locate the stored author-fold table
#'
#' `system.file()` returns "" when the package is not installed, which is the
#' case when `targets` runs `tar_source("R")` from the source tree. Without a
#' source-tree fallback the pipeline would silently recompute folds from the
#' seed -- the bug this helper exists to prevent.
#'
#' Resolution order:
#' 1. `getOption("novels.folds_path")`, an explicit override. If set, it must
#'    exist; the function does not fall through to the other locations.
#' 2. `system.file("extdata", "author_folds.csv", package = "novels")`, which
#'    covers an installed package and `pkgload::load_all()`.
#' 3. `inst/extdata/author_folds.csv` under the working directory or any
#'    parent, which covers the source tree.
#'
#' @returns a length-one character path that exists
#' @noRd
storedFoldPath <- function() {
  override <- getOption("novels.folds_path")
  if (!is.null(override)) {
    if (!file.exists(override)) {
      stop(
        "Stored fold table not found at ", override, " ",
        "(set by options(novels.folds_path)). ",
        "Point the option at inst/extdata/author_folds.csv, or unset it to ",
        "search the installed package and source tree.",
        call. = FALSE
      )
    }
    return(override)
  }

  installed <- system.file("extdata", "author_folds.csv", package = "novels")
  if (nzchar(installed) && file.exists(installed)) {
    return(installed)
  }

  in_tree <- findInSourceTree(file.path("inst", "extdata", "author_folds.csv"))
  if (nzchar(in_tree)) {
    return(in_tree)
  }

  stop(
    "Stored fold table not found. ",
    "use_stored_folds = TRUE needs inst/extdata/author_folds.csv. ",
    "Run from the package root (or install the package) and try again. ",
    "Do not regenerate the table to fix this: it is append-only, and a new ",
    "seed would move most existing authors.",
    call. = FALSE
  )
}
