#' Title assignAuthorFolds
#'
#' The seeded permutation behind every author-grouped fold in this package:
#' authors, in the order given, get fold numbers from a shuffled repeating
#' 1..k. Because it is positional, the result depends on the number and order
#' of the authors, so it is only used to build the stored table once
#' (`inst/extdata/author_folds.csv`); see `data-raw/author-folds.R`.
#'
#' @param authors character vector of unique authors
#' @param k number of folds
#' @param seed random seed
#' @returns integer vector of folds, named by author
#' @noRd
assignAuthorFolds <- function(authors, k = 5, seed = 42) {
  set.seed(seed)
  folds <- sample(rep(1:k, length.out = length(authors)))
  names(folds) <- authors
  folds
}

#' Title authorFoldTable
#'
#' The stored form of an author-grouped fold scheme: one row per author, so
#' the assignment is data that any language can read, not a seed that only R
#' can reproduce.
#'
#' @param data a data frame with an `author` column
#' @param k number of folds
#' @param seed random seed
#' @param scheme name recorded in the table
#' @returns tibble with `author`, `scheme` and `fold`
#' @noRd
authorFoldTable <- function(data, k = 5, seed = 42, scheme = "author5_v1") {
  authors <- unique(data$author)
  tibble::tibble(
    author = authors,
    scheme = scheme,
    fold = unname(assignAuthorFolds(authors, k, seed))
  )
}
