#' Title createKFoldSplit
#'
#' Generalizes createSplit() from a single 80/20 split into K author-safe
#' folds, for repeated cross-validation rather than one train/test split.
#' Splitting by author (not by row) means no author's books span more than
#' one fold, the same safeguard createSplit() uses.
#'
#' @param data a data frame of literary texts
#' @param k number of folds (default 5)
#' @param seed random seed for reproducibility
#' @returns a list of k elements, each a list with integer vectors `train`
#'   and `test` -- the same shape as createSplit()'s single-split return
#'   value, so existing per-split code can be reused unchanged per fold
#' @export
createKFoldSplit <- function(data, k = 5, seed = 42) {
  stopifnot('data.frame' %in% class(data) && 'author' %in% names(data))
  set.seed(seed)
  all_authors <- unique(data$author)
  fold_assignment <- sample(rep(1:k, length.out = length(all_authors)))
  names(fold_assignment) <- all_authors

  lapply(seq_len(k), function(i) {
    test_authors <- all_authors[fold_assignment == i]
    train_authors <- all_authors[fold_assignment != i]
    list(
      train = which(data$author %in% train_authors),
      test = which(data$author %in% test_authors)
    )
  })
}
