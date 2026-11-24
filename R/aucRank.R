#' Area under the ROC curve, by ranks
#'
#' The Mann-Whitney form: the probability that a random success scores above
#' a random failure, ties counting one half (`rank()`'s average ranks). It
#' equals `pROC::auc()` and needs no ROC object, so it suits the analysis
#' scripts' bootstraps and simulations, which call it many thousands of
#' times. A score with the classes reversed gives `1 - AUC`, not an error.
#'
#' One copy for every script (review/003 finding 9): it had been copied into
#' four, and two of the copies divided by zero for a sample with one class.
#'
#' @param truth the labels: 0/1 numbers or a logical, no `NA`
#' @param score the scores, higher meaning more likely 1, no `NA`
#' @returns a single number in \[0, 1\], or `NA` when `truth` holds only
#'   one class (a resample can), so a quantile over draws can drop it with
#'   `na.rm` rather than be poisoned by `NaN`
#' @noRd
aucRank <- function(truth, score) {
  if (!(is.numeric(truth) || is.logical(truth)) || anyNA(truth) ||
      !all(truth %in% c(0, 1))) {
    stop("truth must be 0/1 or logical, without NA.", call. = FALSE)
  }
  if (!is.numeric(score) || anyNA(score) || length(score) != length(truth)) {
    stop("score must be numeric, without NA, one per element of truth (",
         length(truth), "); it has ", length(score), ".", call. = FALSE)
  }
  n1 <- sum(truth == 1)
  n0 <- sum(truth == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(score)
  (sum(r[truth == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
