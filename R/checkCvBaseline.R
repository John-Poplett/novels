#' Title checkCvBaseline
#'
#' Compares a cross-validation summary with a recorded baseline and warns for
#' every `alpha` whose mean test AUC moved by more than `n_se` standard errors
#' of the baseline. The standard error of a k-fold mean is the baseline's
#' fold-to-fold SD divided by sqrt(k), so the tolerance is wide where the
#' estimate is noisy and tight where it is not. It warns rather than fails,
#' because a legitimate change to the data or the model moves AUC.
#'
#' The check can catch gross regressions in the modelling or the pipeline. A
#' small data-integrity problem moves AUC by far less than the tolerance and
#' has to be caught by unit tests instead.
#'
#' @param cv_summary tibble with `alpha`, `mean_auc` and `sd_auc`
#' @param baseline same columns, as recorded in baselines/cv_summary.csv
#' @param k number of folds behind the baseline
#' @param n_se tolerance in standard errors (default 2)
#' @returns tibble with `alpha`, `baseline`, `new`, `se`, `delta` and `beyond`
#' @noRd
checkCvBaseline <- function(cv_summary, baseline, k, n_se = 2) {
  needed <- c("alpha", "mean_auc", "sd_auc")
  if (!all(needed %in% names(cv_summary)) || !all(needed %in% names(baseline))) {
    stop("cv_summary and baseline must both have columns: ",
         paste(needed, collapse = ", "), call. = FALSE)
  }

  m <- match(round(baseline$alpha, 8), round(cv_summary$alpha, 8))
  if (anyNA(m) || length(m) != nrow(cv_summary)) {
    stop("The alpha grid differs from the baseline, so the results are not ",
         "comparable. If the grid change is intended, replace baselines/ and ",
         "record why in CHANGELOG.md.", call. = FALSE)
  }

  std_err <- baseline$sd_auc / sqrt(k)
  out <- tibble::tibble(
    alpha = baseline$alpha,
    baseline = baseline$mean_auc,
    new = cv_summary$mean_auc[m],
    se = std_err
  )
  out$delta <- out$new - out$baseline
  out$beyond <- abs(out$delta) > n_se * out$se

  if (any(out$beyond)) {
    moved <- out[out$beyond, ]
    warning(
      "Mean test AUC moved more than ", n_se, " standard errors from the ",
      "baseline at alpha = ",
      paste(sprintf("%s (%+.3f, %.1f SE)", format(moved$alpha), moved$delta,
                    moved$delta / moved$se), collapse = ", "),
      ". If the change is expected, replace baselines/ and record why in ",
      "CHANGELOG.md.",
      call. = FALSE
    )
  }
  out
}
