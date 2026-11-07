#' Classify scored novels at a given threshold
#'
#' Applies a success/failure cutoff to the *unrounded* probability column
#' produced by createPredictor()'s closure -- not the rounded, display-only
#' score. Thresholding against score instead of probability can silently
#' misclassify right at the boundary: a probability of 0.500001 rounds to
#' a displayed score of 50.00, and 50.00/100 > 0.5 is FALSE even though
#' the real probability clears the threshold.
#'
#' Kept separate from createPredictor() so the same scored data can be
#' reclassified at any number of different thresholds without re-running
#' the model -- exploring the precision/recall tradeoff, for instance --
#' rather than needing a new closure per threshold.
#'
#' @param scored_data output of a createPredictor() closure call -- must
#'   have a `probability` column
#' @param threshold probability cutoff for a positive classification
#'   (default 0.5)
#' @returns scored_data with a `prediction` column (0/1) added
#' @seealso createPredictor
#' @export
classifyAt <- function(scored_data, threshold = 0.5) {
  stopifnot("probability" %in% names(scored_data))
  scored_data %>% mutate(
    prediction = ifelse(.data$probability > threshold, 1, 0)
  )
}
