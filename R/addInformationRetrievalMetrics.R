#' Compute information retrieval metrics (e.g. accuracy, precision, recall...)
#'
#' @param data data frame with four fields (TP, TN, FP, and FN)
#' @seealso createConfusionMatrix
#' @importFrom dplyr %>%
#' @export
addInformationRetrievalMetrics <- function(data) {
  prettify <- function(x) { round(x * 100, digits = 2) }
  data %>% dplyr::mutate(
    Total = .data$TP + .data$TN + .data$FP + .data$FN,
    Accuracy = (.data$TP + .data$TN) / .data$Total,
    Precision = ifelse(.data$TP + .data$FP > 0,
                        .data$TP / (.data$TP + .data$FP), NA_real_),
    Recall = ifelse(.data$TP + .data$FN > 0,
                     .data$TP / (.data$TP + .data$FN), NA_real_),
    Specificity = ifelse(.data$TN + .data$FP > 0,
                          .data$TN / (.data$TN + .data$FP), NA_real_),
    # Guarded on Precision + Recall directly, not just NA-propagated from
    # Precision/Recall individually -- TP = 0 with FP > 0 and FN > 0 gives
    # a legitimate Precision = 0 and Recall = 0 (neither is NA), but their
    # sum is still 0, which would otherwise divide by zero here.
    F.Score = ifelse(!is.na(.data$Precision) & !is.na(.data$Recall) &
                        (.data$Precision + .data$Recall) > 0,
                      2 * ((.data$Precision * .data$Recall) / (.data$Precision + .data$Recall)),
                      NA_real_)
  ) %>% dplyr::mutate(
    Accuracy = prettify(.data$Accuracy),
    Precision = prettify(.data$Precision),
    Recall = prettify(.data$Recall),
    Specificity = prettify(.data$Specificity),
    F.Score = prettify(.data$F.Score)
  )
}
