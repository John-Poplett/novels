library(dplyr)
#' Compute information retrieval metrics (e.g. accuracy, precision, recall...)
#'
#' @param data data frame with four fields (TP, TN, FP, and FN)
#' @seealso createConfusionMatrix
#' @export
addInformationRetrievalMetrics <- function(data) {
  prettify <- function(x) { round(x * 100, digits = 2) }
  data %>% mutate(
    Total = .data$TP + .data$TN + .data$FP + .data$FN,
    Accuracy = (.data$TP + .data$TN) / .data$Total,
    Precision = .data$TP / (.data$TP + .data$FP),
    Recall = .data$TP / (.data$TP + .data$FN),
    Specificity = .data$TN / (.data$TN + .data$FP),
    F.Score = 2 * ((.data$Precision * .data$Recall) / (.data$Precision + .data$Recall))
  ) %>% mutate(
    Accuracy = prettify(.data$Accuracy),
    Precision = prettify(.data$Precision),
    Recall = prettify(.data$Recall),
    Specificity = prettify(.data$Specificity),
    F.Score = prettify(.data$F.Score)
  )
}
