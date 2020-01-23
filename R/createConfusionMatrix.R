#' From stony-brook results, compute a confusion matrix
#'
#' This function returns a data frame with a confusion matrix for the given
#' predictions, having the fields "prediction" and "response".
#'
#' @keywords metrics, evaluation, confusion matrix
#' @param predictions data frame with fields "prediction" and "response"
#' @param threshold threshold value for success
#' @export
createConfusionMatrix <- function(predictions, threshold = .5) {
  results_to_text <- function(value) ifelse(value > threshold, "success", "failure")
  predictions <- predictions %>% mutate(
    response = results_to_text(.data$response),
    prediction = results_to_text(.data$prediction)
  )
  as_tibble(data.frame(
    TP = c(sum(predictions$prediction == "success" & predictions$response == "success")),
    FP = c(sum(predictions$prediction == "success" & predictions$response == "failure")),
    TN = c(sum(predictions$prediction == "failure" & predictions$response == "failure")),
    FN = c(sum(predictions$prediction == "failure" & predictions$response == "success"))))
}
