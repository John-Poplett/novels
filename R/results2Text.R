#' Convert numeric response to text value
#' @param response a value between 0 and 1
#' @param threshold specify the threshold for success
#' @export
results2Text <- function(response, threshold = .5) {
  ifelse(response > threshold, "success", "failure")
}
