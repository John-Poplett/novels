#' Return a closure that scores novels using a pre-trained model
#'
#' The returned closure only computes and attaches a score -- it makes no
#' success/failure classification decision. That decision depends on a
#' threshold, which is a real modeling choice (see the README's discussion
#' of quantifying "success") and shouldn't be silently baked into a
#' closure's captured state at creation time. Use classifyAt() on the
#' result to apply a threshold, at any value, without rebuilding the
#' closure or re-running the model.
#'
#' @param vectorizer a text2vec vectorizer
#' @param tfidf tfidf instance used to train model
#' @param model a glmnet model
#' @importFrom dplyr %>%
#' @seealso classifyAt
#' @export
# tfidf modified by fit_transform() call!
# apply pre-trained tf-idf transformation to test data
createPredictor <- function(vectorizer, tfidf, model) {
  function(data) {
    it <- getIterator(data$text, data$title)
    dtm_tfidf <- text2vec::create_dtm(it, vectorizer) %>%
      transform(tfidf)
    probability <- stats::predict(model, dtm_tfidf, type = 'response')[, 1]
    data %>% mutate(
      probability = probability,
      score = round(probability * 100, digits = 2)
    ) %>% arrange(desc(.data$score))
  }
}
