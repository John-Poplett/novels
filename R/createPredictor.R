#' Return a closure that can predict the success of a novel
#' @param vectorizer a text2vec vectorizer
#' @param tfidf tfidf instance used to train model
#' @param model a glmnet model
#' @param threshold specify the threshold for success
#' @importFrom dplyr %>%
#' @export
# tfidf modified by fit_transform() call!
# apply pre-trained tf-idf transformation to test data
createPredictor <- function(vectorizer, tfidf, model, threshold = .5) {
  function(data) {
    it <- getIterator(data$text, data$id)
    dtm_tfidf  = text2vec::create_dtm(it, vectorizer) %>%
      transform(tfidf)
    prediction = predict(model, dtm_tfidf, type = 'response')[,1]
    data %>% mutate(
      score = round(prediction * 100, digits = 2),
      prediction = ifelse(prediction > threshold, 1, 0)
    ) %>% arrange(desc(.data$score))
  }
}

