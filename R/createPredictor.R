#' Return a closure that can predict the success of a novel
#' @param vectorizer a text2vec vectorizer
#' @param tfidf tfidf instance used to train model
#' @param model a glmnet model
#' @importFrom dplyr %>%
#' @export
# tfidf modified by fit_transform() call!
# apply pre-trained tf-idf transformation to test data
createPredictor <- function(vectorizer, tfidf, model) {
  results_to_text <- function(value) ifelse(value > .5, "success", "failure")
  function(data) {
    it <- getIterator(data$text, data$id)
    dtm = text2vec::create_dtm(it, vectorizer, progressbar = TRUE)
    dtm_tfidf = transform(dtm, tfidf)
    preds = predict(glmnet_classifier, dtm_test_tfidf, type = 'response')[,1]
    data %>% mutate(prediction = results_to_text(preds), score = round(preds * 100, digits = 2),
                    response = ifelse(response == 1, "success", "failure")) %>%
      arrange(desc(preds))
  }
}

