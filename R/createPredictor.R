#' Return a closure that can predict the success of a novel
#' @param vectorizer a text2vec vectorizer
#' @param tfidf tfidf instance used to train model
#' @param model a glmnet model
#' @importFrom dplyr %>%
#' @export
# tfidf modified by fit_transform() call!
# apply pre-trained tf-idf transformation to test data
createPredictor <- function(vectorizer, tfidf, model) {
  function(data, titles, type = "class") {
    if (!'data.frame' %in% class(data)) {
      stopifnot(!missing(titles))
      if(class(data) == "character" && class(titles) == "character") {
        files = list(data)
        titles = list(titles)
      } else {
        files = data
      }
      data <- data.frame(
        "title" = titles,
        "text" = sapply(files, function(name) readr::read_file(as.character(name))))

    }
    it <- getIterator(data$text, data$title)
    dtm = text2vec::create_dtm(it, vectorizer, progressbar = TRUE)
    dtm_tfidf = transform(dtm, tfidf)
    predictions = glmnet::predict.cv.glmnet(model, as.matrix(dtm_tfidf), type = type)
    result <- data.frame(title = data$title, prediction = predictions)
    names(result) = c("title", "prediction")
    result
  }
}

