#' Convert a vector of texts into a vocabulary object
#' @param it a text2vec token iterator
#' @param documentCount count of documents in dataset
#' @param ngram a list specifying the min/max for ngrams
#' @param documentMinimum a word must appear in no less than n documents
#' @param stopWords words to eliminate from the vocabulary object (use character(0) for none)
#' @importFrom tm stopwords
#' @importFrom text2vec create_vocabulary
#' @importFrom text2vec prune_vocabulary
#' @export
text2vocab <- function(it, documentCount, ngram = c(1L, 3L), documentMinimum = 10,
                       stopWords = stopwords()) {

  create_vocabulary(it, ngram = ngram, stopwords = stopWords) %>%
    prune_vocabulary(
      term_count_min = 10,
      doc_proportion_max = 0.5,
      doc_proportion_min = max(documentMinimum / documentCount, 0.001)
    )
}
