#' Build a text2vec vocabulary vectorizer, with document ids
#'
#' Uses getIterator()/text2vocab() -- stopword removal and vocabulary
#' pruning, with ngram range and pruning thresholds exposed as parameters
#' rather than hardcoded, so isolation tests (e.g. unigrams-only) don't
#' require editing text2vocab() itself. Document titles are threaded
#' through as ids so predictions can always be traced back to the book
#' they came from.
#'
#' @param df a data frame with text and title columns
#' @param ngram passed through to text2vocab() (default c(1L, 3L))
#' @param documentMinimum passed through to text2vocab() (default 10)
#' @param stopWords passed through to text2vocab() (default tm::stopwords())
#' @returns a text2vec vocab_vectorizer
#' @export
buildVectorizer <- function(df, ngram = c(1L, 3L), documentMinimum = 10,
                             stopWords = stopwords()) {
  it <- getIterator(df$text, df$title)
  vocab <- text2vocab(it, nrow(df), ngram = ngram,
                       documentMinimum = documentMinimum, stopWords = stopWords)
  vocab_vectorizer(vocab)
}
