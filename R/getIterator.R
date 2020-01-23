#' Get a text2vec iterator over tokenized words
#'
#' @param text a vector of text documents
#' @param ids a list of identifiers (e.g. book titles)
#' @param progressbar show progress bar if true (default: FALSE)
#' @importFrom text2vec word_tokenizer
#' @importFrom text2vec itoken
#' @export
getIterator <- function(text, ids, progressbar = FALSE) {
  text %>% tolower() %>% word_tokenizer() %>% 
    itoken(ids = ids, progressbar = progressbar)
}
