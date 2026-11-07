#' Get a text2vec iterator over tokenized words
#'
#' @param text a vector of text documents
#' @param ids a vector of identifiers (e.g. book titles). Defaults to NULL,
#' which text2vec::itoken() turns into sequential integer ids automatically.
#' (Previously defaulted to the bare function `identity`, not a value -- a
#' likely source of an old, unexplained "cannot coerce class 'function' to
#' a data.frame" error.)
#' @param progressbar show progress bar if true (default: FALSE)
#' @importFrom text2vec word_tokenizer
#' @importFrom text2vec itoken
#' @export
getIterator <- function(text, ids = NULL, progressbar = FALSE) {
  text %>% tolower() %>% word_tokenizer() %>%
    itoken(ids = ids, progressbar = progressbar)
}
