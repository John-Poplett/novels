#' Return a logical vector of authors that match the specified pattern
#' @param author a regex pattern for one or more authors
#' @param data a data frame of literary texts in "Stonybrook" format
#' @export
selectByAuthor <- function(author, data) {
  grepl(author, gsub("([^,][^,]*).*$", "\\1", data$author), ignore.case = TRUE)
}
