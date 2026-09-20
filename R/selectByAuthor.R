#' Return a logical vector of authors that match the specified pattern
#' @param author a regex pattern for one or more authors
#' @param data a data frame of literary texts in "Stonybrook" format
#' @export
selectByAuthor <- function(author, data) {
  sapply(data$author, function(authors_field) {
    # tidyRawData() joins co-authors with "; " (see the group_by/summarize
    # dedup step in tidyRawData()) -- check each co-author individually,
    # not just whichever happens to be listed first.
    co_authors <- strsplit(authors_field, "; ", fixed = TRUE)[[1]]
    surnames <- gsub("([^,][^,]*).*$", "\\1", co_authors)
    any(grepl(author, surnames, ignore.case = TRUE))
  }, USE.NAMES = FALSE)
}
