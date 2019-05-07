
#' Tidy up metadata
#' @param metadata data frame of novel metadata
#' @import dplyr
#' @importFrom tools toTitleCase
#' @export
tidyMetadata <- function(metadata) {
  metadata <- metadata  %>%
    mutate(author = toTitleCase(as.character(.data$author)), title = toTitleCase(as.character(.data$title)))
  metadata %>% select(.data$author, .data$title, .data$genre, .data$language, .data$file.name, .data$download.count, .data$response)
}
