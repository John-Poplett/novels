#' Create a train and test split of the novels dataset
#' @param data a data frame of literary texts in "Stonybrook" format
#' @param split specify a split ratio (default = .8)
#' @export
createSplit <- function(data, split = .8) {
  stopifnot('data.frame' %in% class(data) && 'author' %in% names(data))
  all_ids <- unique(data$author)
  train_ids <- sample(all_ids, as.integer(length(all_ids) * .8))
  test_ids <- setdiff(all_ids, train_ids)
  # Training and test split by author
  train_indices <- which(data$author %in% train_ids)
  test_indices <- which(data$author %in% test_ids)
  # Eliminate duplicate filenames
  duplicateFilenames <- data %>% duplicated(by = 'file.name')
  train_indices <- which(train_indices %in% which(!duplicateFilenames))
  test_indices <- which(test_indices %in% which(!duplicateFilenames))
  list("train" = train_indices, "test" = test_indices)
}
