#' Create a train and test split of the novels dataset
#'
#' Splits by author, not by row, so that no author's books appear in both
#' train and test. Deliberately does not deduplicate by filename -- the
#' same book legitimately appearing under multiple genres is intentional
#' (see novel_metadata's design), not something to strip out here. Since
#' the split is by author, every genre-row belonging to one author's books
#' is guaranteed to land on the same side of the split regardless.
#'
#' @param data a data frame of literary texts
#' @param split fraction of authors assigned to the training set (default 0.8)
#' @param seed random seed for reproducibility
#' @export
createSplit <- function(data, split = .8, seed = 42) {
  stopifnot('data.frame' %in% class(data) && 'author' %in% names(data))
  set.seed(seed)
  all_ids <- unique(data$author)
  train_ids <- sample(all_ids, as.integer(length(all_ids) * split))
  test_ids <- setdiff(all_ids, train_ids)
  # Training and test split by author
  train_indices <- which(data$author %in% train_ids)
  test_indices <- which(data$author %in% test_ids)
  list("train" = train_indices, "test" = test_indices)
}
