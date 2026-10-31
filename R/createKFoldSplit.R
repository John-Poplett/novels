#' Title createKFoldSplit
#'
#' Generalizes createSplit() from a single 80/20 split into K author-safe
#' folds, for repeated cross-validation rather than one train/test split.
#' Splitting by author (not by row) means no author's books span more than
#' one fold, the same safeguard createSplit() uses.
#'
#' @param data a data frame of literary texts
#' @param k number of folds (default 5)
#' @param seed random seed for reproducibility (ignored when use_stored_folds = TRUE)
#' @param use_stored_folds whether to read fold assignments from stored table (default: FALSE)
#' @returns a list of k elements, each a list with integer vectors `train`
#'   and `test` -- the same shape as createSplit()'s single-split return
#'   value, so existing per-split code can be reused unchanged per fold
#' @export
createKFoldSplit <- function(data, k = 5, seed = 42, use_stored_folds = FALSE) {
  stopifnot('data.frame' %in% class(data) && 'author' %in% names(data))
  
  if (use_stored_folds) {
    # Read stored folds from CSV
    stored_path <- system.file("extdata", "author_folds.csv", package = "novels")
    if (file.exists(stored_path)) {
      stored_folds <- readr::read_csv(stored_path, show_col_types = FALSE)
      
      # Create a mapping for the authors in our dataset
      # Match stored authors to current dataset authors
      dataset_authors <- unique(data$author) 
      matched_folds <- stored_folds$fold[match(dataset_authors, stored_folds$author)]
      
      # Check that all authors were found (should not happen with good design)
      if (any(is.na(matched_folds))) {
        stop("Some authors in the dataset were not found in the stored fold table")
      }
      
      names(matched_folds) <- dataset_authors
    } else {
      # Fall back to generating folds if table doesn't exist
      all_authors <- unique(data$author)
      matched_folds <- assignAuthorFolds(all_authors, k, seed)
    }
  } else {
    # Original behavior - generate folds using the seed approach
    all_authors <- unique(data$author)
    matched_folds <- assignAuthorFolds(all_authors, k, seed)
  }

  lapply(seq_len(k), function(i) {
    test_authors <- names(matched_folds[matched_folds == i])
    train_authors <- names(matched_folds[matched_folds != i])
    list(
      train = which(data$author %in% train_authors),
      test = which(data$author %in% test_authors)
    )
  })
}
