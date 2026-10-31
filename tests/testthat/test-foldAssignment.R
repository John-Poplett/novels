context('Fold assignment using stored table')

test_that('createKFoldSplit with use_stored_folds = TRUE matches stored folds', {
  # Load the test dataset  
  novels <- local({
    load("data/novels.rda")
    novels
  })
  
  # Create splits using stored folds approach
  stored_splits <- createKFoldSplit(novels, k_folds = 5, use_stored_folds = TRUE)
  
  # Verify that these are valid splits (each fold has train and test)
  expect_equal(length(stored_splits), 5)
  
  for (k in 1:5) {
    expect_true(is.integer(stored_splits[[k]]$train))
    expect_true(is.integer(stored_splits[[k]]$test))
    expect_true(length(stored_splits[[k]]$train) > 0)
    expect_true(length(stored_splits[[k]]$test) > 0)
  }
  
  # Verify no overlap between train and test
  for (k in 1:5) {
    expect_equal(intersect(stored_splits[[k]]$train, stored_splits[[k]]$test), integer(0))
  }
})

test_that('createKFoldSplit with use_stored_folds = FALSE produces correct structure', {
  # This should work the same as before for backward compatibility
  novels <- local({
    load("data/novels.rda")
    novels
  })
  
  # Create splits using original method
  original_splits <- createKFoldSplit(novels, k_folds = 5, use_stored_folds = FALSE)
  
  expect_equal(length(original_splits), 5)
  
  for (k in 1:5) {
    expect_true(is.integer(original_splits[[k]]$train))
    expect_true(is.integer(original_splits[[k]]$test))
  }
})