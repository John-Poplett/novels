context('Cross-validation folds do not leak books or authors across the split')

test_that("no book appears in more than one test fold", {
  folds <- createKFoldSplit(novels, k = 5, seed = 42)
  test_ids <- lapply(folds, function(f) unique(novels$gutenberg_id[f$test]))
  occurrences <- table(unlist(test_ids))

  # 41 books carry two or three genre rows; every row of a book must land in
  # the same fold, otherwise identical text is trained on and scored at once.
  expect_true(all(occurrences == 1))
  expect_equal(length(occurrences), length(unique(novels$gutenberg_id)))
})

test_that("no author is in both the training and held-out side of a fold", {
  folds <- createKFoldSplit(novels, k = 5, seed = 42)

  for (fold in folds) {
    expect_length(
      intersect(unique(novels$author[fold$train]), unique(novels$author[fold$test])),
      0
    )
  }
})

test_that("makeFoldid keeps every row of an author in the same inner fold", {
  folds <- makeFoldid(novels$author, nfolds = 5, seed = 42)
  spread <- tapply(folds, novels$author, function(x) length(unique(x)))

  expect_true(all(spread == 1))
})

test_that("the dataset's native fold column is not safe to pool", {
  # Documents why folds are regenerated rather than reused: the same book sits
  # in different native folds under different genres, so a pooled model using
  # that column would be trained and tested on identical text. If this ever
  # stops being true, the Roadmap's per-genre plan can be revisited.
  multi <- names(which(table(novels$gutenberg_id) > 1))
  native_folds <- vapply(
    multi,
    function(id) length(unique(novels$fold[novels$gutenberg_id == as.integer(id)])),
    integer(1)
  )

  expect_gt(sum(native_folds > 1), 0)
})
