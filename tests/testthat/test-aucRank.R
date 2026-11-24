test_that("aucRank agrees with pROC, ties included", {
  set.seed(1)
  truth <- rep(c(0, 1), c(40, 60))
  score <- round(stats::rnorm(100) + truth, 1)  # rounding makes ties
  expected <- as.numeric(pROC::auc(pROC::roc(truth, score, quiet = TRUE,
                                             direction = "<")))
  expect_equal(aucRank(truth, score), expected)
})

test_that("aucRank's edges: perfect, reversed, all tied", {
  expect_identical(aucRank(c(0, 0, 1, 1), c(1, 2, 3, 4)), 1)
  expect_identical(aucRank(c(0, 0, 1, 1), c(4, 3, 2, 1)), 0)
  expect_identical(aucRank(c(0, 1, 0, 1), rep(5, 4)), 0.5)
  expect_identical(aucRank(c(FALSE, TRUE), c(0.2, 0.9)), 1)
  expect_identical(aucRank(c(0L, 1L), c(0.2, 0.9)), 1)
})

test_that("a sample with one class gives NA, not NaN", {
  # Regression: two of the four script copies divided by n1 * n0 = 0.
  expect_identical(aucRank(c(1, 1, 1), c(0.1, 0.5, 0.9)), NA_real_)
  expect_identical(aucRank(c(0, 0), c(0.1, 0.5)), NA_real_)
  expect_identical(aucRank(numeric(), numeric()), NA_real_)
})

test_that("aucRank refuses inputs it would misread", {
  expect_error(aucRank(c(0, 2), c(1, 2)), "truth must be 0/1")
  expect_error(aucRank(c(0, NA), c(1, 2)), "truth must be 0/1")
  expect_error(aucRank(factor(c(0, 1)), c(1, 2)), "truth must be 0/1")
  expect_error(aucRank(c(0, 1), c(1, NA)), "score must be numeric")
  expect_error(aucRank(c(0, 1), c(1, 2, 3)), "it has 3")
  expect_error(aucRank(c(0, 1), c("a", "b")), "score must be numeric")
})
