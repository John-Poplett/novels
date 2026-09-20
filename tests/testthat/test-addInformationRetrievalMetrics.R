context('Zero-denominator confusion matrix cells are handled explicitly, not silently')
library(novels)

# NOTE: is.na(NaN) is TRUE in R -- NaN is treated as a case of NA. So these
# tests deliberately check is.nan() as well, to distinguish "produced a
# genuine, intentional NA" (the fix) from "produced a floating-point NaN
# via 0/0" (today's behavior). A naive is.na()-only check would pass even
# against the bug and wouldn't be a meaningful test-first guard.

test_that("Precision is a real NA, not NaN, when TP + FP == 0", {
  data <- data.frame(TP = 0, TN = 10, FP = 0, FN = 2)
  result <- addInformationRetrievalMetrics(data)
  expect_true(is.na(result$Precision))
  expect_false(is.nan(result$Precision))
})

test_that("Recall is a real NA, not NaN, when TP + FN == 0", {
  data <- data.frame(TP = 0, TN = 10, FP = 3, FN = 0)
  result <- addInformationRetrievalMetrics(data)
  expect_true(is.na(result$Recall))
  expect_false(is.nan(result$Recall))
})

test_that("Specificity is a real NA, not NaN, when TN + FP == 0", {
  data <- data.frame(TP = 5, TN = 0, FP = 0, FN = 2)
  result <- addInformationRetrievalMetrics(data)
  expect_true(is.na(result$Specificity))
  expect_false(is.nan(result$Specificity))
})

test_that("F.Score is a real NA, not NaN, when Precision + Recall == 0", {
  data <- data.frame(TP = 0, TN = 10, FP = 0, FN = 0)
  result <- addInformationRetrievalMetrics(data)
  expect_true(is.na(result$F.Score))
  expect_false(is.nan(result$F.Score))
})

test_that("a normal, well-populated confusion matrix still computes correctly", {
  data <- data.frame(TP = 40, TN = 40, FP = 10, FN = 10)
  result <- addInformationRetrievalMetrics(data)
  expect_equal(result$Accuracy, 80)
  expect_equal(result$Precision, 80)
  expect_equal(result$Recall, 80)
})
