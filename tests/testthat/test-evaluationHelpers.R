context("Threshold and confusion-matrix helpers")

test_that("results2Text is strictly greater than the threshold", {
  expect_identical(
    results2Text(c(0.49, 0.5, 0.5001, 1)),
    c("failure", "failure", "success", "success")
  )
  expect_identical(
    results2Text(c(0.6, 0.4), threshold = 0.5),
    c("success", "failure")
  )
})

test_that("classifyAt thresholds probability, not the rounded score", {
  # All three rows display score 50, but only the first probability clears 0.5.
  scored <- tibble::tibble(
    probability = c(0.500001, 0.5, 0.499999),
    score = c(50, 50, 50)
  )

  out <- classifyAt(scored)

  expect_equal(out$prediction, c(1, 0, 0))
})

test_that("classifyAt requires a probability column", {
  expect_error(classifyAt(tibble::tibble(score = 50)), "probability")
})

test_that("classifyAt accepts any threshold without refitting", {
  scored <- tibble::tibble(probability = c(0.2, 0.8))

  expect_equal(classifyAt(scored, threshold = 0.1)$prediction, c(1, 1))
  expect_equal(classifyAt(scored, threshold = 0.9)$prediction, c(0, 0))
})

test_that("createConfusionMatrix counts TP, FP, TN and FN", {
  preds <- tibble::tibble(
    response = c(1, 1, 0, 0),
    prediction = c(1, 0, 1, 0)
  )

  cm <- createConfusionMatrix(preds)

  expect_equal(cm$TP, 1)
  expect_equal(cm$FP, 1)
  expect_equal(cm$TN, 1)
  expect_equal(cm$FN, 1)
})

test_that("createConfusionMatrix applies the threshold to both columns", {
  preds <- tibble::tibble(
    response = c(0.9, 0.1),
    prediction = c(0.9, 0.1)
  )

  cm <- createConfusionMatrix(preds)

  expect_equal(cm$TP, 1)
  expect_equal(cm$TN, 1)
  expect_equal(cm$FP + cm$FN, 0)
})

test_that("createConfusionMatrix keeps a row per count, not per prediction", {
  preds <- tibble::tibble(
    response = c(1, 1, 0, 0, 0),
    prediction = c(1, 1, 0, 0, 0)
  )

  cm <- createConfusionMatrix(preds)

  expect_equal(nrow(cm), 1L)
  expect_equal(cm$TP, 2)
  expect_equal(cm$TN, 3)
})
