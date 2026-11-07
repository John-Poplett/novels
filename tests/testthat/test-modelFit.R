context("Model fitting and evaluation")

# Small enough to run in a second, large enough that cv.glmnet's five inner
# folds always contain both classes.
make_model_input <- function(n = 80L, p = 12L, seed = 11L) {
  set.seed(seed)
  x <- matrix(stats::rbinom(n * p, 1, 0.3), nrow = n)
  colnames(x) <- paste0("v", seq_len(p))
  y <- sample(rep(0:1, length.out = n))
  list(x = x, y = y)
}

test_that("fitGlmnetModel rejects a non-binary response", {
  d <- make_model_input()
  foldid <- makeFoldid(rep(paste0("a", 1:16), length.out = nrow(d$x)), nfolds = 5)

  expect_error(fitGlmnetModel(d$x, d$y + 2L, alpha = 0.5, foldid = foldid))
})

test_that("fitGlmnetModel honours the supplied foldid", {
  d <- make_model_input()
  foldid <- makeFoldid(rep(paste0("a", 1:16), length.out = nrow(d$x)), nfolds = 5)

  fit <- fitGlmnetModel(d$x, d$y, alpha = 0.5, foldid = foldid)

  expect_s3_class(fit, "cv.glmnet")
  # cv.glmnet does not return foldid as a field, but it records the call.
  expect_identical(fit$call$foldid, quote(foldid))
  # A fixed foldid makes the fit reproducible.
  expect_identical(fit$cvm, fitGlmnetModel(d$x, d$y, alpha = 0.5, foldid = foldid)$cvm)
})

test_that("evaluateModel reports one row per fit with alpha and AUC", {
  d <- make_model_input()
  foldid <- makeFoldid(rep(paste0("a", 1:16), length.out = nrow(d$x)), nfolds = 5)
  fit <- fitGlmnetModel(d$x, d$y, alpha = 0.5, foldid = foldid)

  evaluation <- evaluateModel(fit, d$x, d$y, alpha = 0.5)

  expect_named(evaluation, c("alpha", "auc", "n_nonzero_coef"))
  expect_equal(nrow(evaluation), 1L)
  expect_equal(evaluation$alpha, 0.5)
  expect_true(evaluation$auc >= 0 && evaluation$auc <= 1)
  expect_type(evaluation$n_nonzero_coef, "integer")
})

test_that("evaluateModel requires aligned predictions and response", {
  d <- make_model_input()
  foldid <- makeFoldid(rep(paste0("a", 1:16), length.out = nrow(d$x)), nfolds = 5)
  fit <- fitGlmnetModel(d$x, d$y, alpha = 0.5, foldid = foldid)

  expect_error(evaluateModel(fit, d$x, d$y[-1], alpha = 0.5))
})
