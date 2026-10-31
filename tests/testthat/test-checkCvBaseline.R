context('The cross-validation baseline flags results that move more than noise')

baseline <- data.frame(
  alpha    = c(0.01, 0.5),
  mean_auc = c(0.74, 0.69),
  sd_auc   = c(0.075, 0.02) # SE with k = 5: 0.0335 and 0.0089
)

new_results <- function(mean_auc) {
  data.frame(alpha = c(0.01, 0.5), mean_auc = mean_auc, sd_auc = c(0.07, 0.03))
}

test_that("results within the tolerance pass without a warning", {
  expect_no_warning(
    out <- novels:::checkCvBaseline(new_results(c(0.75, 0.70)), baseline, k = 5)
  )
  expect_false(any(out$beyond))
})

test_that("a move beyond 2 standard errors warns and names the alpha", {
  expect_warning(
    out <- novels:::checkCvBaseline(new_results(c(0.74, 0.72)), baseline, k = 5),
    "alpha = 0.5"
  )
  expect_identical(out$beyond, c(FALSE, TRUE))
})

test_that("the tolerance scales with the baseline's own noise", {
  # The same +0.04 is noise at alpha 0.01 (SE 0.0335) but not at alpha 0.5
  # (SE 0.0089).
  expect_warning(
    out <- novels:::checkCvBaseline(new_results(c(0.78, 0.73)), baseline, k = 5)
  )
  expect_identical(out$beyond, c(FALSE, TRUE))
})

test_that("a drop is flagged as well as a rise", {
  expect_warning(
    novels:::checkCvBaseline(new_results(c(0.74, 0.66)), baseline, k = 5),
    "-0.030"
  )
})

test_that("the comparison is returned with the documented columns", {
  out <- suppressWarnings(
    novels:::checkCvBaseline(new_results(c(0.75, 0.70)), baseline, k = 5)
  )
  expect_named(out, c("alpha", "baseline", "new", "se", "delta", "beyond"))
  expect_equal(out$delta, c(0.01, 0.01))
})

test_that("a different alpha grid is an error, not a silent mismatch", {
  other <- data.frame(alpha = c(0.01, 0.9), mean_auc = c(0.74, 0.69), sd_auc = c(0.07, 0.03))

  expect_error(novels:::checkCvBaseline(other, baseline, k = 5), "alpha grid differs")
})
