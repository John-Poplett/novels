test_that("parallelResults passes complete results through", {
  r <- list(data.frame(x = 1), data.frame(x = 2))
  expect_identical(parallelResults(r), r)
  expect_identical(parallelResults(list()), list())
})

test_that("a job whose worker died stops the run, not just a warning", {
  # Regression: a NULL slot passed the try-error guard and bind_rows dropped it.
  r <- list(data.frame(x = 1), NULL, data.frame(x = 3), NULL)
  expect_error(parallelResults(r), "2 of 4 parallel jobs returned nothing \\(jobs 2, 4\\)")
})

test_that("a job that failed in R stops the run with its message", {
  bad <- try(stop("glmnet did not converge"), silent = TRUE)
  expect_error(parallelResults(list(data.frame(x = 1), bad)),
               "1 of 2 parallel jobs failed \\(jobs 2\\).*glmnet did not converge")
})

test_that("a killed forked worker is caught", {
  skip_on_os("windows")  # no forking
  r <- suppressWarnings(parallel::mclapply(1:2, function(i) {
    if (i == 2) tools::pskill(Sys.getpid(), tools::SIGKILL)
    i
  }, mc.cores = 2, mc.preschedule = FALSE))
  expect_error(parallelResults(r), "returned nothing \\(jobs 2\\)")
})

test_that("parallelResults refuses what is not a list", {
  expect_error(parallelResults(1:3), "must be a list")
})
