context('The nightly collector pulls, collects and copies, and keeps going on failure')

# data-raw/ is in .Rbuildignore, so the script exists only in a checkout:
# these tests run under `make test` and skip under R CMD check.
collector_script <- function() {
  path <- testthat::test_path("..", "..", "data-raw", "collect-snapshot.sh")
  if (file.exists(path)) normalizePath(path) else NA_character_
}

# A throwaway checkout holding the script, plus stand-ins for rsync and
# Rscript that record each call and fail when told to.
collector_sandbox <- function(rscript_status = 0L, fail_rsync = "") {
  root <- tempfile("collector-")
  dir.create(file.path(root, "data-raw"), recursive = TRUE)
  file.copy(collector_script(), file.path(root, "data-raw", "collect-snapshot.sh"))
  writeLines("# stand-in", file.path(root, "data-raw", "snapshot-downloads.R"))

  bin <- file.path(root, "bin")
  dir.create(bin)
  calls <- file.path(root, "calls.log")
  writeLines(c(
    "#!/bin/sh",
    sprintf('echo "rsync $*" >> "%s"', calls),
    sprintf('case "$*" in *"%s"*) [ -n "%s" ] && exit 23 ;; esac', fail_rsync, fail_rsync),
    "exit 0"
  ), file.path(bin, "rsync"))
  writeLines(c(
    "#!/bin/sh",
    sprintf('echo "Rscript $* TZ=$TZ" >> "%s"', calls),
    sprintf("exit %d", rscript_status)
  ), file.path(bin, "Rscript"))
  Sys.chmod(file.path(bin, c("rsync", "Rscript")), "0755")

  list(root = root, bin = bin, calls = calls)
}

run_collector <- function(box, args = character()) {
  out <- suppressWarnings(system2(
    "sh", c(shQuote(file.path(box$root, "data-raw", "collect-snapshot.sh")),
            shQuote(args)),
    env = paste0("PATH=", shQuote(paste(box$bin, Sys.getenv("PATH"), sep = ":"))),
    stdout = TRUE, stderr = TRUE
  ))
  list(
    status = if (is.null(attr(out, "status"))) 0L else attr(out, "status"),
    log = out,
    calls = if (file.exists(box$calls)) readLines(box$calls) else character()
  )
}

skip_unless_collector <- function() {
  testthat::skip_on_os("windows")
  testthat::skip_if(is.na(collector_script()),
                    "data-raw/collect-snapshot.sh is not in this build")
}

test_that("the collector pulls, then collects in UTC, then copies, in that order", {
  skip_unless_collector()
  box <- collector_sandbox()
  res <- run_collector(box, c("--pull-from", "pi:snaps/",
                              "--copy-to", "gpu-box:", "--copy-to", "pi:snaps/"))
  expect_identical(res$status, 0L)
  expect_identical(res$calls, c(
    "rsync -a --ignore-existing pi:snaps/ data-raw/snapshots/",
    "Rscript data-raw/snapshot-downloads.R TZ=UTC",
    "rsync -a --ignore-existing data-raw/snapshots/ gpu-box:",
    "rsync -a --ignore-existing data-raw/snapshots/ pi:snaps/"
  ))
  expect_true(any(grepl("done, status 0$", res$log)))
})

test_that("no copy can delete or overwrite: every rsync is --ignore-existing, none --delete", {
  skip_unless_collector()
  box <- collector_sandbox()
  res <- run_collector(box, c("--pull-from", "a:/", "--copy-to", "b:"))
  rsyncs <- grep("^rsync ", res$calls, value = TRUE)
  expect_length(rsyncs, 2L)
  expect_true(all(grepl("--ignore-existing", rsyncs, fixed = TRUE)))
  expect_false(any(grepl("--delete", rsyncs, fixed = TRUE)))
})

test_that("a failed pull is logged, and the collection and copies still run", {
  skip_unless_collector()
  box <- collector_sandbox(fail_rsync = "pi:snaps/")
  res <- run_collector(box, c("--pull-from", "pi:snaps/", "--copy-to", "gpu-box:"))
  expect_identical(res$status, 1L)
  expect_true(any(grepl("FAILED: pull from pi:snaps/", res$log, fixed = TRUE)))
  expect_length(res$calls, 3L)
})

test_that("a failed collection is logged, and the copies still run", {
  skip_unless_collector()
  box <- collector_sandbox(rscript_status = 1L)
  res <- run_collector(box, c("--copy-to", "gpu-box:"))
  expect_identical(res$status, 1L)
  expect_true(any(grepl("FAILED: data-raw/snapshot-downloads.R exited 1", res$log, fixed = TRUE)))
  expect_identical(res$calls[2], "rsync -a --ignore-existing data-raw/snapshots/ gpu-box:")
})

test_that("one failed copy does not stop the next", {
  skip_unless_collector()
  box <- collector_sandbox(fail_rsync = "gpu-box:")
  res <- run_collector(box, c("--copy-to", "gpu-box:", "--copy-to", "pi:snaps/"))
  expect_identical(res$status, 1L)
  expect_true(any(grepl("FAILED: copy to gpu-box:", res$log, fixed = TRUE)))
  expect_true(any(grepl("copied to pi:snaps/", res$log, fixed = TRUE)))
})

test_that("an unknown or incomplete argument runs nothing", {
  skip_unless_collector()
  box <- collector_sandbox()
  expect_identical(run_collector(box, "--copy-too")$status, 2L)
  expect_identical(run_collector(box, "--copy-to")$status, 2L)
  expect_false(file.exists(box$calls))
})

test_that("a script outside a checkout runs nothing", {
  skip_unless_collector()
  box <- collector_sandbox()
  unlink(file.path(box$root, "data-raw", "snapshot-downloads.R"))
  res <- run_collector(box)
  expect_identical(res$status, 1L)
  expect_true(any(grepl("cannot find the checkout", res$log, fixed = TRUE)))
  expect_false(file.exists(box$calls))
})
