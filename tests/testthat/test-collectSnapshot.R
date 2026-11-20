context('The nightly collector pulls, collects and copies, and keeps going on failure')

# data-raw/ is in .Rbuildignore, so the script exists only in a checkout:
# these tests run under `make test` and skip under R CMD check.
collector_script <- function() {
  path <- testthat::test_path("..", "..", "data-raw", "collect-snapshot.sh")
  if (file.exists(path)) normalizePath(path) else NA_character_
}

# A throwaway checkout holding the script, plus stand-ins for rsync and
# Rscript that record each call and fail when told to.
collector_sandbox <- function(rscript_status = 0L, fail_rsync = "",
                              fail_rclone = "") {
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
    sprintf('echo "rclone $*" >> "%s"', calls),
    sprintf('case "$*" in *"%s"*) [ -n "%s" ] && exit 1 ;; esac', fail_rclone, fail_rclone),
    "exit 0"
  ), file.path(bin, "rclone"))
  writeLines(c(
    "#!/bin/sh",
    sprintf('echo "Rscript $* TZ=$TZ" >> "%s"', calls),
    sprintf("exit %d", rscript_status)
  ), file.path(bin, "Rscript"))
  Sys.chmod(file.path(bin, c("rsync", "rclone", "Rscript")), "0755")

  list(root = root, bin = bin, calls = calls)
}

# Every rsync the collector makes: append-only, and bounded in time so a
# switched-off peer cannot hang the night.
rsync_call <- paste(
  "rsync -a --ignore-existing --timeout=300 -e ssh -o BatchMode=yes",
  "-o ConnectTimeout=30 -o ServerAliveInterval=30 -o ServerAliveCountMax=4"
)

run_collector <- function(box, args = character(), env = character()) {
  out <- suppressWarnings(system2(
    "sh", c(shQuote(file.path(box$root, "data-raw", "collect-snapshot.sh")),
            shQuote(args)),
    env = c(paste0("PATH=", shQuote(paste(box$bin, Sys.getenv("PATH"), sep = ":"))),
            env),
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
    paste(rsync_call, "pi:snaps/ data-raw/snapshots/"),
    "Rscript data-raw/snapshot-downloads.R TZ=UTC",
    paste(rsync_call, "data-raw/snapshots/ gpu-box:"),
    paste(rsync_call, "data-raw/snapshots/ pi:snaps/")
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
  expect_identical(res$calls[2], paste(rsync_call, "data-raw/snapshots/ gpu-box:"))
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

test_that("uploads come last, one per remote, and can only add files", {
  skip_unless_collector()
  box <- collector_sandbox()
  res <- run_collector(box, c("--copy-to", "gpu-box:",
                              "--offsite", "b2:bucket/snapshots/",
                              "--offsite", "b2:other/"))
  expect_identical(res$status, 0L)
  expect_identical(res$calls[3:4], c(
    "rclone copy --immutable data-raw/snapshots/ b2:bucket/snapshots/",
    "rclone copy --immutable data-raw/snapshots/ b2:other/"
  ))
  rclones <- grep("^rclone ", res$calls, value = TRUE)
  expect_true(all(startsWith(rclones, "rclone copy --immutable ")))
  expect_false(any(grepl("--delete|--backup-dir", rclones)))
})

test_that("a failed collection or copy does not stop the upload, and a failed upload is logged", {
  skip_unless_collector()
  box <- collector_sandbox(rscript_status = 1L, fail_rsync = "gpu-box:",
                           fail_rclone = "b2:bad/")
  res <- run_collector(box, c("--copy-to", "gpu-box:",
                              "--offsite", "b2:bad/", "--offsite", "b2:good/"))
  expect_identical(res$status, 1L)
  expect_true(any(grepl("FAILED: upload to b2:bad/", res$log, fixed = TRUE)))
  expect_true(any(grepl("uploaded to b2:good/", res$log, fixed = TRUE)))
})

test_that("RCLONE names the rclone binary", {
  skip_unless_collector()
  box <- collector_sandbox()
  alt <- file.path(box$bin, "rclone-new")
  writeLines(c("#!/bin/sh", sprintf('echo "rclone-new $*" >> "%s"', box$calls)), alt)
  Sys.chmod(alt, "0755")
  res <- run_collector(box, c("--offsite", "b2:x/"), env = paste0("RCLONE=", shQuote(alt)))
  expect_identical(res$calls[2], "rclone-new copy --immutable data-raw/snapshots/ b2:x/")
})

test_that("--offsite without a remote runs nothing", {
  skip_unless_collector()
  box <- collector_sandbox()
  expect_identical(run_collector(box, "--offsite")$status, 2L)
  expect_false(file.exists(box$calls))
})

test_that("every rsync gives up on an absent peer instead of hanging", {
  skip_unless_collector()
  box <- collector_sandbox()
  res <- run_collector(box, c("--pull-from", "a:/", "--copy-to", "b:"))
  rsyncs <- grep("^rsync ", res$calls, value = TRUE)
  expect_length(rsyncs, 2L)
  for (flag in c("--timeout=300", "ConnectTimeout=30", "BatchMode=yes")) {
    expect_true(all(grepl(flag, rsyncs, fixed = TRUE)), info = flag)
  }
})

# A payload in the sandbox, with the provenance line that vouches for it.
add_payload <- function(box, date, content = "BZh9 payload", recorded = NULL) {
  dir <- file.path(box$root, "data-raw", "payloads", date)
  dir.create(dir, recursive = TRUE)
  file <- file.path(dir, "rdf-files.tar.bz2")
  writeBin(charToRaw(content), file)
  sha <- digest::digest(file, algo = "sha256", file = TRUE)
  snap <- file.path(box$root, "data-raw", "snapshots", date)
  dir.create(snap, recursive = TRUE)
  writeLines(c("source_url: x", paste0("payload_sha256: ",
                                       if (is.null(recorded)) sha else recorded)),
             file.path(snap, "provenance.txt"))
  file
}

test_that("payloads move by rsync, with the source removed once sent", {
  skip_unless_collector()
  box <- collector_sandbox()
  res <- run_collector(box, c("--move-payloads-to", "pi:payloads/"))
  expect_identical(res$status, 0L)
  expect_identical(res$calls[2], paste(
    rsync_call, "--remove-source-files --exclude *.partial data-raw/payloads/ pi:payloads/"))
  expect_true(any(grepl("payloads moved to pi:payloads/", res$log, fixed = TRUE)))
})

test_that("a failed payload move is logged and leaves the payload", {
  skip_unless_collector()
  box <- collector_sandbox(fail_rsync = "pi:payloads/")
  file <- add_payload(box, "2026-12-07")
  res <- run_collector(box, c("--move-payloads-to", "pi:payloads/"))
  expect_identical(res$status, 1L)
  expect_true(any(grepl("FAILED: payload move", res$log, fixed = TRUE)))
  expect_true(file.exists(file))
})

test_that("a payload whose hash matches provenance goes off-site with moveto --immutable", {
  skip_unless_collector()
  box <- collector_sandbox()
  add_payload(box, "2026-12-07")
  res <- run_collector(box, c("--offsite-payloads", "b2:bucket/raw/"))
  expect_identical(res$status, 0L)
  expect_identical(res$calls[2], paste(
    "rclone moveto --immutable data-raw/payloads/2026-12-07/rdf-files.tar.bz2",
    "b2:bucket/raw/2026-12-07/rdf-files.tar.bz2"))
  expect_true(any(grepl("payload 2026-12-07 moved to b2:bucket/raw/2026-12-07/",
                        res$log, fixed = TRUE)))
})

test_that("a payload that does not match provenance, or has none, is kept and not uploaded", {
  skip_unless_collector()
  box <- collector_sandbox()
  bad <- add_payload(box, "2026-12-07", recorded = strrep("0", 64))
  orphan <- add_payload(box, "2027-01-06")
  unlink(file.path(box$root, "data-raw", "snapshots", "2027-01-06", "provenance.txt"))
  res <- run_collector(box, c("--offsite-payloads", "b2:bucket/raw/"))
  expect_identical(res$status, 1L)
  expect_false(any(grepl("^rclone", res$calls)))
  expect_true(any(grepl("payload 2026-12-07 hashes to", res$log, fixed = TRUE)))
  expect_true(any(grepl("payload 2027-01-06 has no payload_sha256", res$log, fixed = TRUE)))
  expect_true(all(file.exists(c(bad, orphan))))
})

test_that("a failed payload upload is logged and keeps the payload", {
  skip_unless_collector()
  box <- collector_sandbox(fail_rclone = "b2:bucket/raw/")
  file <- add_payload(box, "2026-12-07")
  res <- run_collector(box, c("--offsite-payloads", "b2:bucket/raw/"))
  expect_identical(res$status, 1L)
  expect_true(any(grepl("FAILED: payload 2026-12-07 upload", res$log, fixed = TRUE)))
  expect_true(file.exists(file))
})

test_that("each payload option may be given once, and needs a value", {
  skip_unless_collector()
  box <- collector_sandbox()
  expect_identical(run_collector(box, c("--move-payloads-to", "a:", "--move-payloads-to", "b:"))$status, 2L)
  expect_identical(run_collector(box, c("--offsite-payloads", "a:", "--offsite-payloads", "b:"))$status, 2L)
  expect_identical(run_collector(box, "--offsite-payloads")$status, 2L)
  expect_false(file.exists(box$calls))
})

test_that("RSYNC names the rsync binary", {
  skip_unless_collector()
  box <- collector_sandbox()
  alt <- file.path(box$bin, "rsync-new")
  writeLines(c("#!/bin/sh", sprintf('echo "rsync-new $*" >> "%s"', box$calls)), alt)
  Sys.chmod(alt, "0755")
  res <- run_collector(box, c("--copy-to", "b:"), env = paste0("RSYNC=", shQuote(alt)))
  expect_identical(res$calls[2], sub("^rsync", "rsync-new",
                                     paste(rsync_call, "data-raw/snapshots/ b:")))
})
