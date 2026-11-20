context('Download snapshots are read, written and appended once')
library(novels)

rdf_record <- function(id, downloads = 1234L, issued = "2008-07-02") {
  paste0(
    '<?xml version="1.0" encoding="utf-8"?>\n',
    '<rdf:RDF xmlns:pgterms="http://www.gutenberg.org/2009/pgterms/" ',
    'xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" ',
    'xmlns:dcterms="http://purl.org/dc/terms/">\n',
    '  <pgterms:ebook rdf:about="ebooks/', id, '">\n',
    '    <dcterms:issued rdf:datatype="http://www.w3.org/2001/XMLSchema#date">',
    issued, '</dcterms:issued>\n',
    '    <pgterms:downloads rdf:datatype="http://www.w3.org/2001/XMLSchema#integer">',
    downloads, '</pgterms:downloads>\n',
    '  </pgterms:ebook>\n',
    '</rdf:RDF>\n'
  )
}

write_fixture <- function(dir, id, ...) {
  path <- file.path(dir, paste0("pg", id, ".rdf"))
  writeLines(rdf_record(id, ...), path)
  path
}

test_that("readGutenbergRdf extracts the counter and the release date", {
  dir <- tempfile("rdf")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  paths <- c(
    write_fixture(dir, 215L, downloads = 19679L, issued = "2008-07-02"),
    write_fixture(dir, 77600L, downloads = 0L, issued = "2026-01-01")
  )

  result <- readGutenbergRdf(paths)
  expect_equal(result$gutenberg_id, c(215L, 77600L))
  expect_equal(result$downloads, c(19679L, 0L))
  expect_equal(result$date_added, as.Date(c("2008-07-02", "2026-01-01")))
  expect_type(result$downloads, "integer")
})

test_that("readGutenbergRdf refuses a record with no download count", {
  dir <- tempfile("rdf")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  path <- file.path(dir, "pg1.rdf")
  writeLines('<rdf:RDF><pgterms:ebook></pgterms:ebook></rdf:RDF>', path)
  expect_error(readGutenbergRdf(path), "pg1[.]rdf")
})

test_that("readGutenbergRdf refuses duplicate book ids", {
  dir <- tempfile("rdf")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  path <- write_fixture(dir, 7L)
  expect_error(readGutenbergRdf(c(path, path)), "Duplicate")
})

test_that("a snapshot round-trips through writeDownloadSnapshot and readDownloadSnapshots", {
  dir <- tempfile("snapshots")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  books <- data.frame(
    gutenberg_id = c(215L, 7L),
    downloads = c(19679L, 8L),
    date_added = as.Date(c("2008-07-02", "2003-05-01"))
  )

  path <- writeDownloadSnapshot(books, "2026-11-30", "test-source", dir = dir)
  expect_true(file.exists(path))
  expect_true(file.exists(file.path(dir, "2026-11-30", "date_added.csv.gz")))

  series <- readDownloadSnapshots(dir)
  expect_equal(names(series), c("gutenberg_id", "snapshot_date", "source", "downloads"))
  expect_equal(nrow(series), 2L)
  expect_equal(unique(series$snapshot_date), as.Date("2026-11-30"))
  expect_equal(unique(series$source), "test-source")
  expect_equal(series$downloads[series$gutenberg_id == 215L], 19679L)
})

test_that("an empty snapshot root reads as zero rows, not an error", {
  dir <- tempfile("snapshots")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  series <- readDownloadSnapshots(dir)
  expect_equal(nrow(series), 0L)
  expect_equal(names(series), c("gutenberg_id", "snapshot_date", "source", "downloads"))
  expect_s3_class(series$snapshot_date, "Date")
})

test_that("a second snapshot for the same date is refused", {
  dir <- tempfile("snapshots")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  books <- data.frame(
    gutenberg_id = 1L, downloads = 10L, date_added = as.Date("2000-01-01")
  )
  writeDownloadSnapshot(books, "2026-11-30", "test-source", dir = dir)
  expect_error(
    writeDownloadSnapshot(books, "2026-11-30", "test-source", dir = dir),
    "append-only"
  )
})

test_that("writeDownloadSnapshot rejects missing or negative counts", {
  dir <- tempfile("snapshots")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  books <- data.frame(
    gutenberg_id = 1L, downloads = NA_integer_, date_added = as.Date("2000-01-01")
  )
  expect_error(
    writeDownloadSnapshot(books, "2026-11-30", "test-source", dir = dir),
    "non-missing"
  )
})

snapshot_fixture <- function(dir, ...) {
  books <- data.frame(
    gutenberg_id = 1L, downloads = 10L, date_added = as.Date("2000-01-01")
  )
  writeDownloadSnapshot(books, ..., source = "test-source", dir = dir)
}

test_that("snapshotDue is TRUE when the series is empty", {
  dir <- tempfile("snapshots")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  expect_true(snapshotDue(dir, today = as.Date("2026-11-07")))
})

test_that("snapshotDue is FALSE one day before the 30-day window closes", {
  dir <- tempfile("snapshots")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  snapshot_fixture(dir, "2026-11-07")
  expect_false(snapshotDue(dir, today = as.Date("2026-12-06")))
})

test_that("snapshotDue is TRUE on the thirtieth day after the last snapshot", {
  dir <- tempfile("snapshots")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  snapshot_fixture(dir, "2026-11-07")
  expect_true(snapshotDue(dir, today = as.Date("2026-12-07")))
})

test_that("snapshotDue stays TRUE after a missed run, so a late job still fires", {
  dir <- tempfile("snapshots")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  snapshot_fixture(dir, "2026-11-07")
  expect_true(snapshotDue(dir, today = as.Date("2027-01-20")))
})

test_that("a snapshot 29 days after the last one would double count, so it is refused", {
  dir <- tempfile("snapshots")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  snapshot_fixture(dir, "2026-11-07")
  expect_error(
    snapshot_fixture(dir, "2026-12-06"),
    "overlap"
  )
})

test_that("the overlap error names the previous snapshot so the fix is obvious", {
  dir <- tempfile("snapshots")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  snapshot_fixture(dir, "2026-11-07")
  expect_error(snapshot_fixture(dir, "2026-12-06"), "2026-11-07")
})

test_that("a snapshot exactly 30 days after the last one is written without complaint", {
  dir <- tempfile("snapshots")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  snapshot_fixture(dir, "2026-11-07")
  expect_silent(snapshot_fixture(dir, "2026-12-07"))
})

test_that("a late snapshot is written but warns, because it leaves a coverage gap", {
  dir <- tempfile("snapshots")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  snapshot_fixture(dir, "2026-11-07")
  expect_warning(snapshot_fixture(dir, "2027-01-20"), "day")
})

# --- The fixed grid (regression: a late run used to shift every later date) --

test_that("nextSnapshotDate steps on the grid anchored at 2026-11-07", {
  expect_equal(nextSnapshotDate("2026-11-07"), as.Date("2026-12-07"))
  expect_equal(nextSnapshotDate("2026-12-07"), as.Date("2027-01-06"))
  expect_equal(nextSnapshotDate("2026-12-08"), as.Date("2027-01-06"))
  expect_equal(nextSnapshotDate("2027-01-05"), as.Date("2027-01-06"))
  expect_equal(nextSnapshotDate("2028-12-26"), as.Date("2029-01-25"))
})

test_that("nextSnapshotDate honours a different anchor and rejects a bad date", {
  expect_equal(nextSnapshotDate("2026-01-01", anchor = "2026-01-01"),
               as.Date("2026-01-31"))
  expect_error(nextSnapshotDate(NA), "single date")
  expect_error(nextSnapshotDate(c("2026-11-07", "2026-12-07")), "single date")
})

test_that("a missed grid day does not shift the next due date", {
  dir <- tempfile("snapshots")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  snapshot_fixture(dir, "2026-11-07")
  # the 2026-12-07 run was missed; the catch-up runs a day late
  expect_true(snapshotDue(dir, today = as.Date("2026-12-08")))
  expect_warning(snapshot_fixture(dir, "2026-12-08"), "missing")

  # before the fix this was due on 2027-01-07, one day off the grid for good
  expect_false(snapshotDue(dir, today = as.Date("2027-01-05")))
  expect_true(snapshotDue(dir, today = as.Date("2027-01-06")))
})

test_that("the snapshot after a late one returns to the grid, with a warning", {
  dir <- tempfile("snapshots")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  snapshot_fixture(dir, "2026-11-07")
  suppressWarnings(snapshot_fixture(dir, "2026-12-08"))
  expect_warning(snapshot_fixture(dir, "2027-01-06"), "counted twice")
  expect_equal(latestSnapshotDate(dir), as.Date("2027-01-06"))
  expect_equal(nextSnapshotDate(latestSnapshotDate(dir)), as.Date("2027-02-05"))
})

test_that("a short interval off the grid is still refused as an overlap", {
  dir <- tempfile("snapshots")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  snapshot_fixture(dir, "2026-11-07")
  suppressWarnings(snapshot_fixture(dir, "2026-12-10"))
  # 2027-01-06 was missed too; 2027-01-07 is off the grid and 28 days on
  expect_false(snapshotDue(dir, today = as.Date("2027-01-07")))
  expect_error(snapshot_fixture(dir, "2027-01-07"), "2027-01-09")
  # a full window later it is allowed again, as an ordinary late snapshot
  expect_true(snapshotDue(dir, today = as.Date("2027-01-09")))
  expect_silent(snapshot_fixture(dir, "2027-01-09"))
})

test_that("snapshotDue and writeDownloadSnapshot agree on every day after a late run", {
  dir <- tempfile("snapshots")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  snapshot_fixture(dir, "2026-11-07")
  suppressWarnings(snapshot_fixture(dir, "2026-12-09"))
  days <- seq(as.Date("2026-12-10"), as.Date("2027-01-20"), by = "day")
  due <- vapply(days, \(d) snapshotDue(dir, today = d), logical(1))
  writable <- vapply(days, \(d) {
    probe <- tempfile("probe")
    dir.create(probe)
    on.exit(unlink(probe, recursive = TRUE), add = TRUE)
    file.copy(dir, probe, recursive = TRUE)
    probe_dir <- file.path(probe, basename(dir))
    tryCatch({
      suppressWarnings(snapshot_fixture(probe_dir, d))
      TRUE
    }, error = function(e) FALSE)
  }, logical(1))
  expect_equal(due, writable)
  expect_equal(days[which(due)[1]], as.Date("2027-01-06"))
})

test_that("checkRdfPayload accepts a bzip2 file above the size floor", {
  path <- tempfile(fileext = ".tar.bz2")
  on.exit(unlink(path))
  writeBin(c(charToRaw("BZh9"), as.raw(rep(0L, 2000L))), path)
  expect_identical(novels:::checkRdfPayload(path, min_bytes = 1000), 2004)
})

test_that("checkRdfPayload refuses a page served in place of the archive", {
  path <- tempfile(fileext = ".tar.bz2")
  on.exit(unlink(path))
  writeLines("<!DOCTYPE html><title>Temporarily Down for Maintenance</title>", path)
  expect_error(novels:::checkRdfPayload(path, min_bytes = 1), "not a bzip2 archive")
})

test_that("checkRdfPayload refuses a truncated archive", {
  path <- tempfile(fileext = ".tar.bz2")
  on.exit(unlink(path))
  writeBin(c(charToRaw("BZh9"), as.raw(rep(0L, 100L))), path)
  expect_error(novels:::checkRdfPayload(path), "truncated")
})

test_that("checkRdfPayload refuses a missing file", {
  expect_error(novels:::checkRdfPayload(tempfile()), "does not exist")
})

test_that("snapshotCollector drops the domain suffix", {
  expect_identical(novels:::snapshotCollector("Johns-MacBook-Pro.local"), "Johns-MacBook-Pro")
  expect_identical(novels:::snapshotCollector("appliance"), "appliance")
  expect_identical(novels:::snapshotCollector("gpu-box.taild554c4.ts.net"), "gpu-box")
})

test_that("snapshotCollector refuses an empty host name", {
  expect_error(novels:::snapshotCollector(""), "no host name")
  expect_error(novels:::snapshotCollector(".local"), "no host name")
})

test_that("snapshotCollector names this machine by default", {
  name <- novels:::snapshotCollector()
  expect_type(name, "character")
  expect_length(name, 1L)
  expect_true(nzchar(name))
})

# Regression, 2026-11-19: R's default 60 s timeout covers the whole transfer,
# so a 127 MB payload at ~95 KB/s failed after 5.7 MB (the Pi's rehearsal).
test_that("downloadRdfPayload allows the payload far longer than R's 60 s default", {
  seen <- NULL
  fake <- function(url, destfile, ...) seen <<- getOption("timeout")
  old <- options(timeout = 60)
  on.exit(options(old))
  novels:::downloadRdfPayload("https://example.invalid/x", tempfile(), download = fake)
  expect_identical(seen, 3600)
})

test_that("downloadRdfPayload restores the caller's timeout, even when the download fails", {
  old <- options(timeout = 60)
  on.exit(options(old))
  fail <- function(...) stop("download failed")
  expect_error(novels:::downloadRdfPayload("https://example.invalid/x", tempfile(),
                                           download = fail), "download failed")
  expect_identical(getOption("timeout"), 60)
})

test_that("downloadRdfPayload never lowers a longer timeout the caller already set", {
  seen <- NULL
  fake <- function(url, destfile, ...) seen <<- getOption("timeout")
  old <- options(timeout = 7200)
  on.exit(options(old))
  novels:::downloadRdfPayload("https://example.invalid/x", tempfile(), download = fake)
  expect_identical(seen, 7200)
})

test_that("downloadRdfPayload writes a real file through the default downloader", {
  src <- tempfile(); dest <- tempfile()
  on.exit(unlink(c(src, dest)))
  writeBin(charToRaw("BZh9payload"), src)
  novels:::downloadRdfPayload(paste0("file://", src), dest)
  expect_identical(readBin(dest, "raw", 100L), charToRaw("BZh9payload"))
})

# ---- feed freshness -------------------------------------------------------

utc <- function(x) as.POSIXct(x, tz = "UTC")

test_that("parseHttpDate reads an IMF-fixdate in UTC, whatever the locale", {
  expect_equal(novels:::parseHttpDate("Sun, 15 Nov 2026 15:40:13 GMT"),
               utc("2026-11-15 15:40:13"))
  expect_equal(novels:::parseHttpDate(" Sun, 01 Feb 2026 00:00:00 GMT\r\n"),
               utc("2026-02-01 00:00:00"))
})

test_that("parseHttpDate gives NA for anything else", {
  for (x in c("", "yesterday", "2026-11-15T15:40:13Z",
              "Sun, 15 Foo 2026 15:40:13 GMT", NA_character_)) {
    expect_true(is.na(novels:::parseHttpDate(x)), info = x)
  }
})

fake_headers <- function(lines, status = 200L) {
  function(url, ...) structure(paste0(lines, "\r\n"), status = status)
}

test_that("feedLastModified reads the final response's header after a redirect", {
  get <- fake_headers(c(
    "HTTP/1.1 302 Found", "Last-Modified: Mon, 02 Nov 2026 00:00:00 GMT",
    "Location: https://mirror/", "",
    "HTTP/1.1 200 OK", "last-modified: Sun, 15 Nov 2026 15:40:13 GMT", ""))
  expect_equal(novels:::feedLastModified("https://x/", headers = get),
               utc("2026-11-15 15:40:13"))
})

test_that("feedLastModified gives NA when the final response has no header", {
  get <- fake_headers(c(
    "HTTP/1.1 302 Found", "Last-Modified: Mon, 02 Nov 2026 00:00:00 GMT", "",
    "HTTP/1.1 200 OK", "Content-Length: 5", ""))
  expect_true(is.na(novels:::feedLastModified("https://x/", headers = get)))
})

test_that("feedLastModified refuses an unavailable feed", {
  get <- fake_headers(c("HTTP/2 503", ""), status = 503L)
  expect_error(novels:::feedLastModified("https://x/", headers = get), "HTTP 503")
})

test_that("checkFeedFreshness accepts a feed up to the limit", {
  now <- utc("2026-12-07 04:00:00")
  expect_equal(novels:::checkFeedFreshness(utc("2026-12-06 04:00:00"), now), 24)
  expect_equal(novels:::checkFeedFreshness(utc("2026-12-05 04:00:00"), now), 48)
})

test_that("checkFeedFreshness refuses a stale feed, such as a frozen mirror", {
  expect_error(
    novels:::checkFeedFreshness(utc("2026-11-15 15:40:13"),
                                now = utc("2026-11-19 21:33:21")),
    "102 hours ago")
  expect_error(
    novels:::checkFeedFreshness(utc("2026-12-05 03:59:59"),
                                now = utc("2026-12-07 04:00:00")),
    "limit 48")
})

test_that("checkFeedFreshness refuses a feed whose age cannot be shown", {
  expect_error(novels:::checkFeedFreshness(utc(NA)), "no usable Last-Modified")
  expect_error(novels:::checkFeedFreshness("2026-11-15"), "POSIXct")
})

# ---- keeping the raw payload ----------------------------------------------

payload_fixture <- function() {
  archive <- tempfile(fileext = ".tar.bz2")
  writeBin(c(charToRaw("BZh9"), as.raw(1:200)), archive)
  list(archive = archive, dir = tempfile("payloads-"),
       sha = digest::digest(archive, algo = "sha256", file = TRUE),
       date = as.Date("2026-12-07"))
}

test_that("keepRdfPayload keeps a verified copy at <dir>/<date>/", {
  f <- payload_fixture()
  kept <- novels:::keepRdfPayload(f$archive, f$date, f$sha, dir = f$dir)
  expect_identical(kept, file.path(f$dir, "2026-12-07", "rdf-files.tar.bz2"))
  expect_identical(digest::digest(kept, algo = "sha256", file = TRUE), f$sha)
  expect_identical(list.files(dirname(kept), all.files = TRUE, no.. = TRUE),
                   "rdf-files.tar.bz2")
  expect_true(file.exists(f$archive))
})

test_that("keepRdfPayload never replaces a kept payload", {
  f <- payload_fixture()
  kept <- novels:::keepRdfPayload(f$archive, f$date, f$sha, dir = f$dir)
  writeBin(charToRaw("BZh9 other"), f$archive)
  other <- digest::digest(f$archive, algo = "sha256", file = TRUE)
  expect_error(novels:::keepRdfPayload(f$archive, f$date, other, dir = f$dir),
               "append-only")
  expect_identical(digest::digest(kept, algo = "sha256", file = TRUE), f$sha)
})

test_that("keepRdfPayload discards a copy that does not match provenance", {
  f <- payload_fixture()
  expect_error(novels:::keepRdfPayload(f$archive, f$date, strrep("0", 64),
                                       dir = f$dir), "discarded")
  expect_length(list.files(file.path(f$dir, "2026-12-07"), all.files = TRUE,
                           no.. = TRUE), 0L)
})

test_that("keepRdfPayload validates its inputs", {
  f <- payload_fixture()
  expect_error(novels:::keepRdfPayload(tempfile(), f$date, f$sha, dir = f$dir),
               "existing file")
  expect_error(novels:::keepRdfPayload(f$archive, "2026-12-07", f$sha, dir = f$dir),
               "Date")
  expect_error(novels:::keepRdfPayload(f$archive, f$date, toupper(f$sha), dir = f$dir),
               "64 hex")
})
