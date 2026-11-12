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
