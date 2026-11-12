# Take one download snapshot of the whole Project Gutenberg catalogue.
#
# The counter is a trailing 30-day flow (unique address-days, robots excluded),
# so a snapshot every 30 days is sufficient: consecutive windows do not
# overlap, and the sum over K snapshots is the downloads over K months.
#
# The source is Project Gutenberg's own RDF dump, regenerated daily:
#
#   https://www.gutenberg.org/cache/epub/feeds/rdf-files.tar.bz2
#
# Measured 2026-11-07: 121 MB compressed, 2.1 GB extracted, 79,762 records,
# 33 s to unpack and 44 s to parse on this laptop. The extracted tree is a
# temporary; only the two CSV files and the provenance line are kept.
#
# Run from the package root, as often as you like, and let the script decide:
#
#   Rscript data-raw/snapshot-downloads.R
#
# The schedule is a fixed 30-day grid anchored at the first snapshot, not a
# calendar rule, because the counter's window is 30 days: sampling on the same
# day of each month gives a 28-day interval in February (two days counted
# twice) and 31-day intervals elsewhere (a one-day hole). snapshotDue() answers
# "is it time?" from the series itself, so a daily cron job is enough; a run on
# a day that is not due exits quietly, before downloading anything.
#
# The script refuses a second snapshot for the same date, and refuses one that
# would overlap the previous window. Snapshots are append-only -- a re-run
# cannot replace an observation.
#
# What this does NOT record: the catalogue's Type and Language, which decide
# wave eligibility. Those are frozen from pg_catalog.csv on 2026-12-31, as
# PREREGISTRATION.md section 13 specifies.

suppressMessages({
  library(digest)
  library(readr)
})
pkgload::load_all(quiet = TRUE)

run_date <- as.Date(format(Sys.time(), "%Y-%m-%d", tz = "UTC"))
latest <- latestSnapshotDate()
if (!snapshotDue(today = run_date)) {
  cat("No snapshot due: the latest is", format(latest), "and the next is",
      format(latest + SNAPSHOT_INTERVAL_DAYS), "\n")
  quit(save = "no", status = 0)
}
interval_days <- if (is.na(latest)) NA_integer_ else as.integer(run_date - latest)

source_url <- "https://www.gutenberg.org/cache/epub/feeds/rdf-files.tar.bz2"
min_records <- 70000L   # 79,762 on 2026-11-07; a collapse means a changed feed

work_dir <- tempfile("pg-rdf-")
dir.create(work_dir)
on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

archive <- file.path(work_dir, "rdf-files.tar.bz2")
message("downloading ", source_url)
utils::download.file(source_url, archive, mode = "wb", quiet = TRUE)

payload_bytes <- file.info(archive)$size
payload_sha256 <- digest::digest(archive, algo = "sha256", file = TRUE)
message(sprintf("  %s bytes, sha256 %s", format(payload_bytes, big.mark = ","),
                payload_sha256))

extract_dir <- file.path(work_dir, "rdf")
dir.create(extract_dir)
message("unpacking")
utils::untar(archive, exdir = extract_dir)

rdf_paths <- list.files(
  file.path(extract_dir, "cache", "epub"),
  pattern = "[.]rdf$", recursive = TRUE, full.names = TRUE
)
if (length(rdf_paths) < min_records) {
  stop(
    "Only ", length(rdf_paths), " RDF records were unpacked, below the ",
    min_records, " floor. The feed has changed or the download is truncated; ",
    "refusing to write a snapshot.",
    call. = FALSE
  )
}

message("parsing ", format(length(rdf_paths), big.mark = ","), " records")
books <- readGutenbergRdf(rdf_paths)

if (nrow(books) != length(rdf_paths)) {
  stop("Parsed ", nrow(books), " rows from ", length(rdf_paths), " files.",
       call. = FALSE)
}
total_downloads <- sum(books$downloads)
if (total_downloads <= 0) {
  stop("Total downloads is ", total_downloads, "; refusing to write a snapshot.",
       call. = FALSE)
}

snapshot_date <- run_date
snapshot_path <- writeDownloadSnapshot(
  books,
  snapshot_date = snapshot_date,
  source = "gutenberg-rdf-files.tar.bz2"
)

provenance <- c(
  paste0("source_url: ", source_url),
  paste0("retrieved_at_utc: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
  paste0("payload_bytes: ", payload_bytes),
  paste0("payload_sha256: ", payload_sha256),
  paste0("rdf_files: ", length(rdf_paths)),
  paste0("rows_written: ", nrow(books)),
  paste0("total_downloads: ", total_downloads),
  paste0("interval_days: ",
         if (is.na(interval_days)) "first snapshot" else interval_days)
)
writeLines(provenance, file.path(dirname(snapshot_path), "provenance.txt"))

cat("\nsnapshot written:", snapshot_path, "\n")
cat("  books:", format(nrow(books), big.mark = ","), "\n")
cat("  total 30-day downloads:", format(total_downloads, big.mark = ","), "\n")
cat("  median:", format(stats::median(books$downloads), big.mark = ","), "\n")
