# Freeze the catalogue that fixes wave eligibility.
#
# PREREGISTRATION.md section 3 fixes eligibility "at 2026-12-31" from Project
# Gutenberg's catalogue: Type, Language, Issued and Bookshelves (the genre
# allow-list keys on Bookshelves). The 2026-11-20 amendment (section 11)
# allows for an outage: the catalogue counts if Project Gutenberg regenerated
# it on or after 2026-12-31 (its Last-Modified) and it is retrieved by
# 2027-01-31. Mirrors are never used.
#
#   Rscript data-raw/freeze-catalogue.R                  # the freeze, once
#   Rscript data-raw/freeze-catalogue.R --dry-run        # rehearse, write nothing
#   Rscript data-raw/freeze-catalogue.R --dry-run --source URL
#
# A real run enforces every rule and writes data-raw/catalogue-freeze/ exactly
# once: the file as downloaded (pg_catalog.csv.gz, bit for bit, so its SHA-256
# pins it) and provenance.txt. It refuses to run if that directory exists.
# Afterwards: commit both files, record the SHA-256 in PREREGISTRATION.md
# section 13, and upload a copy to B2 (OPERATIONS.md, section G).
#
# --dry-run reports which rule a real run would refuse on, then downloads and
# validates anyway, writing only to a temporary directory. --source is
# accepted only with --dry-run, so a mirror can rehearse the parsing but never
# feed the freeze.

suppressMessages(library(digest))
pkgload::load_all(quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
source_at <- match("--source", args)
official_url <- "https://www.gutenberg.org/cache/epub/feeds/pg_catalog.csv.gz"
source_url <- if (is.na(source_at)) official_url else args[source_at + 1L]
known <- c("--dry-run", "--source", if (!is.na(source_at)) source_url)
if (length(setdiff(args, known)) > 0 || is.na(source_url)) {
  stop("usage: freeze-catalogue.R [--dry-run [--source URL]]", call. = FALSE)
}
if (!dry_run && source_url != official_url) {
  stop("--source is for --dry-run only: the freeze reads Project Gutenberg's ",
       "own catalogue, never a mirror (PREREGISTRATION.md section 11, ",
       "2026-11-20).", call. = FALSE)
}

freeze_dir <- "data-raw/catalogue-freeze"
if (!dry_run && dir.exists(freeze_dir)) {
  stop(freeze_dir, " already exists: the catalogue is frozen once. Nothing ",
       "was written.", call. = FALSE)
}

run_date <- as.Date(format(Sys.time(), "%Y-%m-%d", tz = "UTC"))
collector <- novels:::snapshotCollector()
last_modified <- novels:::feedLastModified(source_url, consequence = paste(
  "Nothing was written. Retry later; the catalogue may be retrieved until",
  format(novels:::CATALOGUE_FREEZE_DEADLINE), "(PREREGISTRATION.md section 11)."))
message(sprintf("%s regenerated %s", source_url,
                format(last_modified, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))
timing <- tryCatch(novels:::checkCatalogueTiming(last_modified, run_date),
                   error = function(e) e)
if (inherits(timing, "error")) {
  if (!dry_run) stop(timing)
  message("DRY RUN: a real run would refuse here:\n  ", conditionMessage(timing))
} else {
  message("timing: accepted (regenerated on or after ",
          format(novels:::CATALOGUE_FREEZE_DATE), ", retrieved by ",
          format(novels:::CATALOGUE_FREEZE_DEADLINE), ")")
}

work_dir <- tempfile("catalogue-")
dir.create(work_dir)
on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
download <- file.path(work_dir, "pg_catalog.csv.gz")
novels:::downloadWithTimeout(source_url, download)
catalogue <- novels:::readCatalogue(download)
counts <- novels:::catalogueCounts(catalogue)
payload_sha256 <- digest::digest(download, algo = "sha256", file = TRUE)

provenance <- c(
  paste0("source_url: ", source_url),
  paste0("collector: ", collector),
  paste0("feed_last_modified_utc: ",
         format(last_modified, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
  paste0("retrieved_at_utc: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
  paste0("payload_bytes: ", file.info(download)$size),
  paste0("payload_sha256: ", payload_sha256),
  paste0("columns: ", paste(names(catalogue), collapse = ",")),
  paste0(names(counts), ": ", counts),
  "rules: Type == Text, Language == en, Issued <= 2026-12-31 (PREREGISTRATION.md section 3)"
)

if (dry_run) {
  cat("\nDRY RUN, nothing kept. Would write", freeze_dir, "with:\n")
  cat(paste0("  ", provenance), sep = "\n")
  quit(save = "no", status = 0)
}

partial <- paste0(freeze_dir, ".partial")
unlink(partial, recursive = TRUE)
dir.create(partial)
stopifnot(file.copy(download, file.path(partial, "pg_catalog.csv.gz")))
stopifnot(identical(
  digest::digest(file.path(partial, "pg_catalog.csv.gz"), algo = "sha256", file = TRUE),
  payload_sha256))
writeLines(provenance, file.path(partial, "provenance.txt"))
stopifnot(file.rename(partial, freeze_dir))

cat("\ncatalogue frozen:", freeze_dir, "\n")
cat(paste0("  ", provenance), sep = "\n")
cat("\nNext: commit", freeze_dir, "; record payload_sha256 in PREREGISTRATION.md",
    "section 13; upload a copy to B2 (OPERATIONS.md, section G).\n")
