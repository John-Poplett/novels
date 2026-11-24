# Read the 2013 corpus's download counts back from the Wayback Machine
# (TODO.md, "Wayback reader"). For every book, the archive's captures of its
# Gutenberg page nearest the scrape (about May 2012: PREREGISTRATION.md,
# amendment of 2026-11-23, point 1(a)) are fetched, one on each side of
# --target, and their "Downloads" figure read (R/wayback.R).
#
# What it is for: an independent reading of every corpus count, to see how
# many agree and to date the scrape from all 754 books rather than one; and
# the counts of the three patched LibriVox rows' text editions, which the
# corpus never had. For those rows both pages are read: the text edition's
# (source_id, the count the row should carry) and the audio entry's own
# (gutenberg_id, which should match the corpus as the other rows do).
#
# The manifest (committed) has one row per page read: gutenberg_id, read_id,
# side ("before"/"after" --target), timestamp, days (from --target), url,
# downloads, window ("unstated" before 2013-04, "30 days" after), sha256 of
# the page, captures (how many the index listed), fetched_utc, status ("ok",
# "no capture", "no downloads row" or the error). The pages are kept
# byte for byte under --dir (git-ignored). A re-run skips the books whose
# rows are all settled and whose pages are on disk unchanged, so it resumes.
#
# Run from the package root after `tar_make()`:
#
#   Rscript data-raw/wayback-2012.R [--ids=215,19513,...] [--target=2012-05-15]
#       [--from=2011] [--to=2013] [--dir=DIR] [--manifest=FILE] [--delay=SECONDS]
#       [--limit=N]
#
# --ids reads only those corpus books (the mini-test: 215, whose answer is
# known, and the three patched rows); --delay (default 4) spaces the
# requests out, about three per book, so about 15 a minute: at one a second
# the archive refused connections in bursts (2026-11-24, 740 of 1,090
# reads). A refused request is retried after 10, 30 and 90 seconds.

pkgload::load_all(quiet = TRUE)

arg <- function(name, default) {
  v <- sub(paste0("^--", name, "="), "", grep(paste0("^--", name, "="),
                                               commandArgs(trailingOnly = TRUE), value = TRUE))
  if (length(v)) v else default
}
target <- as.Date(arg("target", "2012-05-15"))
from <- arg("from", "2011")
to <- arg("to", "2013")
dir <- arg("dir", "data-raw/wayback")
manifest_path <- arg("manifest", "data-raw/wayback-manifest.csv")
delay <- as.numeric(arg("delay", "4"))
limit <- as.integer(arg("limit", NA))
only <- arg("ids", NA)
if (is.na(target)) stop("--target must be a date, YYYY-MM-DD.", call. = FALSE)
if (is.na(delay) || delay < 0) stop("--delay must be a number of seconds.", call. = FALSE)

options(HTTPUserAgent = "novels-wayback (R; research corpus reproduction)")

ids <- refetchIds(targets::tar_read(novels)$gutenberg_id)
if (!is.na(only)) {
  wanted <- as.integer(strsplit(only, ",", fixed = TRUE)[[1]])
  unknown <- setdiff(wanted, ids$gutenberg_id)
  if (anyNA(wanted) || length(unknown)) {
    stop("--ids must be corpus gutenberg_ids, comma-separated; not in the corpus: ",
         paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  ids <- ids[ids$gutenberg_id %in% wanted, ]
}
# The pages to read: the source text's, and a patched row's own as well.
reads <- do.call(rbind, lapply(seq_len(nrow(ids)), function(i) {
  data.frame(gutenberg_id = ids$gutenberg_id[i],
             read_id = unique(c(ids$source_id[i], ids$gutenberg_id[i])))
}))

columns <- c("gutenberg_id", "read_id", "side", "timestamp", "days", "url", "downloads",
             "window", "sha256", "captures", "fetched_utc", "status")
manifest <- if (file.exists(manifest_path)) {
  readr::read_csv(manifest_path, col_types = readr::cols(.default = "c"))
} else {
  as.data.frame(setNames(replicate(length(columns), character(), simplify = FALSE), columns))
}

pagePath <- function(read_id, timestamp) file.path(dir, read_id, paste0(timestamp, ".html"))

# A read is settled when it has rows, none of them an error, and every page
# it fetched is on disk unchanged.
isDone <- function(rows) {
  if (!nrow(rows) || any(startsWith(rows$status, "error"))) return(FALSE)
  fetched <- rows[!is.na(rows$sha256) & nzchar(rows$sha256), ]
  all(vapply(seq_len(nrow(fetched)), function(j) {
    path <- pagePath(fetched$read_id[j], fetched$timestamp[j])
    file.exists(path) &&
      identical(digest::digest(path, algo = "sha256", file = TRUE), fetched$sha256[j])
  }, logical(1)))
}
todo <- reads[!vapply(seq_len(nrow(reads)), function(i) {
  isDone(manifest[manifest$gutenberg_id == as.character(reads$gutenberg_id[i]) &
                    manifest$read_id == as.character(reads$read_id[i]), ])
}, logical(1)), ]
if (!is.na(limit)) todo <- utils::head(todo, limit)
message(nrow(reads) - nrow(todo), " of ", nrow(reads), " pages' reads already settled; ",
        "reading ", nrow(todo), " (target ", format(target), ")")

stamp <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

readOne <- function(gutenberg_id, read_id) {
  base <- list(gutenberg_id = gutenberg_id, read_id = read_id, side = NA, timestamp = NA,
               days = NA, url = NA, downloads = NA, window = NA, sha256 = NA,
               captures = NA, fetched_utc = stamp(), status = "ok")
  index <- tryCatch({
    tmp <- tempfile(fileext = ".txt")
    on.exit(unlink(tmp))
    withRetry(function() downloadWithTimeout(waybackIndexUrl(read_id, from, to), tmp,
                                             timeout = 120))
    waybackCaptures(readLines(tmp, warn = FALSE))
  }, error = function(e) e)
  Sys.sleep(delay)
  if (inherits(index, "error")) {
    base$status <- paste("error: index:", conditionMessage(index))
    return(as.data.frame(lapply(base, as.character)))
  }
  base$captures <- nrow(index)
  bracket <- waybackBracket(index, target)
  if (!nrow(bracket)) {
    base$status <- "no capture"
    return(as.data.frame(lapply(base, as.character)))
  }
  rows <- lapply(seq_len(nrow(bracket)), function(j) {
    row <- base
    row$side <- bracket$side[j]
    row$timestamp <- bracket$timestamp[j]
    row$days <- round(bracket$days[j], 1)
    row$url <- waybackPageUrl(read_id, bracket$timestamp[j])
    row$fetched_utc <- stamp()
    result <- tryCatch({
      dest <- pagePath(read_id, bracket$timestamp[j])
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      partial <- paste0(dest, ".partial")
      withRetry(function() downloadWithTimeout(row$url, partial, timeout = 120))
      if (!file.size(partial)) stop("the page is empty")
      file.rename(partial, dest)
      row$sha256 <- digest::digest(dest, algo = "sha256", file = TRUE)
      d <- waybackDownloads(readLines(dest, warn = FALSE))
      row$downloads <- d$downloads
      row$window <- d$window
      if (is.na(d$downloads)) row$status <- "no downloads row"
      row
    }, error = function(e) {
      row$status <- paste("error:", conditionMessage(e))
      row
    })
    Sys.sleep(delay)
    as.data.frame(lapply(result, as.character))
  })
  do.call(rbind, rows)
}

for (i in seq_len(nrow(todo))) {
  rows <- readOne(todo$gutenberg_id[i], todo$read_id[i])
  keep <- !(manifest$gutenberg_id == as.character(todo$gutenberg_id[i]) &
              manifest$read_id == as.character(todo$read_id[i]))
  manifest <- rbind(manifest[keep, columns], rows[columns])
  manifest <- manifest[order(as.integer(manifest$gutenberg_id), as.integer(manifest$read_id),
                             manifest$side), ]
  readr::write_csv(manifest, manifest_path, na = "")
  bad <- rows$status[rows$status != "ok"]
  if (length(bad)) message(todo$gutenberg_id[i], "/", todo$read_id[i], ": ", bad[1])
  if (i %% 50 == 0) message(i, " of ", nrow(todo))
}

errors <- sum(startsWith(manifest$status, "error"))
message(nrow(manifest), " rows in ", manifest_path, "; ", errors, " error(s), ",
        sum(manifest$status == "no capture"), " book page(s) without a capture")
if (errors) {
  message("Re-run to retry the errors; see the status column.")
  quit(status = 1)
}
