# Re-fetch every 2013 book from a Project Gutenberg mirror, in its original
# case (TODO.md, "Pristine text from scratch, now, with the 2013 corpus as
# the answer key"). Step 1 of that item: fetch and keep the raw files.
# Stripping and the comparison with the 2013 texts come after.
#
# gutenberg.org has returned 503 since at least 2026-11-17, so the texts come
# from a mirror. That is safe for these books, unlike the catalogue: mirrors
# lag by weeks, and none of these texts has changed in years. The mirror and
# every URL are recorded.
#
# For each book: the mirror directory is listed, one text file is chosen
# (UTF-8, then 8-bit, then ASCII; R/refetch.R) and kept byte for byte at
# <dir>/<source_id>/<file>, written under a .partial name and renamed when
# complete. The names of the earlier editions in the directory's old/ folder
# are recorded but not fetched; they are where to look when a 2013 text
# does not match today's.
#
# The manifest (committed) has one row per book: gutenberg_id (the corpus's
# id), source_id (the id fetched: a patched LibriVox row's replacement),
# mirror, url, file, encoding, bytes, sha256, fetched_utc, old_editions,
# status ("ok" or the error). A re-run fetches only the books without an "ok"
# row whose file is on disk with the recorded SHA-256, so it resumes.
#
# Run from the package root after `tar_make()`:
#
#   Rscript data-raw/refetch-2013.R [--mirror=URL] [--dir=DIR] [--delay=SECONDS]
#       [--manifest=FILE] [--limit=N]
#
# The default mirror is https://gutenberg.pglaf.org; --delay (default 1)
# spaces the requests out; --limit fetches at most N books (for a trial).
# The raw files (about 300 MB) are git-ignored.

pkgload::load_all(quiet = TRUE)

arg <- function(name, default) {
  v <- sub(paste0("^--", name, "="), "", grep(paste0("^--", name, "="),
                                               commandArgs(trailingOnly = TRUE), value = TRUE))
  if (length(v)) v else default
}
mirror <- sub("/+$", "", arg("mirror", "https://gutenberg.pglaf.org"))
dir <- arg("dir", "data-raw/refetch")
manifest_path <- arg("manifest", "data-raw/refetch-manifest.csv")
delay <- as.numeric(arg("delay", "1"))
limit <- as.integer(arg("limit", NA))
if (is.na(delay) || delay < 0) stop("--delay must be a number of seconds.", call. = FALSE)

options(HTTPUserAgent = "novels-refetch (R; research corpus reproduction)")

ids <- refetchIds(targets::tar_read(novels)$gutenberg_id)

columns <- c("gutenberg_id", "source_id", "mirror", "url", "file", "encoding", "bytes",
             "sha256", "fetched_utc", "old_editions", "status")
manifest <- if (file.exists(manifest_path)) {
  readr::read_csv(manifest_path, col_types = readr::cols(.default = "c"))
} else {
  as.data.frame(setNames(replicate(length(columns), character(), simplify = FALSE), columns))
}

# A book is done when its row is "ok" and its file is on disk, unchanged.
isDone <- function(row) {
  if (!nrow(row) || row$status != "ok") return(FALSE)
  path <- file.path(dir, row$source_id, row$file)
  file.exists(path) && identical(digest::digest(path, algo = "sha256", file = TRUE), row$sha256)
}
todo <- ids[!vapply(seq_len(nrow(ids)), function(i) {
  isDone(manifest[manifest$gutenberg_id == as.character(ids$gutenberg_id[i]), ])
}, logical(1)), ]
done <- nrow(ids) - nrow(todo)
if (!is.na(limit)) todo <- utils::head(todo, limit)
message(done, " of ", nrow(ids), " already fetched; fetching ", nrow(todo),
        " from ", mirror)

listing <- function(url) {
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp))
  downloadWithTimeout(url, tmp, timeout = 120)
  listingEntries(readLines(tmp, warn = FALSE))
}

fetchOne <- function(gutenberg_id, source_id) {
  base <- paste0(mirror, "/", mirrorDir(source_id), "/")
  row <- list(gutenberg_id = gutenberg_id, source_id = source_id, mirror = mirror,
              url = NA, file = NA, encoding = NA, bytes = NA, sha256 = NA,
              fetched_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
              old_editions = NA, status = "ok")
  result <- tryCatch({
    entries <- listing(base)
    pick <- pickTextFile(entries, source_id)
    Sys.sleep(delay)
    if ("old/" %in% entries) {
      old <- listing(paste0(base, "old/"))
      row$old_editions <- paste(old[grepl("\\.txt$", old)], collapse = ";")
      Sys.sleep(delay)
    } else {
      row$old_editions <- ""
    }
    dest_dir <- file.path(dir, source_id)
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    dest <- file.path(dest_dir, pick$file)
    partial <- paste0(dest, ".partial")
    row$url <- paste0(base, pick$file)
    downloadWithTimeout(row$url, partial, timeout = 600)
    if (!file.size(partial)) stop("the file is empty")
    file.rename(partial, dest)
    row$file <- pick$file
    row$encoding <- pick$encoding
    row$bytes <- file.size(dest)
    row$sha256 <- digest::digest(dest, algo = "sha256", file = TRUE)
    row
  }, error = function(e) {
    row$status <- paste("error:", conditionMessage(e))
    row
  })
  Sys.sleep(delay)
  as.data.frame(lapply(result, as.character))
}

for (i in seq_len(nrow(todo))) {
  row <- fetchOne(todo$gutenberg_id[i], todo$source_id[i])
  manifest <- rbind(manifest[manifest$gutenberg_id != row$gutenberg_id, columns], row[columns])
  manifest <- manifest[order(as.integer(manifest$gutenberg_id)), ]
  readr::write_csv(manifest, manifest_path, na = "")
  if (row$status != "ok") message(row$gutenberg_id, ": ", row$status)
  if (i %% 50 == 0) message(i, " of ", nrow(todo))
}

ok <- sum(manifest$status == "ok")
message(ok, " of ", nrow(ids), " books fetched; manifest ", manifest_path)
if (ok < nrow(ids)) {
  message("Re-run to retry the ", nrow(ids) - ok, " failure(s); see the status column.")
  quit(status = 1)
}
