# Recording Project Gutenberg's trailing 30-day download counter.
#
# Project Gutenberg publishes one number per book: downloads during the last
# 30 days, counted as unique address-days, with addresses that fetch more than
# 100 ebooks in a day excluded as robots. It is a flow, not a stock -- there is
# no cumulative figure anywhere -- and it is the only public per-book counter.
#
# A usable series comes from sampling it every 30 days. Consecutive 30-day
# windows are then disjoint, so the sum of K snapshots is the book's downloads
# over K months without deconvolution. Daily snapshots are only needed for
# daily resolution, which no analysis here uses.
#
# The source is Project Gutenberg's own RDF dump rather than a mirror, so the
# provenance chain stays on one host:
#
#   https://www.gutenberg.org/cache/epub/feeds/rdf-files.tar.bz2
#
# It is regenerated daily (~121 MB, one pg<id>.rdf per ebook). Gutendex carries
# the same numbers and is convenient, but a multi-year series should not
# depend on a third party's copy of it.

# The counter reports a trailing 30-day window, so this is both the window
# length and the only interval that makes consecutive snapshots disjoint.
# A calendar rule cannot express it: the same day of each month is 28 days in
# February (two days counted twice) and 31 days elsewhere (a one-day hole).
SNAPSHOT_INTERVAL_DAYS <- 30L

# The grid's origin: the trial snapshot. Every scheduled snapshot falls on
# SNAPSHOT_ANCHOR + 30k (PREREGISTRATION.md section 2 lists them). Scheduling
# from the latest snapshot instead would let one late run shift every later
# date off the published grid.
SNAPSHOT_ANCHOR <- as.Date("2026-11-07")

#' Next date on the snapshot grid
#'
#' The schedule is a fixed grid, `anchor + 30k`, not "30 days after the last
#' snapshot". This returns the first grid date strictly after `after`, so a
#' late snapshot (one taken off the grid) is followed by the next grid date,
#' not by a date 30 days after the late one.
#'
#' @param after the date to step from, `Date` or `"YYYY-MM-DD"`, usually the
#'   latest snapshot
#' @param anchor the grid's origin (default `SNAPSHOT_ANCHOR`, 2026-11-07)
#' @returns a `Date`
#' @seealso snapshotDue, writeDownloadSnapshot
#' @examples
#' nextSnapshotDate("2026-11-07")  # 2026-12-07, on the grid
#' nextSnapshotDate("2026-12-08")  # 2027-01-06: a late run returns to the grid
#' @export
nextSnapshotDate <- function(after, anchor = SNAPSHOT_ANCHOR) {
  after <- as.Date(after)
  anchor <- as.Date(anchor)
  stopifnot(
    "after must be a single date" = length(after) == 1 && !is.na(after),
    "anchor must be a single date" = length(anchor) == 1 && !is.na(anchor)
  )
  steps <- floor(as.numeric(after - anchor) / SNAPSHOT_INTERVAL_DAYS) + 1
  anchor + steps * SNAPSHOT_INTERVAL_DAYS
}

#' Is `date` on the snapshot grid?
#' @noRd
onSnapshotGrid <- function(date, anchor = SNAPSHOT_ANCHOR) {
  as.numeric(as.Date(date) - as.Date(anchor)) %% SNAPSHOT_INTERVAL_DAYS == 0
}

#' May a snapshot dated `date` follow one dated `previous`?
#'
#' Not before the next grid date; and, after it, only if the interval is a
#' full window or the date is on the grid (a return after a late snapshot).
#' The one rule behind both snapshotDue() and writeDownloadSnapshot(), so the
#' scheduler never downloads a payload the writer would refuse.
#' @noRd
snapshotAllowed <- function(previous, date, anchor = SNAPSHOT_ANCHOR) {
  date >= nextSnapshotDate(previous, anchor) &&
    (as.numeric(date - previous) >= SNAPSHOT_INTERVAL_DAYS ||
       onSnapshotGrid(date, anchor))
}

#' The first date on or after `date` that snapshotAllowed() accepts
#' @noRd
nextAllowedSnapshot <- function(previous, date, anchor = SNAPSHOT_ANCHOR) {
  next_grid <- nextSnapshotDate(previous, anchor)
  if (date < next_grid) next_grid else previous + SNAPSHOT_INTERVAL_DAYS
}

# The smallest RDF payload accepted as complete. The feed was 121 MB on
# 2026-11-07 and only grows; a file under half that is a truncated download.
RDF_PAYLOAD_MIN_BYTES <- 60e6

#' Refuse an RDF payload that is not a plausible bzip2 archive
#'
#' Checked before unpacking, so a truncated download or a page served in
#' place of the archive stops with a message that names the cause, instead of
#' failing inside `untar()`. (An outage or a stale feed is caught before the
#' download, by [feedLastModified()] and [checkFeedFreshness()].)
#'
#' @param path the downloaded file
#' @param min_bytes smallest size accepted as a complete payload
#' @returns the file size in bytes, invisibly
#' @noRd
checkRdfPayload <- function(path, min_bytes = RDF_PAYLOAD_MIN_BYTES) {
  stopifnot(
    "path must be a single string" = is.character(path) && length(path) == 1,
    "min_bytes must be a single positive number" =
      is.numeric(min_bytes) && length(min_bytes) == 1 && min_bytes > 0
  )
  if (!file.exists(path)) {
    stop("The RDF payload ", path, " does not exist; the download did not ",
         "produce a file. Re-run later; no snapshot was written.", call. = FALSE)
  }
  magic <- readBin(path, "raw", n = 3L)
  if (!identical(magic, charToRaw("BZh"))) {
    stop("The RDF payload ", path, " is not a bzip2 archive (it does not ",
         "start with 'BZh'). Project Gutenberg probably served a page in its ",
         "place. Refusing to unpack; no snapshot was written. Re-run later.",
         call. = FALSE)
  }
  size <- file.info(path)$size
  if (size < min_bytes) {
    stop("The RDF payload ", path, " is ", format(size, big.mark = ","),
         " bytes, below the ", format(min_bytes, big.mark = ",",
                                       scientific = FALSE),
         "-byte floor (the feed was 121 MB on 2026-11-07), so the download is ",
         "probably truncated. No snapshot was written. Re-run later.",
         call. = FALSE)
  }
  invisible(size)
}

#' Download the RDF payload with a timeout sized for it
#'
#' `download.file()` gives up after `getOption("timeout")` seconds, 60 by
#' default, measured over the whole transfer -- under 2 MB/s that is less than
#' the 121 MB payload, so a slow night would fail every night. This raises the
#' limit for this one call and restores it afterwards.
#'
#' @param url the payload's URL
#' @param dest where to write it
#' @param timeout seconds allowed for the whole transfer
#' @param download the downloader (injected by tests)
#' @returns `dest`, invisibly
#' @noRd
downloadRdfPayload <- function(url, dest, timeout = 3600,
                               download = utils::download.file) {
  stopifnot(
    "url must be a single string" = is.character(url) && length(url) == 1,
    "dest must be a single string" = is.character(dest) && length(dest) == 1,
    "timeout must be a single positive number of seconds" =
      is.numeric(timeout) && length(timeout) == 1 && timeout > 0
  )
  old <- options(timeout = max(timeout, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  download(url, dest, mode = "wb", quiet = TRUE)
  invisible(dest)
}

# The oldest feed accepted. Project Gutenberg regenerates the dump daily; a
# feed older than two days is a stalled generator or a frozen mirror, and its
# counts belong to an earlier window than the snapshot's date would claim.
FEED_MAX_AGE_HOURS <- 48

#' Parse an HTTP date (RFC 9110 IMF-fixdate) to UTC
#'
#' Locale-independent: `strptime()`'s `%a` and `%b` follow the session's
#' locale, so month names are matched here instead.
#'
#' @param x a single string such as `"Sun, 15 Nov 2026 15:40:13 GMT"`
#' @returns a POSIXct in UTC, or `NA` if `x` is not in that format
#' @noRd
parseHttpDate <- function(x) {
  stopifnot("x must be a single string" = is.character(x) && length(x) == 1)
  pattern <- paste0("^\\s*[A-Za-z]{3}, (\\d{2}) ([A-Za-z]{3}) (\\d{4}) ",
                    "(\\d{2}:\\d{2}:\\d{2}) GMT\\s*$")
  if (is.na(x) || !grepl(pattern, x, perl = TRUE)) {
    return(as.POSIXct(NA, tz = "UTC"))
  }
  parts <- regmatches(x, regexec(pattern, x, perl = TRUE))[[1]]
  month <- match(tolower(parts[3]), tolower(month.abb))
  if (is.na(month)) return(as.POSIXct(NA, tz = "UTC"))
  as.POSIXct(sprintf("%s-%02d-%s %s", parts[4], month, parts[2], parts[5]),
             format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

#' When the feed was last regenerated, from its `Last-Modified` header
#'
#' A HEAD request, made before the hour-long download, so an outage or a
#' stale feed is refused at once. After redirects, only the final response's
#' headers count.
#'
#' @param url the payload's URL
#' @param headers the header fetcher (injected by tests); returns the header
#'   lines with the final HTTP status as attribute `"status"`
#' @returns a POSIXct in UTC, `NA` when the response has no usable header
#' @noRd
feedLastModified <- function(url, headers = curlGetHeaders) {
  stopifnot("url must be a single string" = is.character(url) && length(url) == 1)
  lines <- headers(url, redirect = TRUE, timeout = 120L)
  status <- attr(lines, "status")
  if (!identical(as.integer(status), 200L)) {
    stop("The feed ", url, " answered HTTP ", status, " to a HEAD request, ",
         "so it is unavailable. No snapshot was written; the next nightly ",
         "run retries (a late snapshot keeps its own date).", call. = FALSE)
  }
  starts <- grep("^HTTP/", lines)
  final <- if (length(starts)) lines[max(starts):length(lines)] else lines
  field <- grep("^last-modified:", final, ignore.case = TRUE, value = TRUE)
  if (length(field) == 0L) return(as.POSIXct(NA, tz = "UTC"))
  parseHttpDate(sub("^[^:]*:", "", utils::tail(field, 1L)))
}

#' Refuse a feed that is not fresh
#'
#' The counter is a trailing 30-day flow, so a stale feed would record an
#' earlier window under this snapshot's date -- a silently wrong observation,
#' where a missed night is only a late one. A feed without a usable
#' `Last-Modified` is refused for the same reason: its age cannot be shown.
#'
#' @param last_modified POSIXct, from [feedLastModified()]
#' @param now the current time
#' @param max_age_hours the oldest feed accepted
#' @returns the feed's age in hours, invisibly
#' @noRd
checkFeedFreshness <- function(last_modified, now = Sys.time(),
                               max_age_hours = FEED_MAX_AGE_HOURS) {
  stopifnot(
    "last_modified must be a single POSIXct" =
      inherits(last_modified, "POSIXct") && length(last_modified) == 1,
    "now must be a single non-missing POSIXct" =
      inherits(now, "POSIXct") && length(now) == 1 && !is.na(now),
    "max_age_hours must be a single positive number" =
      is.numeric(max_age_hours) && length(max_age_hours) == 1 && max_age_hours > 0
  )
  if (is.na(last_modified)) {
    stop("The feed sent no usable Last-Modified header, so its age cannot be ",
         "shown and a stale feed cannot be ruled out. No snapshot was ",
         "written. Check the feed by hand (curl -I) and see OPERATIONS.md.",
         call. = FALSE)
  }
  age <- as.numeric(difftime(now, last_modified, units = "hours"))
  if (age > max_age_hours) {
    stop(sprintf(paste0(
      "The feed was last regenerated %s UTC, %.0f hours ago (limit %g), so ",
      "its counts cover an earlier window than today's. No snapshot was ",
      "written; the next nightly run retries. If it stays stale, see ",
      "OPERATIONS.md -- do not switch to a mirror without amending the ",
      "pre-registration."),
      format(last_modified, "%Y-%m-%d %H:%M:%S", tz = "UTC"), age,
      max_age_hours), call. = FALSE)
  }
  invisible(age)
}

#' The collecting machine's name, for provenance
#'
#' The short host name (domain suffix such as `.local` dropped), so each
#' snapshot records which collector took it (OPERATIONS.md).
#'
#' @param nodename the full host name (default: this machine's)
#' @returns a single non-empty string
#' @noRd
snapshotCollector <- function(nodename = Sys.info()[["nodename"]]) {
  stopifnot("nodename must be a single string" =
              is.character(nodename) && length(nodename) == 1)
  name <- sub("[.].*$", "", nodename)
  if (is.na(name) || !nzchar(name)) {
    stop("This machine reports no host name, so the snapshot cannot record ",
         "its collector. Set one (e.g. hostnamectl set-hostname) and re-run.",
         call. = FALSE)
  }
  name
}

#' Read Project Gutenberg RDF records
#'
#' Extracts the download count and the catalogue's `Issued` date from one or
#' more `pg<id>.rdf` files. The files are machine-generated by Project
#' Gutenberg, so the fields are read positionally from the element bodies
#' rather than through a general XML parser; the cost of a full catalogue pass
#' is seconds, not minutes.
#'
#' The function fails rather than returning a partial table. A changed feed
#' that produced no download counts would otherwise look like a successful
#' snapshot of a catalogue nobody reads.
#'
#' @param paths character vector of paths to `pg<id>.rdf` files
#' @returns a tibble with `gutenberg_id` (integer), `downloads` (integer,
#'   trailing 30 days) and `date_added` (Date, the ebook's release date)
#' @seealso writeDownloadSnapshot, readDownloadSnapshots
#' @export
readGutenbergRdf <- function(paths) {
  if (!is.character(paths) || length(paths) == 0) {
    stop("paths must be a non-empty character vector of pg<id>.rdf files.",
         call. = FALSE)
  }
  missing_paths <- paths[!file.exists(paths)]
  if (length(missing_paths) > 0) {
    stop("RDF file(s) not found: ", paste(utils::head(missing_paths, 3), collapse = ", "),
         call. = FALSE)
  }

  extract_body <- function(pattern, text) {
    match <- regmatches(text, regexpr(pattern, text))
    if (length(match) == 0) NA_character_ else sub(pattern, "\\1", match)
  }

  fields <- vapply(paths, function(path) {
    text <- paste(readLines(path, warn = FALSE), collapse = "")
    c(
      downloads = extract_body(
        "<pgterms:downloads[^>]*>([0-9]+)</pgterms:downloads>", text
      ),
      issued = extract_body(
        "<dcterms:issued[^>]*>([0-9]{4}-[0-9]{2}-[0-9]{2})</dcterms:issued>", text
      )
    )
  }, character(2), USE.NAMES = FALSE)

  ids <- as.integer(sub("^pg([0-9]+)[.]rdf$", "\\1", basename(paths)))
  downloads <- suppressWarnings(as.integer(fields[1, ]))
  issued <- fields[2, ]

  if (anyNA(ids)) {
    stop("Cannot read a gutenberg_id from file name: ",
         basename(paths[which(is.na(ids))[1]]), call. = FALSE)
  }
  if (anyDuplicated(ids) > 0) {
    stop("Duplicate gutenberg_id in input: ", ids[anyDuplicated(ids)],
         call. = FALSE)
  }
  if (anyNA(downloads)) {
    first <- paths[which(is.na(downloads))[1]]
    stop("No <pgterms:downloads> value in ", basename(first),
         ". The feed's shape may have changed; refusing to write a snapshot ",
         "with missing counts.", call. = FALSE)
  }
  if (anyNA(issued)) {
    first <- paths[which(is.na(issued))[1]]
    stop("No <dcterms:issued> value in ", basename(first),
         ". The feed's shape may have changed; refusing to guess date_added.",
         call. = FALSE)
  }

  tibble::tibble(
    gutenberg_id = ids,
    downloads = downloads,
    date_added = as.Date(issued)
  )
}

#' Date of the most recent snapshot in a series
#'
#' The previous snapshot's date is read from the directory names, so the series
#' records its own state; together with the fixed grid ([nextSnapshotDate()])
#' that is all the schedule needs.
#'
#' @param dir root of the snapshot series (default `"data-raw/snapshots"`)
#' @returns a `Date`, or `NA` when the series is empty or absent
#' @seealso snapshotDue, readDownloadSnapshots
#' @export
latestSnapshotDate <- function(dir = "data-raw/snapshots") {
  if (!dir.exists(dir)) {
    return(as.Date(NA))
  }
  names <- list.dirs(dir, recursive = FALSE, full.names = FALSE)
  dates <- as.Date(names, format = "%Y-%m-%d")
  dates <- dates[!is.na(dates)]
  if (!length(dates)) as.Date(NA) else max(dates)
}

#' Is a download snapshot due?
#'
#' Answers the question a schedule would otherwise answer, so that cron or
#' launchd only has to run the job often, not on a computed date. It is `TRUE`
#' when the series is empty or `today` has reached the next grid date after the
#' latest snapshot ([nextSnapshotDate()]), and it stays `TRUE` until a snapshot
#' is written -- a machine that was asleep on the due date catches up on its
#' next run instead of skipping a month.
#'
#' A catch-up snapshot is off the grid, and the one after it is due on the
#' next grid date, not 30 days after the late one, so a single missed day
#' cannot shift the rest of the series.
#'
#' Callers should ask this before downloading the 121 MB RDF payload.
#'
#' @param dir root of the snapshot series (default `"data-raw/snapshots"`)
#' @param today the date to test, `Date` or `"YYYY-MM-DD"` (default: the system
#'   date)
#' @param anchor the grid's origin (default `SNAPSHOT_ANCHOR`, 2026-11-07)
#' @returns `TRUE` or `FALSE`
#' @seealso nextSnapshotDate, latestSnapshotDate, writeDownloadSnapshot
#' @export
snapshotDue <- function(dir = "data-raw/snapshots", today = Sys.Date(),
                        anchor = SNAPSHOT_ANCHOR) {
  today <- as.Date(today)
  stopifnot("today must be a single date" = length(today) == 1 && !is.na(today))

  latest <- latestSnapshotDate(dir)
  is.na(latest) || snapshotAllowed(latest, today, anchor)
}

#' Write one download snapshot
#'
#' Snapshots are append-only. A date that already has a snapshot is an error,
#' never an overwrite, so a re-run cannot quietly replace an observation. The
#' directory holds what one retrieval produced: the counter
#' (`downloads.csv.gz`) and the catalogue's release dates
#' (`date_added.csv.gz`).
#'
#' The interval to the previous snapshot is checked against the grid
#' ([nextSnapshotDate()]):
#'
#' * before the next grid date, or less than 30 days later and off the grid:
#'   refused, because the overlapping days would be counted twice;
#' * exactly 30 days: written silently;
#' * more than 30 days (a late, catch-up snapshot): written with a warning,
#'   because the missing days are a gap;
#' * less than 30 days but on the grid (the snapshot after a late one): written
#'   with a warning, because returning to the grid counts the late snapshot's
#'   delay twice. This keeps the pre-registered dates instead of shifting
#'   every later one.
#'
#' @param books a tibble as returned by [readGutenbergRdf()]
#' @param snapshot_date the observation date, `Date` or `"YYYY-MM-DD"`
#' @param source short label for the payload, recorded in every row
#' @param dir root of the snapshot series (default `"data-raw/snapshots"`)
#' @param anchor the grid's origin (default `SNAPSHOT_ANCHOR`, 2026-11-07)
#' @returns the path to `downloads.csv.gz`, invisibly
#' @seealso readGutenbergRdf, readDownloadSnapshots, nextSnapshotDate
#' @export
writeDownloadSnapshot <- function(books, snapshot_date, source,
                                  dir = "data-raw/snapshots",
                                  anchor = SNAPSHOT_ANCHOR) {
  stopifnot(
    "books must be a data frame with gutenberg_id, downloads and date_added" =
      is.data.frame(books) &&
        all(c("gutenberg_id", "downloads", "date_added") %in% names(books)),
    "snapshot_date must be a single date" =
      length(snapshot_date) == 1 && !is.na(as.Date(snapshot_date)),
    "source must be a single non-empty string" =
      is.character(source) && length(source) == 1 && nzchar(source)
  )

  snapshot_date <- as.Date(snapshot_date)
  if (anyNA(books$downloads) || any(books$downloads < 0)) {
    stop("downloads must be non-missing and non-negative.", call. = FALSE)
  }

  out_dir <- file.path(dir, format(snapshot_date))
  downloads_path <- file.path(out_dir, "downloads.csv.gz")
  if (file.exists(downloads_path)) {
    stop("A snapshot for ", format(snapshot_date), " already exists: ",
         downloads_path, ". Snapshots are append-only; use a new date.",
         call. = FALSE)
  }

  previous <- latestSnapshotDate(dir)
  if (!is.na(previous)) {
    interval <- as.numeric(snapshot_date - previous)
    if (!snapshotAllowed(previous, snapshot_date, anchor)) {
      stop("The latest snapshot is ", format(previous), " and ",
           format(snapshot_date), " is only ", interval, " days later. The ",
           "counter reports a trailing ", SNAPSHOT_INTERVAL_DAYS, "-day window, ",
           "so snapshots closer together count the overlapping days twice; ",
           "a shorter interval is accepted only when it returns the series ",
           "to the grid. Wait until ",
           format(nextAllowedSnapshot(previous, snapshot_date, anchor)),
           " or later.",
           call. = FALSE)
    }
    if (interval < SNAPSHOT_INTERVAL_DAYS) {
      warning("Snapshot ", format(snapshot_date), " returns the series to the ",
              "grid ", interval, " days after the off-grid snapshot ",
              format(previous), ": ", SNAPSHOT_INTERVAL_DAYS - interval,
              " day(s) of counter coverage are counted twice. Recorded, ",
              "never adjusted.", call. = FALSE)
    }
    if (interval > SNAPSHOT_INTERVAL_DAYS) {
      warning("Snapshot ", format(snapshot_date), " is ", interval, " days ",
              "after ", format(previous), " rather than ",
              SNAPSHOT_INTERVAL_DAYS, ": ", interval - SNAPSHOT_INTERVAL_DAYS,
              " day(s) of counter coverage are missing. Recorded as a gap, ",
              "never interpolated.", call. = FALSE)
    }
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  counter <- tibble::tibble(
    gutenberg_id = as.integer(books$gutenberg_id),
    snapshot_date = snapshot_date,
    source = source,
    downloads = as.integer(books$downloads)
  )
  readr::write_csv(counter, downloads_path)

  dates <- tibble::tibble(
    gutenberg_id = as.integer(books$gutenberg_id),
    date_added = as.Date(books$date_added)
  )
  readr::write_csv(dates, file.path(out_dir, "date_added.csv.gz"))

  invisible(downloads_path)
}

#' Read a download snapshot series
#'
#' Binds every `downloads.csv.gz` under a snapshot root. The result is long:
#' one row per book per snapshot date.
#'
#' @param dir root of the snapshot series (default `"data-raw/snapshots"`)
#' @returns a tibble with `gutenberg_id`, `snapshot_date`, `source`,
#'   `downloads`; empty when no snapshot exists yet
#' @seealso readGutenbergRdf, writeDownloadSnapshot
#' @export
readDownloadSnapshots <- function(dir = "data-raw/snapshots") {
  paths <- list.files(
    dir, pattern = "^downloads[.]csv[.]gz$",
    recursive = TRUE, full.names = TRUE
  )
  if (length(paths) == 0) {
    return(tibble::tibble(
      gutenberg_id = integer(0),
      snapshot_date = as.Date(character(0)),
      source = character(0),
      downloads = integer(0)
    ))
  }

  snapshots <- lapply(paths, function(path) {
    readr::read_csv(
      path,
      col_types = readr::cols(
        gutenberg_id = readr::col_integer(),
        snapshot_date = readr::col_date(),
        source = readr::col_character(),
        downloads = readr::col_integer()
      )
    )
  })
  series <- dplyr::bind_rows(snapshots)

  duplicated_rows <- duplicated(series[c("gutenberg_id", "snapshot_date")])
  if (any(duplicated_rows)) {
    stop("Duplicate (gutenberg_id, snapshot_date) rows in the snapshot series: ",
         "two files claim the same observation.", call. = FALSE)
  }
  series
}
