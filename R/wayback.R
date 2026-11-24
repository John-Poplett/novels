# Reading the 2013 corpus's download counts back from the Wayback Machine's
# captures of each book's Project Gutenberg page (TODO.md, "Wayback reader").
# Network-free helpers; the fetching itself is data-raw/wayback-2012.R.

#' The capture-index query for a book's Gutenberg page
#'
#' The Wayback Machine's CDX API lists the archived copies of one URL. The
#' plain-text output asked for here has one capture per line, as
#' `timestamp digest`, only for answers the archive got with status 200.
#'
#' @param id a single positive whole-number Gutenberg id
#' @param from,to the first and last year (or `YYYYMM`) to list
#' @returns the query URL
#' @noRd
waybackIndexUrl <- function(id, from = "2011", to = "2013") {
  mirrorDir(id)  # validates the id
  paste0("https://web.archive.org/cdx/search/cdx?url=gutenberg.org/ebooks/",
         format(id, scientific = FALSE, trim = TRUE),
         "&from=", from, "&to=", to,
         "&fl=timestamp,digest&filter=statuscode:200")
}

#' The captures a capture-index query returned
#'
#' @param lines the query's answer, one capture per line
#' @returns a data frame of `timestamp` (the 14-digit string the archive
#'   uses), `time` (POSIXct, UTC) and `digest`, in the archive's order; an
#'   error if a line is not a capture
#' @noRd
waybackCaptures <- function(lines) {
  lines <- trimws(lines[nzchar(trimws(lines))])
  ok <- grepl("^[0-9]{14} [A-Z0-9]+$", lines)
  if (!all(ok)) {
    stop("Not a capture-index line: '", lines[!ok][1], "'. The query must ask ",
         "for fl=timestamp,digest in the plain-text output.", call. = FALSE)
  }
  parts <- strsplit(lines, " ", fixed = TRUE)
  timestamp <- vapply(parts, `[`, "", 1)
  data.frame(timestamp = timestamp,
             time = as.POSIXct(timestamp, format = "%Y%m%d%H%M%S", tz = "UTC"),
             digest = vapply(parts, `[`, "", 2))
}

#' The captures nearest a date, one on each side
#'
#' A trailing-window count moves from month to month, so a reading is
#' only comparable with the corpus's when it is near the scrape. The
#' nearest capture at or before `target` and the nearest after it bracket
#' the date; either is `NA` when the archive has none on that side.
#'
#' @param captures from [waybackCaptures()]
#' @param target the date to bracket
#' @returns a data frame of up to two rows from `captures`, with a `side`
#'   column (`"before"`, `"after"`) and `days` from `target` (negative
#'   before)
#' @noRd
waybackBracket <- function(captures, target) {
  target <- as.POSIXct(as.Date(target), tz = "UTC")
  stopifnot("target must be one date" = length(target) == 1 && !is.na(target))
  days <- as.numeric(difftime(captures$time, target, units = "days"))
  pick <- function(keep, best) {
    i <- which(keep)
    if (!length(i)) return(NULL)
    j <- i[best(days[i])]
    cbind(captures[j, , drop = FALSE], days = days[j])
  }
  out <- rbind(
    if (!is.null(b <- pick(days <= 0, which.max))) cbind(b, side = "before"),
    if (!is.null(a <- pick(days > 0, which.min))) cbind(a, side = "after"))
  if (is.null(out)) {
    out <- cbind(captures[0, , drop = FALSE], days = numeric(), side = character())
  }
  rownames(out) <- NULL
  out
}

#' The archived page itself, as it was stored
#'
#' The `id_` flag asks for the original bytes, without the archive's
#' toolbar or rewritten links.
#' @noRd
waybackPageUrl <- function(id, timestamp) {
  mirrorDir(id)
  stopifnot("timestamp must be 14 digits" = grepl("^[0-9]{14}$", timestamp))
  paste0("https://web.archive.org/web/", timestamp, "id_/http://www.gutenberg.org/ebooks/",
         format(id, scientific = FALSE, trim = TRUE))
}

#' The download count on an archived Gutenberg book page
#'
#' The page's bibliographic table has a `Downloads` row. Three forms are
#' known: a bare number (2011-2012), the same with
#' `itemprop="interactionCount"` (from 2013-01), and "N downloads in the
#' last 30 days" (from 2013-04). Only the last states its window; the
#' earlier pages give none (PREREGISTRATION.md, amendment of 2026-11-23,
#' point 1(a)).
#'
#' @param html the page, as a character vector of lines
#' @returns a one-row data frame: `downloads` (integer) and `window`
#'   (`"30 days"` or `"unstated"`); `NA` for both when the page has no
#'   Downloads row (a capture of an error or a moved page)
#' @noRd
waybackDownloads <- function(html) {
  page <- paste(html, collapse = " ")
  m <- regmatches(page, regexec(
    "<th>\\s*Downloads\\s*</th>\\s*<td[^>]*>\\s*([0-9][0-9,]*)([^<]*)", page))[[1]]
  if (!length(m)) {
    return(data.frame(downloads = NA_integer_, window = NA_character_))
  }
  data.frame(downloads = as.integer(gsub(",", "", m[2])),
             window = if (grepl("last 30 days", m[3], fixed = TRUE)) "30 days" else "unstated")
}

#' Call `f()` again after a failure, waiting longer each time
#'
#' The archive refuses connections in bursts when it is asked too often, so
#' a failed request is usually worth repeating after a pause. `f` is retried
#' after each wait in `waits`; the last failure is re-raised.
#'
#' @param f a function of no arguments
#' @param waits seconds to wait before each retry
#' @param sleep the waiting function (a test passes a recorder)
#' @returns `f()`'s value, from the first call that succeeds
#' @noRd
withRetry <- function(f, waits = c(10, 30, 90), sleep = Sys.sleep) {
  stopifnot("waits must be non-negative seconds" = is.numeric(waits) && all(waits >= 0))
  for (w in waits) {
    result <- tryCatch(f(), error = function(e) e)
    if (!inherits(result, "error")) return(result)
    sleep(w)
  }
  f()
}
