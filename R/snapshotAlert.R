# Daily health check for the download-snapshot series (OPERATIONS.md,
# "Alert"). data-raw/check-snapshots.R gathers the inputs on the Pi and
# reports through a GitHub issue; the functions here only decide, so they
# can be tested without a network, a log or a clock.

# The collector runs nightly; a log whose newest stamped line is older than
# this means it did not run at all (crontab gone, script deleted, Pi asleep).
COLLECTOR_LOG_MAX_AGE_HOURS <- 26

# The files every snapshot directory holds (writeDownloadSnapshot() and
# snapshot-downloads.R), so an off-site copy missing one is incomplete.
SNAPSHOT_FILES <- c("date_added.csv.gz", "downloads.csv.gz", "provenance.txt")

# collect-snapshot.sh's stamp() at the start of a line.
LOG_STAMP <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z "

#' Timestamps of a collector log's stamped lines
#'
#' collect-snapshot.sh starts each of its own lines with an ISO-8601 UTC
#' stamp; lines R prints in between carry none and are skipped.
#' @returns a POSIXct vector, one per stamped line, in log order
#' @noRd
logStamps <- function(lines) {
  stamped <- grepl(LOG_STAMP, lines)
  as.POSIXct(substr(lines[stamped], 1, 20), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' What is wrong with the snapshot series today?
#'
#' Four questions, each answered from its inputs alone:
#'
#' 1. **Overdue:** has the next grid date after the latest snapshot
#'    ([nextSnapshotDate()]) arrived, after both collectors have run? This
#'    repeats every day until a snapshot is written.
#' 2. **Off-site:** does the off-site listing hold every file of the latest
#'    local snapshot? `offsite` is `NULL` when the listing itself failed.
#' 3. **Failures:** does the log hold `FAILED` lines from the last
#'    `max_age_hours`, other than those matching `ignore` (a peer that is
#'    switched off on purpose)?
#' 4. **Silence:** is the log's newest stamped line older than
#'    `max_age_hours` (or absent), meaning the collector never ran?
#'
#' @param dir the snapshot series
#' @param today the UTC date the collectors used
#' @param log_lines the collector log, or `NULL` if it could not be read
#' @param offsite paths `"<date>/<file>"` listed off-site; `NULL` when the
#'   listing failed, `NA` to skip the check
#' @param now the current time
#' @param ignore regular expression for `FAILED` lines that are expected, or
#'   `NULL`
#' @returns a character vector, one sentence per problem; empty when healthy
#' @noRd
snapshotProblems <- function(dir, today, log_lines, offsite, now = Sys.time(),
                             ignore = NULL,
                             max_age_hours = COLLECTOR_LOG_MAX_AGE_HOURS) {
  today <- as.Date(today)
  stopifnot("today must be a single date" = length(today) == 1 && !is.na(today))
  problems <- character()
  latest <- latestSnapshotDate(dir)

  # Not snapshotDue(): after a late snapshot, the day after a missed grid
  # date is not allowed (it would overlap), yet the grid date was missed.
  due <- if (is.na(latest)) today else nextSnapshotDate(latest)
  if (today >= due) {
    problems <- c(problems, sprintf(
      paste("Overdue: no snapshot since %s, and one has been due since %s.",
            "Both collectors have run without writing it; their logs say why",
            "(a feed outage needs nothing but patience, any other failure",
            "needs a fix before tonight's runs)."),
      if (is.na(latest)) "the start" else format(latest), format(due)))
  }

  if (!is.na(latest) && !identical(offsite, NA)) {
    if (is.null(offsite)) {
      problems <- c(problems, paste(
        "Off-site: the off-site listing failed, so whether the snapshots",
        "are there is unknown. Check the rclone remote and its key."))
    } else {
      wanted <- paste0(format(latest), "/", SNAPSHOT_FILES)
      missing <- setdiff(wanted, offsite)
      if (length(missing)) {
        problems <- c(problems, paste0(
          "Off-site: the latest snapshot is incomplete off-site; missing ",
          paste(missing, collapse = ", "),
          ". The next collector run uploads it; if this repeats, its log says why."))
      }
    }
  }

  if (is.null(log_lines)) {
    problems <- c(problems, paste(
      "Silence: the collector log could not be read, so whether the",
      "collector runs is unknown."))
  } else {
    stamped <- grepl(LOG_STAMP, log_lines)
    stamps <- logStamps(log_lines)
    recent <- log_lines[stamped][stamps >= now - max_age_hours * 3600]
    failed <- grep("FAILED", recent, value = TRUE, fixed = TRUE)
    if (!is.null(ignore)) failed <- failed[!grepl(ignore, failed)]
    if (length(failed)) {
      problems <- c(problems, paste0(
        "Failures: the collector logged ", length(failed), " failure",
        if (length(failed) > 1) "s", " in the last ", max_age_hours,
        " hours:\n", paste0("    ", failed, collapse = "\n")))
    }
    newest <- if (length(stamps)) max(stamps) else NA
    if (is.na(newest) || newest < now - max_age_hours * 3600) {
      problems <- c(problems, sprintf(
        paste("Silence: the collector log's newest entry is %s, more than",
              "%s hours ago. The collector is not running: check the crontab",
              "and that the machine is up."),
        if (is.na(newest)) "missing" else
          format(newest, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        max_age_hours))
    }
  }
  problems
}

#' What to do on GitHub, given today's problems and the last report
#'
#' One issue tracks one episode. It is opened when problems appear, gets a
#' comment only when the problems change (a feed outage does not email you
#' every day), and is closed with a comment when they clear.
#'
#' @param problems today's [snapshotProblems()]
#' @param previous the problems last reported, `character()` if none
#' @param issue the episode's issue number, or `NA`
#' @returns one of `"open"`, `"comment"`, `"close"`, `"none"`
#' @noRd
alertAction <- function(problems, previous, issue) {
  if (!length(problems)) {
    if (length(previous) && !is.na(issue)) "close" else "none"
  } else if (is.na(issue)) {
    "open"
  } else if (!identical(problems, previous)) {
    "comment"
  } else {
    "none"
  }
}

#' A JSON string literal for `x`
#'
#' Enough of RFC 8259 for issue text: quotes, backslashes and control
#' characters escaped, everything else (UTF-8 included) passed through.
#' @noRd
jsonString <- function(x) {
  stopifnot(is.character(x), length(x) == 1, !is.na(x))
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  x <- gsub("\t", "\\t", x, fixed = TRUE)
  chars <- strsplit(x, "")[[1]]
  ctrl <- vapply(chars, function(ch) utf8ToInt(ch) < 32L, logical(1))
  chars[ctrl] <- sprintf("\\u%04x", vapply(chars[ctrl], utf8ToInt, integer(1)))
  paste0("\"", paste(chars, collapse = ""), "\"")
}

#' One problem as a Markdown list item; its indented lines (log excerpts)
#' become a fenced block, which an indented block inside a paragraph is not.
#' @noRd
alertItem <- function(problem) {
  lines <- strsplit(problem, "\n", fixed = TRUE)[[1]]
  item <- paste0("- ", lines[1])
  if (length(lines) > 1) {
    item <- paste0(item, "\n\n  ```\n", paste0("  ", trimws(lines[-1]), collapse = "\n"),
                   "\n  ```")
  }
  item
}

#' The Markdown body that reports `problems`
#' @noRd
alertBody <- function(problems, host, now = Sys.time()) {
  stamp <- format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  if (!length(problems)) {
    return(sprintf("Resolved: every check passed on %s at %s.", host, stamp))
  }
  paste0(
    "The snapshot check on ", host, " at ", stamp, " found:\n\n",
    paste(vapply(problems, alertItem, character(1)), collapse = "\n"),
    "\n\nWhat each check means and what to do: OPERATIONS.md, \"Alert\". ",
    "This issue closes itself when the checks pass again."
  )
}

# Warn this many days before the GitHub token expires: while it still
# works, so the warning itself can be delivered.
TOKEN_WARN_DAYS <- 14

#' Is the GitHub token about to expire?
#'
#' GitHub sends a fine-grained token's expiry with every response
#' (`github-authentication-token-expiration`). An expired token would turn
#' the alert off silently, so the check raises it while the token still
#' works. The text names the date, not a count of days, so it stays the same
#' from one day to the next and the episode gets one email, not one a day.
#'
#' @param headers response headers from any authenticated API call, or
#'   `NULL` when the call failed (not reported here: a network failure is not
#'   a token problem, and the report would fail the same way)
#' @returns a character vector of zero or one problem
#' @noRd
tokenProblem <- function(headers, now = Sys.time(), warn_days = TOKEN_WARN_DAYS) {
  if (is.null(headers)) return(character())
  status <- suppressWarnings(as.integer(
    sub("^HTTP/[0-9.]+ ([0-9]{3}).*", "\\1", grep("^HTTP/", headers, value = TRUE))))
  if (length(status) && status[length(status)] == 401L) {
    return(paste(
      "Token: GitHub refused the token (401): expired or revoked. Generate a",
      "new one and replace ~/.config/novels/github-token on the Pi",
      "(OPERATIONS.md, H, step 1)."))
  }
  line <- grep("^github-authentication-token-expiration:", headers,
               ignore.case = TRUE, value = TRUE)
  if (!length(line)) return(character())  # a token without an expiry
  expires <- as.POSIXct(trimws(sub("^[^:]*:", "", line[1])),
                        format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  if (is.na(expires) || expires - now > as.difftime(warn_days, units = "days")) {
    return(character())
  }
  sprintf(paste(
    "Token: the GitHub token expires on %s. Regenerate it (GitHub, Settings,",
    "Developer settings, Fine-grained tokens) and replace",
    "~/.config/novels/github-token on the Pi (OPERATIONS.md, H, step 1)."),
    format(expires, "%Y-%m-%d", tz = "UTC"))
}
