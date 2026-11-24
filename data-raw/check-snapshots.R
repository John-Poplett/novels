# Daily health check for the download-snapshot series, reported as a GitHub
# issue (OPERATIONS.md, "Alert"). Runs on the Pi two hours after its collector:
#
#   Rscript data-raw/check-snapshots.R --log FILE [--offsite REMOTE]
#     [--ignore REGEX] [--state FILE] [--token-file FILE] [--repo OWNER/NAME]
#     [--dry-run]
#
# --log      the collector's log (collect-snapshot.sh's output)
# --offsite  the rclone remote holding the snapshots, listed read-only
#            ($RCLONE names the binary, as for the collector)
# --ignore   FAILED lines matching this are expected (gpu-box switched off)
# --state    what was last reported, and in which issue
#            (default ~/.local/state/novels-alert.rds)
# --token-file  a fine-grained GitHub token, this repository only, Issues
#            read/write (default ~/.config/novels/github-token, mode 600)
# --repo     default John-Poplett/novels
# --dry-run  print the decision and the text; no GitHub call, no state change
#
# Also checked daily: the token itself, 14 days before it expires or once
# GitHub refuses it.
#
# One issue per episode: opened when a problem appears, commented on only
# when the set of problems changes, closed when every check passes. GitHub's
# own notification email is the alert. The token never appears on a command
# line: curl reads it from a mode-600 header file.
#
# Exit status: 0 healthy, 1 problems found (and reported), 2 reporting failed
# (the state is left alone, so the next run tries again).

pkgload::load_all(quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default = NULL) {
  i <- match(name, args)
  if (is.na(i)) return(default)
  if (i == length(args) || startsWith(args[i + 1], "--")) {
    stop(name, " needs a value.", call. = FALSE)
  }
  args[i + 1]
}
known <- c("--log", "--offsite", "--ignore", "--state", "--token-file", "--repo")
flags <- args[startsWith(args, "--")]
unknown <- setdiff(flags, c(known, "--dry-run"))
if (length(unknown)) {
  stop("Unknown option ", unknown[1], ". See the usage at the top of ",
       "data-raw/check-snapshots.R.", call. = FALSE)
}

log_path <- opt("--log")
if (is.null(log_path)) stop("--log FILE is required.", call. = FALSE)
offsite_remote <- opt("--offsite")
ignore <- opt("--ignore")
state_path <- opt("--state", path.expand("~/.local/state/novels-alert.rds"))
token_path <- opt("--token-file", path.expand("~/.config/novels/github-token"))
repo <- opt("--repo", "John-Poplett/novels")
dry_run <- "--dry-run" %in% args
api <- Sys.getenv("NOVELS_GITHUB_API", "https://api.github.com")  # tests only
snapshot_dir <- Sys.getenv("NOVELS_SNAPSHOT_DIR", "data-raw/snapshots")  # tests only

now <- Sys.time()
today <- as.Date(format(now, "%Y-%m-%d", tz = "UTC"))
host <- Sys.info()[["nodename"]]
stamp <- format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

log_lines <- if (file.exists(log_path)) readLines(log_path, warn = FALSE) else NULL

offsite <- if (is.null(offsite_remote)) NA else {  # NA: not checked
  rclone <- Sys.getenv("RCLONE", "rclone")
  listing <- suppressWarnings(system2(
    rclone, c("lsf", "-R", "--files-only", shQuote(offsite_remote)),
    stdout = TRUE, stderr = FALSE))
  if (!is.null(attr(listing, "status"))) NULL else listing
}
problems <- novels:::snapshotProblems(snapshot_dir, today, log_lines,
                                      offsite, now = now, ignore = ignore)

# GitHub, through the curl binary (present on the Pi, the Mac and gpu-box).
# The token goes into a mode-600 header file, never onto a command line.
header_file <- NULL
if (file.exists(token_path)) {
  token <- trimws(readLines(token_path, n = 1, warn = FALSE))
  header_file <- tempfile("gh-headers-")
  file.create(header_file)
  Sys.chmod(header_file, "600")
  writeLines(c(paste("Authorization: Bearer", token),
               "Accept: application/vnd.github+json",
               "X-GitHub-Api-Version: 2022-11-28"), header_file)
  rm(token)
  on.exit(unlink(header_file), add = TRUE)
}

# One call: its status and response headers, or NULL if nothing came back.
curl_call <- function(method, path, json = NULL) {
  body_file <- tempfile("gh-body-")
  response_headers <- tempfile("gh-response-")
  on.exit(unlink(c(body_file, response_headers)))
  data <- if (is.null(json)) character() else {
    writeLines(json, body_file)
    c("--data-binary", paste0("@", body_file))
  }
  code <- suppressWarnings(system2("curl", c(
    "-sS", "--max-time", "60", "-o", "/dev/null", "-D", response_headers,
    "-w", "'%{http_code}'", "-X", method, "-H", paste0("@", header_file),
    data, paste0(api, "/repos/", repo, path)), stdout = TRUE, stderr = FALSE))
  code <- suppressWarnings(as.integer(code))
  if (is.na(code) || code == 0L) return(NULL)  # curl says 000 when unreachable
  list(code = code, headers = readLines(response_headers, warn = FALSE))
}
github <- function(method, path, json) {
  r <- curl_call(method, path, json)
  if (is.null(r) || r$code < 200 || r$code >= 300) {
    cat(stamp, "check-snapshots: FAILED:", method, path, "returned",
        if (is.null(r)) "no response" else r$code, "\n")
    quit(save = "no", status = 2L)
  }
  invisible(r$headers)
}

# The token's own health, every day: expiring soon, or refused.
if (!is.null(header_file) && !dry_run) {
  problems <- c(problems, novels:::tokenProblem(curl_call("GET", "")$headers, now))
}

state <- if (file.exists(state_path)) readRDS(state_path) else
  list(issue = NA_integer_, problems = character())
action <- novels:::alertAction(problems, state$problems, state$issue)
body <- novels:::alertBody(problems, host, now)
cat(stamp, "check-snapshots:", length(problems), "problem(s); action", action,
    if (!is.na(state$issue)) paste0("(issue #", state$issue, ")"), "\n")
if (length(problems)) cat(paste0("  ", problems, "\n"), sep = "")

if (dry_run) {
  cat("--- dry run: would ", action, " ---\n", body, "\n", sep = "")
  quit(save = "no", status = if (length(problems)) 1L else 0L)
}
if (action == "none") quit(save = "no", status = if (length(problems)) 1L else 0L)
if (is.null(header_file)) {
  cat(stamp, " check-snapshots: FAILED: no GitHub token at ", token_path, ", so the ",
      length(problems), " problem(s) above were not reported. Create it as ",
      "OPERATIONS.md, \"Alert\" describes.\n", sep = "")
  quit(save = "no", status = 2L)
}
dir.create(dirname(state_path), showWarnings = FALSE, recursive = TRUE)

comment <- function(issue, text) {
  github("POST", sprintf("/issues/%d/comments", issue),
         sprintf("{\"body\": %s}", novels:::jsonString(text)))
}

open_issue <- function() {
  kinds <- unique(sub(":.*", "", problems))
  title <- sprintf("Snapshot alert (%s): %s", format(today), paste(kinds, collapse = ", "))
  headers <- github("POST", "/issues", sprintf("{\"title\": %s, \"body\": %s}",
    novels:::jsonString(title), novels:::jsonString(body)))
  location <- grep("^location:", headers, ignore.case = TRUE, value = TRUE)
  number <- as.integer(sub(".*/issues/([0-9]+)\\s*$", "\\1", location[1]))
  if (is.na(number)) {
    cat(stamp, "check-snapshots: FAILED: the new issue's number is not in the response\n")
    quit(save = "no", status = 2L)
  }
  number
}

issue <- state$issue
if (action == "comment") {
  # Someone may have closed the issue by hand. The bot opened it, but an
  # issue its owner closed is one the bot may not reopen (it is not a
  # collaborator): then the change starts a new episode instead. Only a
  # lasting refusal means that; a failure that may pass (a 5xx, the rate
  # limit, no answer) is retried tomorrow, or the open issue is orphaned.
  reopened <- curl_call("PATCH", sprintf("/issues/%d", issue), "{\"state\": \"open\"}")
  if (!is.null(reopened) && reopened$code >= 200 && reopened$code < 300) {
    comment(issue, body)
  } else if (novels:::githubRefused(reopened)) {
    cat(stamp, " check-snapshots: issue #", issue, " cannot be reopened (",
        reopened$code, "); opening a new one\n", sep = "")
    action <- "open"
  } else {
    cat(stamp, " check-snapshots: FAILED: reopening issue #", issue, " returned ",
        if (is.null(reopened)) "no response" else reopened$code, "\n", sep = "")
    quit(save = "no", status = 2L)
  }
}
if (action == "open") {
  issue <- open_issue()
} else if (action == "close") {
  # The "Resolved" comment is posted once: if the close then fails, the state
  # records it, and tomorrow's retry only closes.
  if (!identical(state$resolved, issue)) {
    comment(issue, body)
    saveRDS(utils::modifyList(state, list(resolved = issue)), state_path)
  }
  closed <- curl_call("PATCH", sprintf("/issues/%d", issue),
                      "{\"state\": \"closed\", \"state_reason\": \"completed\"}")
  if (is.null(closed) || closed$code < 200 || closed$code >= 300) {
    if (!novels:::githubRefused(closed)) {
      cat(stamp, " check-snapshots: FAILED: closing issue #", issue, " returned ",
          if (is.null(closed)) "no response" else closed$code,
          " (the comment is posted; the next run only closes)\n", sep = "")
      quit(save = "no", status = 2L)
    }
    # Closed or deleted by its owner, so not the bot's to close: done.
    cat(stamp, " check-snapshots: issue #", issue, " cannot be closed (", closed$code,
        "); it was closed or removed by hand, so the episode ends here\n", sep = "")
  }
}
cat(stamp, " check-snapshots: issue #", issue, " ",
    c(open = "opened", comment = "commented on", close = "closed")[[action]],
    "\n", sep = "")

saveRDS(list(issue = if (action == "close") NA_integer_ else issue,
             problems = problems), state_path)
quit(save = "no", status = if (length(problems)) 1L else 0L)
