series <- function(dates) {
  dir <- tempfile("snapshots-")
  for (d in dates) dir.create(file.path(dir, d), recursive = TRUE)
  dir
}
complete <- function(date) paste0(date, "/", SNAPSHOT_FILES)
log_at <- function(...) {
  # each argument: c(time, message)
  vapply(list(...), function(x) paste0(x[1], " collect-snapshot: ", x[2]), "")
}
now <- as.POSIXct("2026-11-21 08:00:00", tz = "UTC")
healthy_log <- log_at(c("2026-11-21T07:00:00Z", "start on appliance"),
                      c("2026-11-21T07:00:07Z", "done, status 0"))

test_that("a healthy series reports nothing", {
  dir <- series("2026-11-07")
  expect_identical(
    snapshotProblems(dir, "2026-11-21", healthy_log, complete("2026-11-07"), now),
    character())
})

test_that("a snapshot still due after the collectors ran is overdue", {
  dir <- series("2026-11-07")
  p <- snapshotProblems(dir, "2026-12-08", healthy_log, complete("2026-11-07"),
                        now = as.POSIXct("2026-12-08 08:00:00", tz = "UTC"),
                        max_age_hours = 1e6)
  expect_length(p, 1)
  expect_match(p, "^Overdue: no snapshot since 2026-11-07, and one has been due since 2026-12-07")
})

test_that("the grid date itself is overdue once both collectors have missed it", {
  dir <- series("2026-11-07")
  p <- snapshotProblems(dir, "2026-12-07", healthy_log, complete("2026-11-07"),
                        now, max_age_hours = 1e6)
  expect_match(p, "^Overdue", all = FALSE)
  expect_false(any(grepl("^Overdue", snapshotProblems(
    dir, "2026-12-06", healthy_log, complete("2026-11-07"), now, max_age_hours = 1e6))))
})

test_that("a late snapshot's follower is due on the next grid date", {
  dir <- series(c("2026-11-07", "2026-12-09"))
  p <- snapshotProblems(dir, "2027-01-07", healthy_log, complete("2026-12-09"),
                        now, max_age_hours = 1e6)
  expect_match(p, "due since 2027-01-06", all = FALSE)
})

test_that("an incomplete off-site copy names the missing files", {
  dir <- series("2026-11-07")
  p <- snapshotProblems(dir, "2026-11-21", healthy_log,
                        complete("2026-11-07")[-2], now)
  expect_length(p, 1)
  expect_match(p, "^Off-site: .*missing 2026-11-07/downloads.csv.gz\\.")
})

test_that("an older snapshot off-site does not stand in for the latest", {
  dir <- series(c("2026-11-07", "2026-12-07"))
  p <- snapshotProblems(dir, "2026-12-08", healthy_log, complete("2026-11-07"), now)
  expect_match(p, "missing 2026-12-07/date_added.csv.gz, 2026-12-07/downloads", all = FALSE)
})

test_that("a failed listing is a problem, and NA skips the check", {
  dir <- series("2026-11-07")
  expect_match(snapshotProblems(dir, "2026-11-21", healthy_log, NULL, now),
               "^Off-site: the off-site listing failed")
  expect_identical(snapshotProblems(dir, "2026-11-21", healthy_log, NA, now),
                   character())
})

test_that("an empty series has nothing to check off-site", {
  dir <- series(character())
  p <- snapshotProblems(dir, "2026-11-21", healthy_log, NULL, now)
  expect_false(any(grepl("^Off-site", p)))
  expect_match(p, "^Overdue: no snapshot since the start", all = FALSE)
})

test_that("recent FAILED lines are reported, old ones are not", {
  dir <- series("2026-11-07")
  log <- c(log_at(c("2026-11-19T07:00:01Z", "FAILED: upload to b2snap:x/")),
           healthy_log[1],
           "R output without a stamp, FAILED inside",
           log_at(c("2026-11-21T07:00:03Z", "FAILED: pull from ts-appliance:x/"),
                  c("2026-11-21T07:00:04Z", "FAILED: upload to b2snap:x/")),
           healthy_log[2])
  p <- snapshotProblems(dir, "2026-11-21", log, complete("2026-11-07"), now)
  expect_length(p, 1)
  expect_match(p, "^Failures: the collector logged these failures in the last 26 hours")
  expect_match(p, "\n    collect-snapshot: FAILED: pull", fixed = TRUE)
  expect_no_match(p, "2026-11-21T", fixed = TRUE)
  expect_no_match(p, "2026-11-19", fixed = TRUE)
  expect_no_match(p, "without a stamp", fixed = TRUE)
})

test_that("expected failures are ignored", {
  dir <- series("2026-11-07")
  log <- c(healthy_log,
           log_at(c("2026-11-21T07:00:05Z", "FAILED: copy to gpu-box:")))
  expect_identical(
    snapshotProblems(dir, "2026-11-21", log, complete("2026-11-07"), now,
                     ignore = "copy to gpu-box:"),
    character())
  expect_match(
    snapshotProblems(dir, "2026-11-21", log, complete("2026-11-07"), now),
    "this failure in")
})

test_that("a failure repeating on consecutive nights is one episode", {
  # Regression: the stamped lines and their count made each night's text
  # differ, so alertAction() commented (and emailed) every night of an outage.
  dir <- series("2026-11-07")
  night <- function(day, extra = character()) c(
    log_at(c(paste0(day, "T07:00:01Z"), "start on appliance"),
           c(paste0(day, "T07:00:09Z"), "FAILED: data-raw/snapshot-downloads.R exited 1")),
    extra,
    log_at(c(paste0(day, "T07:00:10Z"), "done, status 1")))
  # the 26-hour window still holds the previous night's line at the next check
  log1 <- night("2026-11-21")
  log2 <- c(log1, night("2026-11-22"))
  at <- function(day) as.POSIXct(paste(day, "08:00:00"), tz = "UTC")
  p1 <- snapshotProblems(dir, "2026-11-21", log1, complete("2026-11-07"), at("2026-11-21"))
  p2 <- snapshotProblems(dir, "2026-11-22", log2, complete("2026-11-07"), at("2026-11-22"))
  expect_identical(p2, p1)
  expect_identical(alertAction(p2, p1, 12L), "none")
  expect_identical(lengths(regmatches(p2, gregexpr("FAILED", p2))), 1L)
  # a new kind of failure is a change, and gets its comment
  log3 <- c(log2, night("2026-11-23",
                        log_at(c("2026-11-23T07:00:09Z", "FAILED: upload to b2snap:x/"))))
  p3 <- snapshotProblems(dir, "2026-11-23", log3, complete("2026-11-07"), at("2026-11-23"))
  expect_identical(alertAction(p3, p2, 12L), "comment")
})

test_that("a silent, empty or unreadable log means the collector is not running", {
  dir <- series("2026-11-07")
  stale <- log_at(c("2026-11-20T05:00:00Z", "done, status 0"))
  expect_match(snapshotProblems(dir, "2026-11-21", stale, complete("2026-11-07"), now),
               "^Silence: .*newest entry is 2026-11-20T05:00:00Z, more than 26 hours ago")
  expect_match(snapshotProblems(dir, "2026-11-21", "no stamps", complete("2026-11-07"), now),
               "^Silence: .*newest entry is missing")
  expect_match(snapshotProblems(dir, "2026-11-21", NULL, complete("2026-11-07"), now),
               "^Silence: the collector log could not be read")
})

test_that("unfinishedRun finds a run with no done line after its start", {
  expect_true(is.na(unfinishedRun(healthy_log)))
  expect_true(is.na(unfinishedRun(character())))
  expect_true(is.na(unfinishedRun("no stamps, start on x")))
  going <- c(healthy_log, log_at(c("2026-11-22T07:00:00Z", "start on appliance"),
                                 c("2026-11-22T07:00:05Z", "FAILED: pull from x")))
  expect_identical(unfinishedRun(going),
                   as.POSIXct("2026-11-22 07:00:00", tz = "UTC"))
})

test_that("a run still going is not overdue or incomplete off-site yet", {
  # Regression: on a fallback night the Pi's payload download (up to an hour)
  # outlasts the hour before the check, and a snapshot about to be written
  # was reported overdue.
  dir <- series("2026-11-07")
  at <- as.POSIXct("2026-12-07 08:00:00", tz = "UTC")
  going <- c(healthy_log, log_at(c("2026-12-07T07:00:00Z", "start on appliance")))
  expect_identical(
    snapshotProblems(dir, "2026-12-07", going, character(), now = at,
                     max_age_hours = 1e6),
    character())
  # once it finishes without writing one, it is overdue
  done <- c(going, log_at(c("2026-12-07T07:50:00Z", "done, status 0")))
  p <- snapshotProblems(dir, "2026-12-07", done, character(), now = at,
                        max_age_hours = 1e6)
  expect_match(p, "^Overdue:", all = FALSE)
  expect_match(p, "^Off-site:", all = FALSE)
})

test_that("a run unfinished for hours is stalled, and the rest is judged", {
  dir <- series("2026-11-07")
  at <- as.POSIXct("2026-12-07 11:00:00", tz = "UTC")
  going <- c(healthy_log, log_at(c("2026-12-07T07:00:00Z", "start on appliance")))
  p <- snapshotProblems(dir, "2026-12-07", going, complete("2026-11-07"), now = at,
                        max_age_hours = 1e6)
  expect_match(p, "^Stalled: the collector run that started at 2026-12-07T07:00:00Z",
               all = FALSE)
  expect_match(p, "^Overdue:", all = FALSE)
  # the text names the start, not an age, so a later check reads the same
  expect_identical(
    snapshotProblems(dir, "2026-12-07", going, complete("2026-11-07"),
                     now = at + 3600, max_age_hours = 1e6),
    p)
})

test_that("alertAction opens, comments only on change, and closes", {
  expect_identical(alertAction("a", character(), NA_integer_), "open")
  expect_identical(alertAction("a", "a", 7L), "none")
  expect_identical(alertAction(c("a", "b"), "a", 7L), "comment")
  expect_identical(alertAction(character(), "a", 7L), "close")
  expect_identical(alertAction(character(), character(), NA_integer_), "none")
  # problems whose issue was never recorded (a failed report) open one
  expect_identical(alertAction("a", "a", NA_integer_), "open")
})

test_that("jsonString escapes what JSON requires", {
  expect_identical(jsonString("plain"), "\"plain\"")
  expect_identical(jsonString("a \"q\" \\ b"), "\"a \\\"q\\\" \\\\ b\"")
  expect_identical(jsonString("l1\nl2\tx\r"), "\"l1\\nl2\\tx\\r\"")
  expect_identical(jsonString("bell\a"), "\"bell\\u0007\"")
  expect_identical(jsonString(""), "\"\"")
  expect_identical(jsonString("caf\u00e9"), "\"caf\u00e9\"")
  expect_error(jsonString(NA_character_))
  expect_error(jsonString(c("a", "b")))
})

test_that("jsonString round-trips through a JSON parser", {
  skip_if_not_installed("jsonlite")
  x <- "Failures:\n    2026-11-21T07:00:03Z \"quoted\" \\path\t\001"
  expect_identical(jsonlite::fromJSON(jsonString(x)), x)
})

test_that("alertBody lists problems and says resolved when there are none", {
  t <- as.POSIXct("2026-12-08 08:00:00", tz = "UTC")
  body <- alertBody(c("Overdue: x.", "Failures: two:\n    l1\n    l2"), "appliance", t)
  expect_match(body, "^The snapshot check on appliance at 2026-12-08T08:00:00Z found:\n\n- Overdue: x.\n- Failures")
  expect_match(body, "- Failures: two:\n\n  ```\n  l1\n  l2\n  ```\n\nWhat each", fixed = TRUE)
  expect_identical(alertBody(character(), "appliance", t),
                   "Resolved: every check passed on appliance at 2026-12-08T08:00:00Z.")
})

test_that("tokenProblem warns within 14 days of expiry, by date", {
  h <- function(date) c("HTTP/2 200", "content-type: application/json",
                        paste0("github-authentication-token-expiration: ", date, " UTC"))
  t <- as.POSIXct("2027-02-01 08:00:00", tz = "UTC")
  expect_identical(tokenProblem(h("2027-02-20 00:03:40"), t), character())
  p <- tokenProblem(h("2027-02-14 00:03:40"), t)
  expect_match(p, "^Token: the GitHub token expires on 2027-02-14\\. Regenerate")
  # the same text every day, so one episode, one email
  expect_identical(p, tokenProblem(h("2027-02-14 00:03:40"), t + 86400))
  expect_match(tokenProblem(h("2027-01-30 00:00:00"), t), "expires on 2027-01-30")
})

test_that("tokenProblem reports a refused token, and ignores what it cannot judge", {
  t <- as.POSIXct("2027-02-01 08:00:00", tz = "UTC")
  expect_match(tokenProblem(c("HTTP/2 401", "x: y"), t), "^Token: GitHub refused the token \\(401\\)")
  expect_identical(tokenProblem(NULL, t), character())
  expect_identical(tokenProblem(c("HTTP/2 200", "x: y"), t), character())
  expect_identical(tokenProblem(c("HTTP/2 200",
    "GitHub-Authentication-Token-Expiration: not a date"), t), character())
  expect_match(tokenProblem(c("HTTP/2 200",
    "GitHub-Authentication-Token-Expiration: 2027-02-02 00:00:00 UTC\r"), t), "2027-02-02")
})
