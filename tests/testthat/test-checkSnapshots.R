context("The snapshot check's GitHub reporting retries what may pass and accepts what will not")

test_that("githubRefused tells a lasting refusal from a failure that may pass", {
  r <- function(code, headers = character()) list(code = code, headers = headers)
  expect_false(githubRefused(NULL))
  expect_false(githubRefused(r(200L)))
  expect_false(githubRefused(r(502L)))
  expect_false(githubRefused(r(429L)))
  for (code in c(403L, 404L, 410L, 422L)) expect_true(githubRefused(r(code)))
  # GitHub's rate limits answer 403 too, and pass
  expect_false(githubRefused(r(403L, c("HTTP/2 403", "Retry-After: 60"))))
  expect_false(githubRefused(r(403L, c("HTTP/2 403", "x-ratelimit-remaining: 0"))))
  expect_true(githubRefused(r(403L, c("HTTP/2 403", "x-ratelimit-remaining: 4999"))))
})

# data-raw/ is in .Rbuildignore, so the script exists only in a checkout:
# these tests run under `make test` and skip under R CMD check.
checkout_root <- function() {
  path <- testthat::test_path("..", "..", "data-raw", "check-snapshots.R")
  if (file.exists(path)) normalizePath(file.path(dirname(path), "..")) else NA_character_
}

# A healthy series and log, a token, and a stand-in for curl that records
# each call and answers from `responses`: lines "METHOD PATH CODE [HEADER]",
# first match wins, anything else 200.
check_sandbox <- function(responses = character()) {
  root <- tempfile("check-")
  dir.create(file.path(root, "bin"), recursive = TRUE)
  now <- Sys.time()
  today <- format(now, "%Y-%m-%d", tz = "UTC")
  dir.create(file.path(root, "snapshots", today), recursive = TRUE)
  writeLines(paste(format(now - 3600, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                   "collect-snapshot: done, status 0"), file.path(root, "collector.log"))
  writeLines("not-a-real-token", file.path(root, "token"))
  writeLines(responses, file.path(root, "responses"))
  writeLines(c(
    "#!/bin/sh",
    "while [ $# -gt 0 ]; do",
    '  case "$1" in',
    '    -D) hdr=$2; shift ;;',
    '    -X) method=$2; shift ;;',
    '    -o|-w|-H|--max-time|--data-binary) shift ;;',
    '    -*) ;;',
    '    *) url=$1 ;;',
    "  esac",
    "  shift",
    "done",
    "path=$(echo \"$url\" | sed 's#.*/repos/[^/]*/[^/]*##')",
    sprintf('echo "$method $path" >> "%s"', file.path(root, "calls.log")),
    sprintf('line=$(awk -v m="$method" -v p="$path" \'$1 == m && $2 == p { print; exit }\' "%s")',
            file.path(root, "responses")),
    'code=$(echo "$line" | awk \'{ print $3 }\'); [ -n "$code" ] || code=200',
    'extra=$(echo "$line" | cut -s -d" " -f4-)',
    'echo "HTTP/2 $code" > "$hdr"',
    '[ -n "$extra" ] && echo "$extra" >> "$hdr"',
    '[ "$method $path" = "POST /issues" ] && echo "location: https://x/repos/o/n/issues/99" >> "$hdr"',
    'printf %s "$code"'
  ), file.path(root, "bin", "curl"))
  Sys.chmod(file.path(root, "bin", "curl"), "755")
  root
}

# Run the check with `state` (or none) as the last report.
run_check <- function(root, state = NULL, token = TRUE) {
  state_path <- file.path(root, "state.rds")
  if (!is.null(state)) saveRDS(state, state_path)
  args <- c(file.path(checkout_root(), "data-raw", "check-snapshots.R"),
            "--log", file.path(root, "collector.log"), "--state", state_path,
            "--token-file", file.path(root, if (token) "token" else "missing"))
  env <- c(PATH = paste(file.path(root, "bin"), Sys.getenv("PATH"), sep = ":"),
           NOVELS_SNAPSHOT_DIR = file.path(root, "snapshots"),
           NOVELS_GITHUB_API = "https://api.invalid")
  old <- setwd(checkout_root())  # the script loads the package from here
  on.exit(setwd(old), add = TRUE)
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), shQuote(args),
    env = paste0(names(env), "=", shQuote(env)), stdout = TRUE, stderr = TRUE))
  calls <- file.path(root, "calls.log")
  list(status = if (is.null(attr(out, "status"))) 0L else attr(out, "status"),
       output = out,
       calls = if (file.exists(calls)) readLines(calls) else character(),
       state = if (file.exists(state_path)) readRDS(state_path) else NULL)
}

# A report about a problem that has since cleared, so today's action is close.
resolving <- list(issue = 12L, problems = "Overdue: x.")

test_that("a missing token is a reporting failure, exit 2, state untouched", {
  skip_if(is.na(checkout_root()), "data-raw/ is not in the built package")
  root <- check_sandbox()
  # problems today (the issue is open, the report clears): something to report
  res <- run_check(root, resolving, token = FALSE)
  expect_identical(res$status, 2L)
  expect_match(res$output, "FAILED: no GitHub token", all = FALSE)
  expect_identical(res$state, resolving)
})

test_that("a failure that may pass, on reopening, is retried, not a new issue", {
  skip_if(is.na(checkout_root()), "data-raw/ is not in the built package")
  # Regression: any non-2xx opened a new issue and orphaned the open one.
  root <- check_sandbox("PATCH /issues/12 502")
  writeLines(paste(format(Sys.time() - 3600, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                   "collect-snapshot: FAILED: upload to b2snap:x/"),
             file.path(root, "collector.log"))
  state <- list(issue = 12L, problems = "Overdue: x.")
  res <- run_check(root, state)
  expect_identical(res$status, 2L)
  expect_false("POST /issues" %in% res$calls)
  expect_identical(res$state, state)
})

test_that("a lasting refusal, on reopening, starts a new issue", {
  skip_if(is.na(checkout_root()), "data-raw/ is not in the built package")
  root <- check_sandbox("PATCH /issues/12 403")
  writeLines(paste(format(Sys.time() - 3600, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                   "collect-snapshot: FAILED: upload to b2snap:x/"),
             file.path(root, "collector.log"))
  res <- run_check(root, list(issue = 12L, problems = "Overdue: x."))
  expect_identical(res$status, 1L)
  expect_true("POST /issues" %in% res$calls)
  expect_identical(res$state$issue, 99L)
})

test_that("a failed close is retried without posting \"Resolved\" again", {
  skip_if(is.na(checkout_root()), "data-raw/ is not in the built package")
  # Regression: the comment went out again on every retry of the close.
  root <- check_sandbox("PATCH /issues/12 503")
  first <- run_check(root, resolving)
  expect_identical(first$status, 2L)
  expect_identical(sum(first$calls == "POST /issues/12/comments"), 1L)
  expect_identical(first$state$issue, 12L)

  writeLines(character(), file.path(root, "responses"))  # GitHub is back
  second <- run_check(root)  # the state the first run left
  expect_identical(second$status, 0L)
  expect_identical(sum(second$calls == "POST /issues/12/comments"), 1L)  # still one
  expect_identical(sum(second$calls == "PATCH /issues/12"), 2L)
  expect_identical(second$state, list(issue = NA_integer_, problems = character()))
})

test_that("an issue closed or removed by hand ends the episode", {
  skip_if(is.na(checkout_root()), "data-raw/ is not in the built package")
  root <- check_sandbox("PATCH /issues/12 404")
  res <- run_check(root, resolving)
  expect_identical(res$status, 0L)
  expect_match(res$output, "cannot be closed (404)", fixed = TRUE, all = FALSE)
  expect_identical(res$state, list(issue = NA_integer_, problems = character()))
})

test_that("a dry run with nothing to post says so, not \"Resolved\"", {
  skip_if(is.na(checkout_root()), "data-raw/ is not in the built package")
  root <- check_sandbox()
  res <- run_check(root, list(issue = NA_integer_, problems = character()))
  dry <- local({
    args <- c(file.path(checkout_root(), "data-raw", "check-snapshots.R"),
              "--log", file.path(root, "collector.log"),
              "--state", file.path(root, "state.rds"), "--dry-run")
    old <- setwd(checkout_root())
    on.exit(setwd(old))
    suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(args),
                             env = paste0("NOVELS_SNAPSHOT_DIR=",
                                          shQuote(file.path(root, "snapshots"))),
                             stdout = TRUE, stderr = TRUE))
  })
  expect_identical(res$status, 0L)
  expect_true("--- dry run: nothing to post ---" %in% dry)
  expect_false(any(grepl("Resolved", dry, fixed = TRUE)))
})
