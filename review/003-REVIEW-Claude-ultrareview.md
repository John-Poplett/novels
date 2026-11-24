# Code review, round 3: ultrareview

2026-11-24. This round is a `/code-review ultra` (cloud, multi-agent) of
`dev` at `8de88d7` against `origin/master` (`15646ba8`): 25 files and 3,651
changed lines. `review/` itself was left out to stay under the tool's
8,000-line limit, on a throwaway branch, since deleted, that only removed
`review/`. The reviewer therefore saw none of rounds 000–002.

The reviewer reported nine findings. Each one was checked again locally
before it was written down here, and the "Verified" line says how. None was
dropped. The order below is by consequence, not the reviewer's own order.

| # | where | what | reviewer's grade | verified |
|---|---|---|---|---|
| 1 | `R/snapshotAlert.R:97-104` | a repeating failure comments every day | normal | reproduced |
| 2 | `R/snapshotAlert.R:62-70` | false Overdue/Off-site while the Pi's fallback run is still going | nit | by reading, timings from OPERATIONS.md |
| 3 | `data-raw/check-snapshots.R:170-178` | any non-2xx on reopen orphans the open issue | nit | by reading |
| 4 | `data-raw/check-snapshots.R:183-184` | a failed close re-posts "Resolved" every day | nit | by reading |
| 5 | `data-raw/check-snapshots.R:133-137` | a missing token exits 1 ("reported"), not 2 | nit | by reading |
| 6 | `data-raw/check-snapshots.R:14`, `R/snapshotAlert.R:195,225` | docs say fine-grained token; OPERATIONS.md H says classic `public_repo` | nit | by reading |
| 7 | `data-raw/check-snapshots.R:88` | top-level `on.exit()` is dead code | nit | reproduced |
| 8 | `data-raw/genre-contamination-pilot.R:283` | a killed `mclapply` worker drops draws silently | nit | reproduced |
| 9 | `data-raw/*.R` | `aucRank()` copied four times, already diverged | nit | by reading |

Findings 1–7 are about the snapshot alert, which is live on the Pi
(02:00 CT). Findings 8 and 9 are about the analysis scripts.

---

## 1. A repeating failure sends an email every day

`snapshotProblems()` builds the Failures text from the raw log lines,
timestamps included, and from their count. On the second night of the same
failure the text differs, `alertAction()` sees
`!identical(problems, previous)`, and the bot comments. That emails every
day, which breaks OPERATIONS.md H's promise that "a long feed outage sends
one email, not one a day", and `alertAction()`'s own docstring.

**Verified:** reproduced. Two synthetic logs had the same
`FAILED: data-raw/snapshot-downloads.R exited 1` line on 2026-12-08 and
2026-12-09, and `alertAction(p2, p1, 12L)` returned `"comment"`.

**Fix:** compare episodes on a stable key. Use the FAILED messages with the
timestamp stripped, de-duplicated and sorted, with no count. Keep the full
stamped lines in the body only. Store the key in the state file next to
`problems`, or compare on a derived key in `alertAction()`. Add a regression
test with the two-night fixture above. The Overdue text is already stable,
because it names dates, not counts.

**Priority: first.** The alert is live, and the first multi-night outage
would teach the reader to ignore it.

> **Status, 2026-11-24: fixed, `04fe11e`.** Each distinct `FAILED` message
> is listed once, sorted, without its stamp or a count, so the same failure
> on consecutive nights reads the same and gets no comment. The 26-hour
> window also keeps the previous night's line, which the review missed.
> Regression test: two and three nights of one failure are one episode, and
> a new kind of failure still comments. The trade-off: the issue body no
> longer shows when each failure happened; the log keeps the times.

## 2. False Overdue (or Off-site) while the Pi's fallback is still running

The check runs at 02:00, an hour after the Pi's 01:00 collector, and assumes
that both collectors have finished. On a night when the Mac's 22:00 run
missed a grid date, the Pi does the payload download. The rehearsal took
29.5 minutes, and the only cap is `downloadWithTimeout`'s 3,600 s, followed
by the copy and the upload. A slow feed carries the run past 02:00. The
check then reports "Overdue ... both collectors have run", or
"Off-site: incomplete" if it lands between the write and the upload. Either
way it is a false alarm, on exactly the nights the fallback is being
exercised.

**Verified:** by reading. The crontab lines are at OPERATIONS.md:205 (Pi,
`0 1`) and :371 (check, `0 2`). The log has `start on` (collect-snapshot.sh:108)
and `done, status` (:174) lines to key on.

**Fix:** before judging Overdue or Off-site, find the newest `start on`
line. If no `done, status` line follows it, report nothing for those two
checks that day, or report a stable "Running: the collector started at …
and has not finished" only once it is older than, say, 3 hours. A simpler
alternative is moving the check to 03:00. That would make the race unlikely
rather than impossible, so the log test is still worth having.

> **Status, 2026-11-24: both landed; the race is made rare, not removed.**
> `de539a5` adds the log test. `unfinishedRun()` finds a newest `start on`
> with no `done` after it. A run younger than 3 hours is waited for (Overdue
> and Off-site are not judged that day), and an older one is reported as
> Stalled. `1642bd3` moves the check to 03:00 (the Pi's crontab was changed
> the same day).
>
> What this solves: **no false alarm.** The check reads the log before it
> lists off-site or reads the series (`check-snapshots.R:65`), and the
> collector logs `done` last. So a run the log shows as finished has
> already written and uploaded, and a run it shows as going is not judged.
>
> What it does not solve: **a true alarm can come a day late.** The
> collector and the check are two cron entries, timed apart and not
> sequenced, and only the download is capped (one hour). The pulls, copies
> and uploads are not capped. A night whose run outlasts the two hours
> before the check is judged the next day. If every night's run did, for
> example a feed that times out at the cap, Overdue would never be raised
> and Failures would carry the alert alone. The stakes are low: the alert
> is delayed, never lost, and the series is unaffected. It is still a race,
> so it is logged as a concern, not closed. See TODO.md, "Alert: sequence
> the check after the collector", and OPERATIONS.md H, "Known gap". The
> fixes that remove it are sequencing, not timing: the collector runs the
> check as its last step (cron kept as a backstop), or the two share a
> `flock` that the check waits on. Either needs a test of the interleaving
> itself.
>
> Two smaller interleavings were found and left. First, `collect-snapshot.sh`
> has no lock, so two collector runs could overlap; that needs a run longer
> than 24 hours, which the previous day's check would already have reported
> as Stalled. Second, a Pi that misses its 01:00 run is judged on the
> previous night's state, and the Overdue it raises then is true.

## 3. A transient error on reopen orphans the open issue

On a `"comment"` run, any non-2xx answer to the reopen PATCH is read as
"closed by its owner, cannot be reopened", and a new issue is opened. A 502,
503 or 429 therefore leaves the old issue open for good, because the state
now points at the new one and nothing ever closes the old one.

**Verified:** by reading lines 170–178. The status code is never inspected.

**Fix:** fall back to a new issue only on 403, 404, 410 or 422. Treat every
other answer, `NULL` included, as a reporting failure: exit 2 and leave the
state alone, as `github()` does. To test it, use the existing
`NOVELS_GITHUB_API` hook with a stub server, or factor the status decision
into a small pure function and test that.

> **Status, 2026-11-24: fixed, `70d2814`.** `githubRefused()` decides, and
> leaves a 403 that carries `retry-after` or `x-ratelimit-remaining: 0` out,
> since that is the rate limit. Anything else exits 2 with the state
> untouched. Tested end to end by the new `test-checkSnapshots.R`, which
> runs the script against a stand-in `curl`.

## 4. A failed close re-posts "Resolved" every day

The close path posts the "Resolved" comment and then the PATCH. If the PATCH
fails, `github()` exits 2 without saving the state, and the next run posts
the comment again. A transient timeout gives one duplicate. A persistent
refusal gives one email a day. The reviewer suspects, but could not confirm,
that the case OPERATIONS.md H calls "fine" is a persistent refusal: the owner
closes the issue by hand, and GitHub refuses the non-collaborator bot's
PATCH of an issue someone else closed.

**Verified:** by reading lines 182–184 and 108–114. The persistent-refusal
case was not tested against GitHub.

**Fix:** make the close idempotent. `GET /issues/N` first, and if the issue
is already closed, skip the PATCH. Alternatively, save the state right after
the comment, with a `closing` flag, so a retry only retries the PATCH.

> **Status, 2026-11-24: fixed, `70d2814`, by the second option.** Once the
> "Resolved" comment is posted, the state records it, so a retry only
> closes. A lasting refusal of the close (403, 404, 410, 422) ends the
> episode. Both cases are tested with the stand-in `curl`. Whether GitHub
> does refuse the bot's close of an issue its owner closed is still not
> tested against GitHub itself; either way the episode now ends once.

## 5. A missing token exits 1, which the contract reads as "reported"

When the token file is missing and there is something to report, the script
calls `stop()`, and Rscript exits 1. The header (line 27) and OPERATIONS.md
define 1 as "problems found (and reported)" and 2 as "reporting failed".

**Verified:** by reading lines 136–140. An uncaught error under Rscript
exits with status 1.

**Fix:** print the message and `quit(status = 2L)`.

> **Status, 2026-11-24: fixed, `70d2814`.** It prints the message and
> exits 2. Tested with the stand-in `curl`.

## 6. The token docs contradict OPERATIONS.md

The usage header (`data-raw/check-snapshots.R:14-15`) asks for "a
fine-grained GitHub token, this repository only, Issues read/write".
`tokenProblem()`'s roxygen (`R/snapshotAlert.R:195`) and its rotation text
(:225-226, "Developer settings, Fine-grained tokens") say the same.
OPERATIONS.md H step 1 (:351-353) and the H2 row (:58) say a fine-grained
token cannot work for the bot, which does not own the repository, and that
the bot uses a classic `public_repo` token with no expiry.

The reviewer adds a failure mode. A fine-grained token gets 403 or 404, not
401, so `tokenProblem()` raises nothing, every run exits 2, and no issue is
ever opened. The alert would be silently off. That only happens if someone
rotates the token from the script's comment instead of OPERATIONS.md.

**Verified:** by reading the three places cited.

**Fix:** reword the header and both `tokenProblem()` texts for the classic
token. While there, consider also treating 403 and 404 from the token probe
(`GET /repos/…`) as a Token problem. The report would then fail the same
way, but `novels-alert.log` would say why.

> **Status, 2026-11-24: fixed, `94e1e2d`.** The header, `tokenProblem()`'s
> doc and its expiry message name the bot's classic `public_repo` token and
> its menu path. Not done: the suggestion to report a 403 or 404 from the
> token probe as a Token problem.

## 7. The top-level `on.exit()` does nothing

`on.exit(unlink(header_file), add = TRUE)` sits in a top-level `if` block of
an Rscript, where `on.exit` registers nothing. Every exit goes through
`quit()` in any case. The header file holding the bearer token is removed
only because R deletes its session temporary directory on a normal exit. A
crash or a kill would leave it in `TMPDIR`, and the Pi's crontab sets the
collector's `TMPDIR` to the persistent `~/.cache/novels-tmp`. The check's
own crontab line (:371) does not set it, so for the check this is `/tmp`
today.

**Verified:** reproduced. A script with a top-level
`if (TRUE) on.exit(cat("ONEXIT RAN\n"))` followed by `quit()` printed
nothing, and its session temporary directory was gone afterwards.

**Fix:** remove the line and say in a comment that the tempdir cleanup is
what removes the file. Or wrap the reporting in a function so that
`on.exit()` is real; `quit()` still skips it, so the function would have to
return a status for a single `quit()` at the end.

> **Status, 2026-11-24: fixed, `94e1e2d`, by the first option.** The line
> is gone, and the comment says what removes the file and what a killed
> run leaves in `TMPDIR`. The same commit makes a dry run whose action is
> none print "nothing to post" instead of the "Resolved" body, an oddity
> seen on the Pi.

## 8. A killed worker silently drops draws in the pilot

`parallel::mclapply` puts `NULL`, not a `try-error`, in the slot of a forked
child that died (an OOM kill, for example), and only warns. The guard at
line 283 checks for `try-error` only, and `dplyr::bind_rows()` drops the
`NULL`s. The summary and the `--out` CSV are then built from fewer draws,
and which draws are missing depends on which seeds died, not on chance.
`PREREGISTRATION.md`'s draft qualifier amendment will cite these pilots.

**Verified:** reproduced. With four jobs, one of which SIGKILLs itself,
`mclapply` returned `NULL` in that slot, the `try-error` test gave all
`FALSE`, and `bind_rows` gave 3 rows.

**Fix:** count `NULL` as a failure, and also check
`nrow(res) == 2 * nrow(jobs)`. The committed pilots are not affected. I
checked on 2026-11-24 that `contamination-pilot-v2.csv` and
`contamination-pilot-v2-matched.csv` each hold all 40 rows (2 rates ×
10 draws × 2 label schemes), with every (rate, draw, labels) triple present
once. Fix this before the full run, which will be longer and use more
memory.

> **Status, 2026-11-24: fixed, `5abe321`.** `parallelResults()` (`R/`)
> stops on any `NULL` or `try-error` and names the jobs, and the pilot
> also checks it has 2 rows per job. One test kills a real forked worker
> with SIGKILL.

## 9. `aucRank()` exists four times, and the copies differ

The copies are in `label-audit.R:40`, `genre-contamination-pilot.R:91`,
`continuous-response.R:66` and `nested-comparison.R:96`. Only `label-audit.R`
has the `stopifnot`. The first two return `NA` when a class is empty; the
last two divide by `n1 * n0` and give `NaN` or `Inf`. The alpha grid and
the baseline-covariate builder are copied in the same way.

**Verified:** by reading (`grep -A6 '^aucRank <- ' data-raw/*.R`).

**Fix:** one internal `aucRank()` in `R/` (`@noRd`), with tests for ties,
an empty class and agreement with `pROC`, if it is already a Suggests.
Scripts call `novels:::aucRank()`, as `check-snapshots.R` already does for
its helpers. Do the baseline builder in a second step. This finding is a
refactor across four scripts, so it should come after the alert fixes and be
kept out of any results-producing run.

> **Status, 2026-11-24: fixed, `cb9afd0`.** One internal `aucRank()` in `R/aucRank.R` serves
> all four scripts and checks its inputs. Tests cover agreement with
> `pROC` on tied scores, the edges, and `NA` for one class. No committed
> result changes: with both classes present the arithmetic is the same.
> Left for a separate change: the alpha grid and the baseline-covariate
> builder.

---

## Suggested order

> **Status, 2026-11-24: all six are on `dev`,** each after its own
> level-2 check came back 0/0/0. The Pi has pulled the alert fixes
> (items 1–4), and its dry run says "nothing to post". A seventh,
> `chore/rbuildignore-git` (`0b25532`), came up along the way: a check run
> in a worktree packaged the worktree's `.git` file.

1. `fix/alert-episode-key`: finding 1, with its regression test.
2. `fix/alert-github-paths`: findings 3, 4 and 5, all in the reporting
   branch of `check-snapshots.R`.
3. `fix/alert-running-collector`: finding 2.
4. `docs/alert-token-text`: findings 6 and 7, both comments and messages.
5. `fix/pilot-null-draws`: finding 8, before the full contamination run.
6. `feat/shared-auc`: finding 9.

Items 1–4 change code on the Pi, so after merging, the Pi's checkout needs a
pull (OPERATIONS.md section B), and one `--dry-run` there confirms it.
