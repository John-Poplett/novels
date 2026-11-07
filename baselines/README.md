# Cross-validation baselines

Reference results for the elastic-net sweep, kept in git so a later run can be
compared against them. The files are what `tar_read(cv_summary)` and
`tar_read(cv_results)` returned.

| File | Contents |
|---|---|
| `cv_summary.csv` | Per `alpha`: mean, SD, min and max test AUC across the folds |
| `cv_results.csv` | The 50 per-(fold, `alpha`) test AUCs and nonzero-coefficient counts |

## Current baseline

Produced after the Gutenberg boilerplate fix.

- Pipeline run: `tar_make()`, 2026-11-07, 216 targets, 5m 39.5s.
- Corpus: 796 rows over 754 books, fingerprint `4cea80937cee9b4c8a1151717d7d4ff8`.
- Folds: the stored `author5_v1` table, author-grouped and append-only. For this corpus it equals `createKFoldSplit(novels, 5, seed = 42)`.

The fingerprint is `digest::digest(targets::tar_read(novels))`, which is the
same value as `digest::digest(load("data/novels.rda")$novels)`. It identifies
the shipped object, not the text corpus alone: it changes when any column
changes, including one that no result depends on.

## History

| Baseline | Corpus | Mean test AUC range across `alpha` | Where to find it |
|---|---|---|---|
| Pre-fix, 2026-10-29 | 798 rows, 756 books (`f87ed8388486cdf9476bfcb559a93cb2`) | 0.686–0.739 | git commit `23b5baf` |
| Post-fix, 2026-10-30 | 796 rows, 754 books (`25f3cb40af928b414c6f03e9dd53d1f7`) | 0.695–0.746 | git commit `a1ad0a8` |
| Post-boilerplate, 2026-11-07 | 796 rows, 754 books (`4cea80937cee9b4c8a1151717d7d4ff8`) | 0.695–0.745 | current files |

The 2026-10-30 fingerprint was recorded by an earlier call whose exact form
was not written down, and a digest depends on row order and attributes as well
as content, so the three values identify snapshots rather than measure how far
apart they are. The counts and dates are the reliable identifiers.

The LibriVox fix moved every `alpha`'s mean up by 0.002–0.015 (mean +0.0085);
none by more than two standard errors. The boilerplate fix moved them by at
most 0.0025 in either direction (mean −0.0001); none by more than one standard
error, and the automatic check raised no warning. That is the expected size
for a change to 0.002% of the corpus characters.

## How to read a comparison

The standard error of a 5-fold mean AUC is `sd_auc / sqrt(5)`, about 0.013 to
0.033 depending on `alpha`. A change smaller than about 2 standard errors is
indistinguishable from fold-to-fold noise, so this baseline can catch gross
regressions in the modelling or the pipeline, not small data-integrity
problems. Those are covered by the unit tests.

## Automatic check

The pipeline target `cv_baseline_check` compares each run's `cv_summary` with
`cv_summary.csv` and **warns** for any `alpha` whose mean AUC moved by more
than two standard errors. It warns and does not fail, because legitimate data
or model changes move AUC. `tar_make()` reports it as "1 targets produced
warnings"; `targets::tar_meta(fields = warnings, complete_only = TRUE)` shows
the message, and `tar_read(cv_baseline_check)` shows the full comparison. If
the alpha grid changes, the target stops with an error, because the results are
no longer comparable.

## Updating

When a change is expected to move AUC, replace these files in the same commit
and record the old and new numbers and the reason in `CHANGELOG.md`. Git
history keeps the earlier baselines.
