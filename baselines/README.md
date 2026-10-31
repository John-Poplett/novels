# Cross-validation baselines

Reference results for the elastic-net sweep, kept in git so a later run can be
compared against them. The files are what `tar_read(cv_summary)` and
`tar_read(cv_results)` returned.

| File | Contents |
|---|---|
| `cv_summary.csv` | Per `alpha`: mean, SD, min and max test AUC across the folds |
| `cv_results.csv` | The 50 per-(fold, `alpha`) test AUCs and nonzero-coefficient counts |

## Current baseline

Produced before the LibriVox contamination fix.

- Pipeline run: `tar_make()`, 2026-10-29 21:26.
- Corpus: 798 rows over 756 books (digest `f87ed8388486cdf9476bfcb559a93cb2`).
- Folds: `createKFoldSplit(novels, 5)`, seed 42, author-grouped.

## How to read a comparison

The standard error of a 5-fold mean AUC is `sd_auc / sqrt(5)`, about 0.009 to
0.034 depending on `alpha`. A change smaller than about 2 standard errors is
indistinguishable from fold-to-fold noise, so this baseline can catch gross
regressions in the modelling or the pipeline, not small data-integrity
problems. Those are covered by the unit tests.

## Updating

When a change is expected to move AUC, replace these files in the same commit
and record the old and new numbers and the reason in `CHANGELOG.md`. Git
history keeps the earlier baselines.
