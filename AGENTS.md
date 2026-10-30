# Project
This project originated from work described in the paper, "Success
with Style: Using Writing Style to Predict the Success of Novels",
presented at the EMNLP conference in 2013. The dataset will continue
to evolve from the original both in "tidiness" and magnitude.

The project uses the R language, almost exclusively. Python extensions
are permissible when like facilities are not available in R.  The
coding style leans towards functional coding (e.g., pipelines and
closures)and should shy away from metaprogramming for the sake of
simplicity.

Version 1.03 was published in 2019.
Version 1.04 at the least will be published before the end of 2026.

Recommended reading: README.md, TODO.md and the project's source code files.

# Engineering Standards (non-negotiable)

## Scope discipline
* One work item per iteration (one function or one tight cluster).
* Prefer small diffs; avoid drive-by refactors.
* Write acceptance criteria that are testable.

## Correctness discipline
* Validate inputs early; fail fast with actionable messages.
* Preserve backward compatibility when not contradicted by a justifiable improvement.
* Make behavior deterministic (tests should pass on any machine).

## Testing discipline
* Every bug fix gets a regression test.
* New behavior must be tested on happy path + edge cases.
* Prefer stable assertions (values/classes) over brittle message snapshots.
* Keep tests small and local; avoid heavy fixtures unless they pay for themselves.

## API and UX discipline
* Use explicit namespacing in production code (pkg::fun).
* Errors should say: what happened, why, and what to do next.

## Package hygiene discipline
* Keep roxygen docs current; examples should run.
* Treat devtools::check() as a gate, not a suggestion.
* Track dependency intent: Imports (runtime) vs Suggests (tests/vignettes/optional).

## Setup

```bash
# one-time: point the pipeline at the "golden" archive
cp .env.example .env
```

Then, from the base directory:

```r
library(targets)
tar_make()
```

The archive's SHA-256 is pinned in `_targets.R`; `fetchRawData()`
verifies it before unpacking, so a moved or re-cut archive aborts
rather than silently changing the corpus.

## Testing

Three levels, cheapest first. Use the light ones while developing; the full
check is the gate before publishing a release.

1. Fast loop, seconds:

```r
pkgload::load_all()
testthat::test_file("tests/testthat/test-theThing.R")
```

2. Package check with vignettes not executed, ~6 minutes here:

```r
devtools::check(
  document = FALSE,
  args = c("--no-manual", "--no-build-vignettes")
)
```

Still installs the package and runs every test, example, Rd, namespace
and dependency check; it skips only executing the vignette code.
Measured 6m10s versus 10m21s for the full check. Adding 
`build_args = c("--no-build-vignettes")`
also skips building them once at build time, but yields a tarball 
with no built vignettes — local iteration only, never a release check.

3. Full gate, vignettes on, ~10 minutes:

```r
devtools::check(document = FALSE)
```

Must end `0 errors | 0 warnings | 0 notes`. This is the only level that runs
the vignettes' modelling code end to end, so it is the only one that proves
the pipeline still works from a clean install.

**Rule:** levels 1 and 2 while iterating on a feature; level 3 before merging
or publishing, and always when `R/`, `DESCRIPTION`, `man/` or `vignettes/`
change. Never delete a vignette, or disable it permanently, to make level 3
pass — a failing vignette is a failing pipeline.

## Build

```bash
# TODO: add build command (e.g., npm run build)
```
