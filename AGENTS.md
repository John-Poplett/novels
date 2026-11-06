# Project
This project originated from work described in the paper, "Success
with Style: Using Writing Style to Predict the Success of Novels",
presented at the EMNLP conference in 2013. The dataset will continue
to evolve from the original both in "tidiness" and magnitude.

The project uses the R language, almost exclusively. Python extensions
are permissible when like facilities are not available in R.  The
coding style leans towards functional coding (e.g., pipelines and
closures) and should shy away from metaprogramming for the sake of
simplicity.

Version 1.03 was published in 2019.
At least one new version will be published before the end of 2026.

Recommended reading: README.md, TODO.md and the project's source code files.

# Engineering Standards (non-negotiable)

## Scope discipline
* One work item per iteration (one function or one tight cluster).
* Prefer small diffs; avoid drive-by refactors.
* Write acceptance criteria that are testable.

## Correctness discipline
* Validate inputs early; fail fast with actionable messages.
* Preserve backward compatibility but **not** at the expense of  justifiable improvements.
* As the dataset evolves, at each significant milestone (e.g., a schema or sample-size change),
the code to reproduce the dataset is tracked in committed in the project's git repo and labeled.
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
check is the gate before publishing a release. The `Makefile` wraps all three
so the Rd regeneration cannot be forgotten (see the `man/` note below).

1. Fast loop, seconds:

```bash
make test   # pkgload::load_all(); testthat::test_dir("tests/testthat")
```

For a single file, call the equivalent directly:

```r
pkgload::load_all()
testthat::test_file("tests/testthat/test-theThing.R")
```

2. Package check, vignettes built once but not re-built — measured **6m50.8s**
   with `0 errors | 0 warnings | 0 notes` (2026-11-06):

```bash
make check  # roxygen2::roxygenise() then devtools::check(...)
```

Equivalent to:

```r
devtools::check(
  document = FALSE,
  args = c("--no-manual", "--no-build-vignettes")
)
```

`R CMD build` still knits the vignettes, so `inst/doc` is populated and the
check does not report the missing-`inst/doc` warnings. The flag passed to
`R CMD check` only skips "checking re-building of vignette outputs", which is
the second, redundant execution of the vignette code. So this level runs every
test, example, Rd, namespace and dependency check **and** the vignettes'
modelling code, and it can reach 0/0/0.

> Two measurements disagree, deliberately recorded rather than averaged away.
> The 2026-11-06 pre-fix run took **36m59s** with `build_args =
> c("--no-build-vignettes")` as well; that variant skips building the
> vignettes, necessarily reports two `inst/doc` WARNINGs, and so cannot be a
> 0-warning run. The post-fix `make check` above, on the same machine a few
> hours later, took 6m50.8s. The cause of the 30-minute difference was not
> established (machine load or package-install cache is the obvious
> candidate). Treat 6–40 minutes as the range and read the run's own
> `Duration:` line rather than budgeting from a guess.

3. Full gate, vignettes rebuilt:

```bash
make check-full  # roxygen2::roxygenise() then devtools::check(document = FALSE)
```

Must end `0 errors | 0 warnings | 0 notes`. The only difference from level 2 is
that checks re-build the vignette outputs, so the vignette code runs a second
time and the tarball is verified the way a consumer receives it. Measured
**7m39.1s** (2026-11-06, vignette re-build 301s) — only ~50s longer than level 2,
because both modes run the vignette code once and the reported duration covers
`R CMD check`, not the preceding `R CMD build`. Run it before
merging or publishing, and always when `R/`, `DESCRIPTION`, `man/` or
`vignettes/` change.

`make check-full` passes `--no-manual` because this machine has no LaTeX
installation (`Sys.which("pdflatex")` is empty), and without the flag the PDF
manual check fails for that reason alone. On a machine with LaTeX, drop the
flag for a release — that is the only part of the CRAN check the wrapper does
not cover.

**`man/` is git-ignored**, so `document = FALSE` checks whatever Rd files
happen to be on disk. After changing roxygen comments, run `make document`
(or `devtools::document()`) first or the check reports stale codoc mismatches
that do not exist in the source. `make check` and `make check-full` do this
for you; `devtools::check()` called by hand does not.

**Rule:** levels 1 and 2 while iterating on a feature; level 3 before merging
or publishing, and always when `R/`, `DESCRIPTION`, `man/` or `vignettes/`
change. Never delete a vignette, or disable it permanently, to make level 3
pass — a failing vignette is a failing pipeline.

`data/novels.rda` is git-ignored and produced by the pipeline, not by the
package build: `make data` (i.e. `targets::tar_make()`) regenerates it. A
check run on a fresh clone without it fails in the tests and vignettes that
load the corpus.

## Build

```bash
# TODO: add build command (e.g., npm run build)
```
