# Phase 3 — Fix plan and progress

> **Historical.** This records the 2026-11-06 architecture revision and is not
> maintained. Current documentation starts at [README § Documents](../README.md#documents).

**Date:** 2026-11-06
**Input:** `architecture-workflow/phase-2-diagnosis.md`
**Rule:** one finding per commit; each commit must leave the fast test loop green.
Findings F5, F8, F6 are decisions that may change the dataset or packaging policy and
are held for explicit approval before any bytes change.

Baseline (before any fix): `[ FAIL 2 | WARN 2 | PASS 99 ]` — both failures are F2.

## Status

| ID | Item | Risk | Status | Commit |
|---|---|---|---|---|
| F2 | Fix `test-foldAssignment.R` | Low | done | `52f73c8` |
| F1 | Stored-fold resolution + fail-loud + regression test | Low–Medium | done | — |
| F3 | Characterization + fix tests for core functions | Medium | done | — |
| F5 | Pin the three replacement texts | Medium | done | — |
| F14 | Re-verify extraction / track corpus files | Low | done | — |
| F7 | Correct README/TODO claims | Low | done | — |
| F9 | Delete dead code (`XparseMetaData`, `R/embed.R`) | Low | done | — |
| F10 | Trim unused `packages` in `_targets.R` | Low | done | — |
| F11 | Untrack `tests/testthat/_problems/` | Low | done | — |
| F12 | Remove or document `data-raw/novel_meta.csv` | Low | done | — |
| F13 | De-duplicate fold logic | Low | done | — |
| F16 | Vignette feature-space decision | Medium | done | — |
| F18 | Correct documented check behaviour | Low | done | — |
| F4 | Split `targetFunctions.R` | **High** | done | — |
| F6 | Genre API: ship input or unexport | Low | done | `e0665c2` |
| F8 | `man/`/`data/` packaging policy | **Critical** | done | `5b41326` |
| F15 | Minimal CI | Low | done | `ecefb26` |

## Log

- Baseline commit created with only `architecture-workflow/` and `.Rbuildignore`.
  User's uncommitted edits (`AGENTS.md`, `README.md`, `SKILL.md`, `TODO.md`) were left
  untouched and uncommitted.
- **F2** done: suite green (0 failures, 136 passes).
- **F1** done: `storedFoldPath()` resolves option → installed package → source tree, and
  `use_stored_folds = TRUE` now errors instead of falling back. Verified in the exact
  `tar_source()` context (`system.file()` → `""`) that the source-tree path is found and
  that stored folds equal seeded folds for today's 514 authors.
  Suite: 0 failures, 145 passes.
- **F3** done: added characterization tests for `parseMetaData` (fixture corpus, census,
  renames, truncation, conservation error), `stripGutenbergBoilerplate`,
  `results2Text`/`classifyAt`/`createConfusionMatrix`, the vectorizer/DTM/TF-IDF chain,
  and `fitGlmnetModel`/`evaluateModel`. Suite: 0 failures, 213 passes.
- **F5** done: the three replacement texts are now shipped as pinned, compressed
  snapshots in `inst/extdata/librivox_replacements/`, with SHA-256 verified before
  use. `data-raw/librivox-replacements.R` regenerates them and refuses to overwrite.
  Before committing, I verified the live Gutenberg texts are byte-identical to
  `data/novels.rda`, so this changes no corpus bytes and no baseline.
  Suite: 0 failures, 217 passes.
- **F14** done: new extractions write `.archive_sha256`; a reused extraction is
  checked against the pin, and an older extraction with no marker warns instead of
  being silently trusted. A missing `novel_meta.txt` is an error.
  Suite: 0 failures, 223 passes.
- **F9/F10/F13** done: deleted `XparseMetaData()` and `R/embed.R`, dropped `httr2` from
  `Imports`/NAMESPACE and `usethis`/`assertthat` from `_targets.R`'s `packages`, and made
  `makeFoldid()` delegate to `assignAuthorFolds()` so the seed/sample logic exists once.
- **F11/F12** done: untracked `tests/testthat/_problems/` (now ignored) and removed the
  unused, obsolete `data-raw/novel_meta.csv` (preserved in git history).
  Suite still 0 failures, 223 passes; `tar_manifest()` still 216 targets.
- **F16** done as documentation: the glmnet vignette now states that it uses one split and
  a 1–3-gram vocabulary, that its AUC is not comparable to the README table, and why it is
  slower and more memory-hungry than the pipeline.
- **F4** done: the 700-line `targetFunctions.R` is split into `corpusArchive.R`,
  `parseMetaData.R`, `tidyRawData.R`, `modelFunctions.R` and `corpusRepair.R` (largest 236
  lines). Units were moved whole by parsed source references, each new file was parsed and
  sourced independently before the original was deleted, and the suite stayed at 0 failures /
  223 passes. `tar_manifest()` still reports 216 targets, and the `source()` context resolves
  the stored data.
- **F7** done in README and `baselines/README.md`: target count 213 → 216, the results
  table now says which store object feeds each column, and `fold_assignments` is described
  as reading the stored table. `baselines/README.md` keeps its historical 207-target line
  but marks it as the 2026-10-30 run and notes the current count. The TODO edit (remove the
  already-implemented baseline-comparison item, add it to Done) is applied but **left
  uncommitted**, because TODO.md also carries the user's in-progress rewrite.
- **F18** done: AGENTS.md now records the measured 37-minute level-2 check, that vignette R
  code still runs under `--no-build-vignettes`, that this mode cannot be a 0-warning run,
  and that `man/` being git-ignored means `document = FALSE` can report stale codoc
  mismatches.
- **F6** done: the allow-list moved from git-ignored `tmp/` to `inst/extdata/genre_allowlist.csv`
  and is exposed by a documented `genreAllowlist()` accessor that resolves through
  `system.file()` with a source-tree fallback (the `storedFoldPath()` pattern). Tests pin the
  column contract, the include/exclude/review encoding, two known mappings and
  `allow_flagged = FALSE`. `assignGenre()`/`compareGenreAssignment()` docs now point at it.
  Suite: 0 failures, 243 passes.
- **F8** done as a wrapper rather than committing `man/`: `Makefile` targets `test`,
  `document`, `check`, `check-full`, `data`, `clean`. `check`/`check-full` run
  `roxygen2::roxygenise()` first, so the stale-Rd failure (F17) cannot recur. `Makefile` is in
  `.Rbuildignore`; AGENTS.md's Testing section now uses these targets and records that
  `data/novels.rda` comes from `tar_make()`, not from the build.
- **F15** done: `.github/workflows/R-CMD-check.yaml` regenerates Rd, builds only the
  `novels_data` target from the pinned archive (the corpus is git-ignored), runs the fast
  loop, then `R CMD check --no-manual --no-build-vignettes`. Vignettes are not built, so the
  job fails on errors only. YAML parses and `tar_manifest(names = "novels_data")` resolves;
  the workflow itself is unverified until the first push.

## Phase 3 integration gate

`make check` on the post-fix tree, 2026-11-06:

```
Duration: 6m 50.8s
0 errors ✔ | 0 warnings ✔ | 0 notes ✔
```

`make check-full` (vignette outputs re-built) also passed:

```
Duration: 7m 39.1s
0 errors ✔ | 0 warnings ✔ | 0 notes ✔
```

`make check-full` gained `--no-manual` in the same pass, because this machine has no TeX
(`Sys.which("pdflatex")` is empty) and without the flag the PDF-manual step fails for that
reason alone. The PDF manual is therefore the one part of a CRAN check not exercised here;
both vignettes are HTML, so no vignette output is lost.

This corrects two things the Phase 2 diagnosis recorded from a single pre-fix run:

- **The level-2 check can reach 0/0/0.** The earlier run added
  `build_args = c("--no-build-vignettes")`, which skips *building* the vignettes and
  therefore reports two `inst/doc` WARNINGs. `make check` passes the flag only to
  `R CMD check`, so `R CMD build` still knits the vignettes, `inst/doc` is populated, and the
  warnings never appear.
- **The 37-minute figure is not reproducible.** The same package checked 6m50.8s after that
  measurement. The cause of the difference was not established, so AGENTS.md now records both
  numbers and tells readers to use the run's own `Duration:` line.


## New findings found during Phase 3
- **F19 (LOW–MEDIUM): the corpus census is recorded but never asserted.** `parseMetaData()`
  returns `census$truncated` and `census$absent`, and `tidyRawData()` then `inner_join`s
  files to metadata, silently dropping any file with no metadata row. The file-conservation
  assert only catches metadata rows with no file, not files with no metadata row. Today
  exactly one trailing record is truncated (the known defect), but nothing pins that count:
  a future metadata change could silently drop books. Candidate fix: assert
  `length(census$truncated) == 1` and log `absent` by id. Deferred to Phase 4.
