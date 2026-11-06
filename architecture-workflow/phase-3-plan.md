# Phase 3 — Fix plan and progress

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
| F6 | Genre API: ship input or unexport | Low | **needs decision** | — |
| F8 | `man/`/`data/` packaging policy | **Critical** | **needs approval** | — |
| F15 | Minimal CI | Low | pending | — |

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

## New findings found during Phase 3
- **F19 (LOW–MEDIUM): the corpus census is recorded but never asserted.** `parseMetaData()`
  returns `census$truncated` and `census$absent`, and `tidyRawData()` then `inner_join`s
  files to metadata, silently dropping any file with no metadata row. The file-conservation
  assert only catches metadata rows with no file, not files with no metadata row. Today
  exactly one trailing record is truncated (the known defect), but nothing pins that count:
  a future metadata change could silently drop books. Candidate fix: assert
  `length(census$truncated) == 1` and log `absent` by id. Deferred to Phase 4.
