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
| F14 | Re-verify extraction / track corpus files | Low | pending | — |
| F7 | Correct README/TODO claims | Low | pending | — |
| F9 | Delete dead code (`XparseMetaData`, `R/embed.R`) | Low | pending | — |
| F10 | Trim unused `packages` in `_targets.R` | Low | pending | — |
| F11 | Untrack `tests/testthat/_problems/` | Low | pending | — |
| F12 | Remove or document `data-raw/novel_meta.csv` | Low | pending | — |
| F13 | De-duplicate fold logic | Low | pending | — |
| F16 | Vignette feature-space decision | Medium | pending | — |
| F18 | Correct documented check behaviour | Low | pending | — |
| F4 | Split `targetFunctions.R` | **High** | pending (after F3) | — |
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

## New findings found during Phase 3
- **F19 (LOW–MEDIUM): the corpus census is recorded but never asserted.** `parseMetaData()`
  returns `census$truncated` and `census$absent`, and `tidyRawData()` then `inner_join`s
  files to metadata, silently dropping any file with no metadata row. The file-conservation
  assert only catches metadata rows with no file, not files with no metadata row. Today
  exactly one trailing record is truncated (the known defect), but nothing pins that count:
  a future metadata change could silently drop books. Candidate fix: assert
  `length(census$truncated) == 1` and log `absent` by id. Deferred to Phase 4.
