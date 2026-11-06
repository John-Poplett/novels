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
| F2 | Fix `test-foldAssignment.R` | Low | in progress | — |
| F1 | Stored-fold resolution + fail-loud + regression test | Low–Medium | pending | — |
| F3 | Characterization + fix tests for core functions | Medium | pending | — |
| F5 | Pin the three replacement texts | Medium | **needs approval** | — |
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
