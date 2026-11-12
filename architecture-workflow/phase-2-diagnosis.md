# Phase 2 — Diagnosis: `novels`

> **Historical.** This records the 2026-11-06 architecture revision and is not
> maintained. Current documentation starts at [README § Documents](../README.md#documents).

**Date:** 2026-11-06
**Inputs:** `architecture-workflow/phase-1-discovery.md`, direct measurement (test suite,
`tar_manifest()`, `system.file()` in both execution contexts, package check).
**Review lens:** general software architecture (coupling/cohesion, layering, testability,
reproducibility) plus R-package hygiene. No cloud/frontend/microservice lens applies.

---

## 1. Architecture health score

| Dimension | Score | Key issue |
|---|---:|---|
| Data integrity & correctness | **8/10** | Strong asserts and a schema lock; weakened by 3 live-fetched texts and untested parsers |
| Reproducibility & determinism | **5/10** | Archive is pinned, but the stored-fold mechanism is silently bypassed and 3 texts are fetched live |
| Test coverage & test health | **4/10** | The suite is currently **red**; the core data/feature/model functions have no unit tests |
| Code organization & cohesion | **5/10** | One 630-line module holds 5 concerns (44% of package code); dead code present |
| Documentation accuracy | **6/10** | Excellent explanatory prose; several concrete numbers/claims have drifted |
| Configuration & environment | **7/10** | URL in `.env`, digest pinned in code; one hard-coded LAN IP in dead code |
| Packaging & API hygiene | **5/10** | `man/` and `data/` are git-ignored; exported functions lack a shipped input |
| Maintainability & extensibility | **5/10** | Metaprogrammed pipeline, duplicate fold logic, no CI |
| **Overall** | **5.6/10** | Sound statistical and data-integrity design, undercut by verification and packaging gaps |

---

## 2. Verification evidence (measured, not inferred)

| Claim | Evidence |
|---|---|
| Test suite is red | `testthat::test_dir("tests/testthat")` → `[ FAIL 2 \| WARN 2 \| SKIP 0 \| PASS 99 ]` |
| Stored folds are bypassed in the pipeline | `system.file("extdata","author_folds.csv", package="novels")` → `""` under `source()` of `R/*.R`; → the dev path under `pkgload::load_all()`. `file.exists("")` is `FALSE`, so the fallback branch runs. |
| The installed package is stale | `packageVersion("novels")` = 1.0.3; `list.files(system.file("extdata", package="novels"))` = empty |
| Target count | `tar_manifest()` → **216** targets (README says 213; `baselines/README.md` says 207) |
| `cv_summary` schema | `baselines/cv_summary.csv` has `alpha, mean_auc, sd_auc, min_auc, max_auc` — no nonzero-coefficient column |
| README's nonzero column is real but not from `cv_summary` | Recomputing means from `baselines/cv_results.csv` gives 4078.8, 2514.4, … matching the README's 4079, 2514, … |
| Dead code | `grep -rn XparseMetaData` → definition only; `grep -rn "embed("` → definition only |
| Unused pipeline packages | `grep usethis\|assertthat R/ _targets.R DESCRIPTION` → only `_targets.R`'s `packages` vector |
| Vignette iterator reuse is **not** a live bug | Reusing an `itoken` for two `create_dtm()` calls returned 50×50 both times |
| Pipeline target is well-formed | `tar_manifest()` shows `checkCvBaseline(cv_summary, ..., k_folds)` — an earlier reading of `_targets.R` was wrong; the target does pass `k` |
| Package check result | `devtools::check(document = FALSE, build_args = "--no-build-vignettes", args = c("--no-manual","--no-build-vignettes"))` → **1 ERROR, 3 WARNINGs, 1 NOTE**, 36m59s. The ERROR is F2 (2 test failures). One WARNING is F17 (codoc mismatch). The other two WARNINGs (`inst/doc` missing) and the NOTE (`architecture-workflow` at top level) are artifacts of this local-iteration mode and of the new checkpoint docs; the NOTE is already fixed by adding `^architecture-workflow$` to `.Rbuildignore`. Both vignettes' R code **ran successfully** (139s + 2m). |

> **Correction recorded deliberately:** I initially believed `cv_baseline_check` omitted `k`.
> `tar_manifest()` disproved it. This is why the diagnosis cites executed values, not code
> impressions. The real gap is that the current `_targets.R` has **never been run** (`_targets/`
> is dated Oct 30; the file was edited Nov 5), so the stored-fold path and the baseline target
> have no executed result behind them.

---

## 3. Findings

Severity: **CRITICAL** = wrong results or unusable system; **HIGH** = silent wrongness or a gate
that cannot pass; **MEDIUM** = real but contained; **LOW** = hygiene.
Effort: **Quick** < 1 h; **Moderate** 1–4 h; **Significant** > 4 h.

| ID | Finding | Severity | Impact | Effort | Risk | Unlocks |
|---|---|---|---|---|---|---|
| **F1** | **Stored fold table is silently bypassed.** `createKFoldSplit(..., use_stored_folds = TRUE)` resolves the table with `system.file(..., package = "novels")`. Under the pipeline's `tar_source("R")` this reads the *installed* package (1.0.3, no `extdata/`), gets `""`, and falls back to the seed **without a message**. | HIGH | The append-only fold guarantee added in commits `62026de`/`1d1db83` is not in force. Today the fallback happens to reproduce the same folds, so results are unaffected; the next dataset growth would silently reshuffle 75–80% of authors and invalidate every baseline. | Moderate | Medium | Deterministic dataset expansion; trustworthy baselines |
| **F2** | **The test suite is red.** `tests/testthat/test-foldAssignment.R` loads `data/novels.rda` relative to `tests/testthat` and calls `createKFoldSplit(..., k_folds = 5)` with a non-existent argument name. | HIGH | `devtools::check()` cannot reach 0/0/0; no CI is possible; the documented gate is unusable. | Quick | Low | Verification of every later fix |
| **F3** | **Core data-integrity code is untested.** `parseMetaData`, `stripGutenbergBoilerplate`, `buildVectorizer`, `fitTfidf`/`transformTfidf`, `fitGlmnetModel`, `evaluateModel`, `classifyAt`, `createConfusionMatrix`, `results2Text` have zero tests. | HIGH | A regression in parsing or boilerplate stripping is caught only by a ~5.5-minute pipeline run that needs the network. The README's "each fix backed by an assert" is true only at pipeline runtime. | Moderate | Medium | Safe split of `targetFunctions.R` (F4) |
| **F4** | **`R/targetFunctions.R` is a 630-line god module** covering archive I/O, metadata parsing, corpus repair, feature engineering, and modelling. 44% of the package's R lines. | MEDIUM | High coupling; every change touches one file; tests must source the whole module; new contributors must read 630 lines to find one function. | Significant | **HIGH** (file split) | F3, F9; long-term maintainability |
| **F5** | **The corpus is not hermetic.** `patchContaminatedText()` calls `gutenbergr::gutenberg_download()` for 3 replacement ids on every clean run. | MEDIUM | The pinned archive no longer determines `novels`: the same archive + same code can produce different text if Gutenberg changes. The README's reproducibility claim is overstated. | Moderate | Medium | A genuinely pinned dataset release |
| **F6** | **Exported genre API has no shipped input.** `assignGenre()` and `compareGenreAssignment()` are exported and documented around `genre_allowlist.csv`, which exists only in git-ignored `tmp/`. Neither is used by the pipeline. | MEDIUM | Users cannot call the documented API; the functions look supported but are scaffolding. | Quick | Low | Clear roadmap surface for genre expansion |
| **F7** | **Documentation drift.** README says 213 targets; `baselines/README.md` says 207; reality is 216. README's results table says it is `tar_read(cv_summary)` but its last column is derived from `cv_results`. `TODO.md` still asks for "a target to compare a target run against the current baseline", which `cv_baseline_check` already does, and its first item is to reconcile itself with the README Roadmap — the two lists already overlap and diverge. | MEDIUM | Undermines the project's central claim that numbers are read back from the store, not hand-copied; readers cannot tell what is done from what is planned. | Quick | Low | Trust in the published results; a single roadmap |
| **F8** | **`man/*.Rd` and `data/novels.rda` are git-ignored**, while the documented check uses `document = FALSE`. | MEDIUM | A fresh clone has no man pages and no data; the documented gate fails before it can assess anything. CRAN source tarballs built from git would be incomplete. | Quick–Moderate | **CRITICAL** (build/packaging policy) | Reproducible builds; CI |
| **F9** | **Dead code:** `XparseMetaData()` (~60 lines, older copy of `parseMetaData`), `R/embed.R` (5 lines, unexported, unused, hard-codes `192.168.2.118:11434`, shadows `base::embed`). | LOW | Maintenance confusion; a hard-coded LAN address invites environment bugs if it is ever used; deleting `embed.R` also lets `httr2` leave `Imports`. | Quick | Low | Smaller surface for F4; a truthful dependency list |
| **F10** | **`_targets.R`'s `packages` vector lists unused `usethis` and `assertthat`**, neither in `DESCRIPTION`. | LOW | Noise; suggests dependencies that do not exist. | Quick | Low | — |
| **F11** | **`tests/testthat/_problems/` (14 files) is tracked** and not git-ignored. | LOW | Stale testthat failure artifacts in the repo. | Quick | Low | — |
| **F12** | **`data-raw/novel_meta.csv` (78 KB) is tracked but referenced by no code**; it carries an obsolete `file.name` schema. | LOW | Duplicate/stale data that can be mistaken for the source of truth. | Quick | Low | — |
| **F13** | **`assignAuthorFolds()` and `makeFoldid()` duplicate the same seed/sample logic.** | LOW | Two places to change; easy to let them diverge. | Quick | Low | — |
| **F14** | **The extracted corpus is not tracked as a file target.** `rawdataset_path` is a value target returning a path string; `fetchRawData()` trusts an existing `data-raw/novels/` without re-verifying the digest, and `targets` cannot see that the directory changed or vanished. A partial extraction is used silently. | LOW | A half-extracted corpus is invisible to the dependency graph; combined with F5, the corpus's provenance is weaker than the README claims. | Moderate | Low | A corpus target that actually tracks its files |
| **F15** | **No CI.** The gate is a manual 6–10 minute command. | LOW | F2 and F8 can recur unnoticed; "tests pass on any machine" is asserted, not enforced. | Moderate | Low | Prevents recurrence of F2/F8 |
| **F16** | **The `glmnet` vignette uses a different feature space from the pipeline** (trigram default in `text2vocab()` vs. the pipeline's unigrams) and is memory-heavy: a check with vignette building was still knitting it after several minutes at >5 GB RSS. | LOW | The level-3 gate is fragile on a laptop; readers may assume the vignette reproduces the README table when it uses a different split, n-grams, and `alpha`. | Moderate | Low | A gate that can actually be run routinely |
| **F17** | **`man/createKFoldSplit.Rd` is stale.** Check reports a codoc mismatch: code is `function(data, k = 5, seed = 42, use_stored_folds = FALSE)`, Rd is `function(data, k = 5, seed = 42)`. The **roxygen source is correct** (`@param use_stored_folds` is present); the generated Rd predates the argument. | LOW | One of the three check WARNINGs. Not an independent defect: it is a *symptom* of F8 — `man/` is git-ignored and the documented gate passes `document = FALSE`, so the Rd is never regenerated before checking. | Quick | Low | A check that can reach 0/0/0 (with F8) |
| **F18** | **The documented check is wrong about both duration and behaviour.** `AGENTS.md` level 2 says "~6 minutes" and "skips only executing the vignette code". Measured: **36m59s**, and the vignette R code **did run** (`checking running R code from vignettes` → `glmnet.Rmd` 139s, `novels-text-vectorization.Rmd` 2m). `--no-build-vignettes` skips *knitting/building* the documents, not executing their code. That mode also necessarily emits two `inst/doc` WARNINGs, so it can never be a 0-warning run. | LOW | Developers budget 6 minutes and get 37; they believe vignette code is skipped when it is not, so a vignette breakage is discovered at the wrong gate. The stated level-2/level-3 distinction does not hold as written. | Quick | Low | A gate whose cost and coverage are known |

> **Measurement caveat for F18:** the run added `build_args = "--no-build-vignettes"` (the
> local-iteration variant `AGENTS.md` describes). It therefore did not build vignettes, but
> R CMD check still extracted and ran their R code — which is exactly the behaviour the
> documentation gets wrong. The 36m59s figure is for that variant; level 3's duration was
> not measured.

---

## 4. Issue dependency map

```text
F2 (green suite) ──unlocks──→ verification of F1, F3, F4
F3 (characterization + fix tests) ──unlocks──→ F4 (file split) safely
F4 (split targetFunctions.R) ──depends on──→ F3
F4 ──unlocks──→ F9 cleanup, easier F5 work
F1 (stored folds) ──independent── but needs a regression test (part of F3)
F5 (pin replacement text) ──independent── (data-policy decision)
F6 (genre API) ──independent── (product decision: ship allow-list or unexport)
F7, F9, F10, F11, F12, F13, F18 ──independent hygiene──
F17 (stale Rd) ──depends on──→ F8 (document = FALSE + ignored man/)
F14 (re-verify extraction) ──independent──
F15 (CI) ──depends on──→ F2, F8; then guards everything
```

Critical path: **F2 → F3 → F4**. F1 is the most dangerous silent defect and should be fixed
immediately after F2, with its regression test landing in F3.

---

## 5. Refactoring risk assessment

| Finding | Risk level | Why | Protocol |
|---|---|---|---|
| F4 — split `targetFunctions.R` | **HIGH** | Moving complete functions between files; `_targets.R` sources `R/` wholesale, so a missing/renamed function breaks every downstream target. | Full file-split protocol: move whole functions, verify each new file loads, keep the original until verified, ≤5 files per round, git checkpoint per round. |
| F8 — commit `man/` / change check policy | **CRITICAL** | Changes build/package configuration and what ships to CRAN. | User approval required before proceeding. |
| F3 — extract pure helpers and add tests | **MEDIUM** | New internal helpers, new test fixtures; import chain can break. | TDD + compilation/load gate + checkpoint. |
| F5 — pin replacement text | **MEDIUM** | Changes the dataset's provenance and may change bytes; baselines may move. | TDD + baseline comparison + checkpoint. |
| F1 — path resolution + fail-loud | **LOW–MEDIUM** | Single function plus a helper; changes fallback behavior to an error. | Standard TDD. |
| F2, F6, F7, F9–F13, F17, F18 | **LOW** | Single-file or documentation changes. | Standard TDD / edit. |

---

## 6. Proposed fix order

1. **Foundation — make the gate green and the folds real**
   - **F2** fix `test-foldAssignment.R` (use the package dataset, `k = 5`).
   - **F1** resolve the stored-fold path from the dev tree as well as the installed package;
     **stop with an actionable error** if the table is missing; add a regression test that does
     not depend on `pkgload`'s `system.file` shim.
2. **Close the verification gap**
   - **F3** add characterization + fix tests for `parseMetaData`, `stripGutenbergBoilerplate`,
     and the feature/model functions. Land F1's regression test here.
3. **Reproducibility**
   - **F5** pin the three replacement texts (ship them, or re-cut the archive and re-pin).
   - **F14** re-verify an existing extraction (or always verify).
4. **Hygiene**
   - **F7** correct target counts and the `cv_summary` claim (or add the column to `cv_summary`).
   - **F9** delete dead code; **F10** trim `packages`; **F11** untrack `_problems/`;
     **F12** remove or document `novel_meta.csv`; **F13** de-duplicate fold logic.
   - **F16** decide whether the vignette should use unigrams (fast, comparable) or stay a
     deliberately separate trigram demo; document the difference either way.
   - **F18** replace the level-2/level-3 durations and the "skips executing the vignette
     code" claim with measured behaviour, and say that `--no-build-vignettes` cannot be a
     0-warning run.
5. **Structural (only after step 2 is green)**
   - **F4** split `targetFunctions.R` by concern under the file-split protocol.
6. **Decisions for the maintainer**
   - **F6** ship `genre_allowlist.csv` in `inst/extdata/` and document it, or unexport the two
     functions until the expansion work starts.
   - **F8** commit `man/`, or change the documented check to run `document = TRUE`, or add a
     `make check` wrapper that documents first. This also clears **F17** — regenerating the
     Rd removes the codoc mismatch; there is nothing to fix in the roxygen source.
7. **Optional, high leverage**
   - **F15** add a minimal GitHub Actions workflow running the fast test loop and
     `R CMD check --no-manual --no-build-vignettes`.

**Estimated total effort:** ~15–21 hours, of which F4 is ~4–6 hours and F3 ~3–4 hours.
F17 is absorbed by F8 (regenerate the Rd); F18 is ~30 minutes.

---

## 7. What is working well (preserve these)

- **Data-integrity engineering.** The file-conservation assert, the post-patch "no LibriVox
  text remains" check (the earlier version checked only the input), the row-count and
  no-NA-count asserts, and the schema-lock test are exactly the right instincts.
- **Author-disjoint fold design.** Explicit, tested, and measured: 0 of 756 books appear in more
  than one test fold; the native fold column is deliberately not pooled because 36 of 41
  multi-genre books sit in different native folds.
- **Statistically honest evaluation.** Repeated CV, shared inner `foldid`, noise-aware baseline
  tolerance (2 SE), and prose that refuses to name a "best alpha" from noise.
- **Explanatory documentation.** The README states grain, known defects, and the limits of its
  own claims — rare and valuable.
- **Configuration discipline.** URL in `.env`; digest deliberately pinned in code because
  `targets` does not hash environment variables.
- **Scoring/threshold separation.** `createPredictor()` scores; `classifyAt()` thresholds. No
  modeling decision is hidden in a closure.
