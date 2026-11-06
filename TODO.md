# TODO
* [ ] Reconcile this list with the roadmap section in README.md
* [ ] **Expand dataset**
  * [ ] Propose two to three mechanisms for deducing a Gutenberg texts genre(s). Give the pros and cons. Write the proposal up in a document "deducing-genres.md". Consider:
    * Training a multinomial genre classifier (titles can belong to more than one genre) based on the existing dataset 
    * Connecting to an Open Library MCP server to scan a title's subjects for genre candidates.
    * Using Gutenberg metadata to deduce genres
  * Plan on adding at least two new genres: "Flash Fiction" and "Young Adult Fiction". Use the "Flash Fiction" category for a demo dataset
that might produce a small enought dataset to meet CRAN's tight project size constraints.
  * [ ] Refresh the download counts (this should care of the labels of the three patched LibriVox rows (19513, 19640, 19836). They still carry the audio entries' counts, not the books'. 19836 stands in for *Early Spring*; *The Verse-Book of a Homely Woman* (3477) has its own count. Replace `baselines/` afterwards and record the AUC effect in CHANGELOG.md.
 * Initially qualify the expansion code by having it attempt to reproduce the existing dataset from scratch based solely on gutenberg_identifiers. 
The quality of the expansion code corresponds to how faithfully it reproduces the existing dataset, e.g. getting the genre classifications to match) and evading prior pitfalls such as inclusion of Gutenberg boilerplate and mistaking librivox metadata for actual literary text.
* Maintain the original 50/50 split of success and failure 
* Decide when and how to replace binomial classification with a continuous response based on the pros and cons of each.
* Then the expansion, with the download-count refresh and a decision on how labels are defined for old and new books together.
* Parquet and the CRAN packaging (the sample, the download, the precompute run).
  * [ ] Swap out the current R-specific RDA format to Parquet for the default dataset format
  * [ ] CRAN size: the tarball would be ~76 MB against a ~5 MB guideline. Ship a small sample in the package (whole books, every genre) and fetch the full corpus through the pinned download, as Parquet. Measured: Parquet/zstd-22 is only ~11% smaller than the current bzip2 RDA, so format does not solve size.
* [ ] Precompute the vignettes for CRAN: rename `vignettes/*.Rmd` to `.Rmd.orig`, add `^vignettes/.*\.Rmd\.orig$` to `.Rbuildignore`, run `data-raw/precompute-vignettes.R` against the full corpus.
* [ ] Strip transcriber boilerplate that survives `stripGutenbergBoilerplate()`: 54 of 100 Science Fiction texts carry "extensive research did not uncover any evidence that the u.s. copyright… was renewed", a genre fingerprint. Also front matter in some replacements ("[this e-text comes in three forms…", "[redactor's note…").
* [ ] Silence the intended many-to-many join warning in `tidyRawData()` with `relationship = "many-to-many"` (co-authored books × multi-genre books; the following `summarize()` collapses it).
* [ ] `createKFoldSplit()` assigns folds by position in `unique(data$author)`, so the fold map depends on row order (removing two rows moved 2 of 514 authors). Sorting the authors would fix it but reshuffles every fold; do it deliberately and reset `baselines/` with it.
* [ ] **Embeddings**
  * [ ] Review and update chunking discipline
  * [ ] Setup a local TEI server 
  * [ ] Integrate code to use TEI server for features
* [ ] Expand dataset
  * Consider replacing Sunny Brook's directory  structure for Gutenberg's and the possibility of keeping all texts
in a Gutenberg mirror
  * Automate retrieval of text from Gutenberg.org
  * Create an extensions directory for larger data sets
* [ ] Try linear regression modeling on download counts instead of imposing a binary classifier. (measured case for it, and the sampling caveat, under README Roadmap)
* [ ] Finish SCHEMA.md. Make it a document of versions, including at least original and the first update.
* [ ] Upgrade glmnet.rmd to litedown or Quarto 
* **Refactoring**
  * [ ] Upgrade all magrittr %>% pipe symbols to R's |>
  * [ ] Standardize on snake_case format for var and function names
  * [ ] Consider replacing metaprogramming logic in _targets.R with more pedestrian code. It's Lispy cool but difficult to debug.

# Done (not published)

&#9989; Add a target to compare a target run against the current baseline (`cv_baseline_check`, flagging any `alpha` more than two standard errors from `baselines/cv_summary.csv`).

&#9989; Move fold configurations out of the dataset but preserve the attribute of making them constant and repeatable.
  * Decided 2026-10-30: the original columns (`fold`, `download_count`, `response`) stay in `novels`, frozen and documented as Sunnybrook's. Anything new goes in separate tables.
  * Generated folds become stored data, not a seed: `(author, scheme, fold)`, with `author5_v1` = today's seed-42 assignment, **append-only** (existing authors never move; new authors are assigned and recorded). R's `set.seed()` cannot be reproduced in other languages, and the current positional assignment reshuffles folds whenever the data grows. The author key must handle co-authored books.
  * Fresh download counts go in their own table: `(gutenberg_id, snapshot_date, source, downloads)`. First find out what the 2013 count measured and what can be fetched per book now.
  * Order: folds table before expanding the dataset; counts refresh together with the expansion.
  * Done: `author5_v1` stored in `inst/extdata/author_folds.csv` (514 authors), verified against the pipeline's `fold_assignments`; `data-raw/author-folds.R` refuses to overwrite it. Next: make the pipeline read the table instead of recomputing the seed; documentation for appending new authors; export it with the Parquet dataset.

&#9989; Fix LibriVox contamination (the patch had been a silent no-op)
  * Three books patched with text from a different id, two dropped as duplicates of books already in the corpus; 796 rows over 754 books
  * Pipeline now asserts that no LibriVox text remains, and the regression test mocks the Gutenberg download
  * `data/novels.rda` is built by the `novels_data` pipeline target; `data-raw/novels.R` retired; `_targets/` ignored
  * Effect on AUC: +0.002 to +0.015 at every `alpha`, none beyond two standard errors (see `baselines/`)

&#9989; Cross-validation baseline in `baselines/`, with a pipeline target that warns when AUC moves more than two standard errors

&#9989; Validate folds to guard against duplicates and leaks.

&#9989; Configuration moved out of code: sample `.env` (`.env.example`) plus a pinned archive digest
  * `fetchRawData(data_path, archive_url, archive_sha256)` takes the archive location as an argument, defaulting to `NOVELS_ARCHIVE_URL`; the URL now lives only in `.env`/`.env.example` (git-ignored and git-tracked respectively), so it can move to HuggingFace without a code change
  * The archive's SHA-256 is pinned as a literal `archive_sha256` target in `_targets.R` and verified before extraction, so a moved mirror or corrupted download aborts instead of silently changing the corpus
  * `_targets.R` loads `.env` via `readRenviron()`; `archive_url` and `archive_sha256` are targets so both appear in the dependency graph
  * `verify_sha256()` added (internal) with tests for match, case-insensitive match, mismatch, and for refusing a fetch with no URL or no pinned digest

&#9989; Restore `devtools::check()` to clean operation: 0 errors, 0 warnings, 0 notes
  * Regenerated NAMESPACE and man/ pages; dropped the stale `getFreshGutenbergTextMetaData` export (the sketch lives in `tmp/` until it ships)
  * `.Rbuildignore` keeps `_targets/`, `papers/`, `tmp/`, `.RDataTmp` and loose notes out of the tarball, fixing the non-portable-path error
  * DESCRIPTION Imports/Suggests now match actual use; added `LazyDataCompression: xz` and `Depends: R (>= 4.1.0)`
  * dplyr/tidyr NSE column names declared via `utils::globalVariables()`; missing `@param` docs added
  * Removed an undeclared `library(devtools)` from the glmnet vignette; declared `data.table` for the older vectorization vignette

&#9989; Document fixes and caveats from original study:
  * Corrupt meta-data file
  * Duplicate files in dataset
  * Authors duplicated across folds
  * Comment on download counts
  
&#9989; Integrate with gutenbergr
  * Define a novels\_metadata function
  * Use author\_id and gutenberg\_ids
