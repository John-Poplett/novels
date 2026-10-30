# TODO
* [ ] Bring AGENTS.md up to snuff
* [ ] Move fold configurations out of the dataset but preserve the attribute of making them constant and repeatable.
* [ ] Swap out the current R-specific RDA format to Parquet for the default dataset format
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
