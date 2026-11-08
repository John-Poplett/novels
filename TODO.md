# TODO
* [ ] Reconcile this list with the roadmap section in README.md
* [ ] Keep the download series running: one snapshot a month (`Rscript data-raw/snapshot-downloads.R`), same day, from Project Gutenberg's own RDF dump. Trial snapshot taken 2026-11-07 (`data-raw/snapshots/2026-11-07/`: 79,762 books, 68,600,747 30-day downloads). Before 2026-12-31: freeze eligibility from that date's `pg_catalog.csv`, and settle the Fiction/Romance question recorded below.
* [ ] **Expand dataset**
  * [ ] Propose two to three mechanisms for deducing a Gutenberg texts genre(s). Give the pros and cons. Write the proposal up in a document "deducing-genres.md". Consider:
    * Training a multinomial genre classifier (titles can belong to more than one genre) based on the existing dataset
    * Connecting to an Open Library MCP server to scan a title's subjects for genre candidates.
    * Using Gutenberg metadata to deduce genres
  * Youth and form are **attributes, not genres** (decided 2026-11-07): `children_or_ya_shelf` and `form` (flash / short story / novella / novel). Use the flash subset as a demo dataset
that might produce a small enought dataset to meet CRAN's tight project size constraints. Measurements behind the decision — 5 flash books, no YA-only shelf, the combined shelf on 48 corpus books across seven genres — are in `PREREGISTRATION.md` §5.5 and `SCHEMA.md`.
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

&#9989; Regenerate the learning curve after the v1.1 boilerplate repair (`data-raw/learning-curve.csv`, README table), 2026-11-07. The shipped CSV no longer reproduced the pipeline at 100% — max per-fold difference 0.0064 (fold 5, `alpha` 0.5) — which is exactly what the script's reproduction assertion exists to catch. Re-ran it: max difference 4.4e-16, so the curve and `baselines/` now come from the same corpus. The table moves by at most 0.001 and its shape is unchanged (100% row 0.745 / 0.707 / 0.696; 50% → 100% z = 1.7, 2.6, 2.7).

&#9989; Download-snapshot series started (`R/downloadSnapshots.R`, `data-raw/snapshot-downloads.R`), 2026-11-07. One retrieval of Project Gutenberg's RDF dump produces an append-only `(gutenberg_id, snapshot_date, source, downloads)` table plus `date_added` and a provenance line (URL, UTC time, bytes, SHA-256, rows, total).
  * Trial snapshot: 79,762 books, 68,600,747 30-day downloads, median 564, payload sha256 `89a18c78…`; no missing counts and no duplicate ids.
  * Source is PG's own `rdf-files.tar.bz2` (121 MB, regenerated daily; 33 s unpack, 44 s parse), not Gutendex. The mirror agrees today, but a multi-year series should not depend on a third party's copy.
  * Fail-fast instead of empty results: the parser errors on any missing `pgterms:downloads` or `dcterms:issued`, the script refuses a feed under 70,000 records, and a second write for the same date is refused. 18 unit tests.
  * Publication year is available from the RDF for only ~17% of records (`marc260` 13,381, `marc906` 12,980 of 79,762), so `PREREGISTRATION.md` §12.2 still needs Wikidata or MARC.

&#9989; Label-quality audit (`data-raw/label-audit.R`), 2026-11-07: five tests on one author-disjoint split — artefacts, stability under resampling, counting-noise reliability, non-text predictability, independent-source agreement. The last two need data the repo does not have yet and skip with a stated reason rather than a hard-coded result.
  * Shipped 2013 label: bibliographic-only AUC 0.783, text model 0.745 (paired +0.038, t = 1.26, 3 of 5 folds), so the label is no more reducible to style than to genre, length and upload order.
  * Adding author productivity raises the bibliographic model to 0.816; it is reported separately because it would not generalise to a new writer.
  * Counting-noise curve replaces the earlier Poisson simulation over archived counts, which mixed a cumulative count with a 30-day window and was wrong in both directions.

&#9989; Add a target to compare a target run against the current baseline (`cv_baseline_check`, flagging any `alpha` more than two standard errors from `baselines/cv_summary.csv`).

&#9989; Move fold configurations out of the dataset but preserve the attribute of making them constant and repeatable.
  * Decided 2026-10-30: the original columns (`fold`, `download_count`, `response`) stay in `novels`, frozen and documented as Sunnybrook's. Anything new goes in separate tables.
  * Generated folds become stored data, not a seed: `(author, scheme, fold)`, with `author5_v1` = today's seed-42 assignment, **append-only** (existing authors never move; new authors are assigned and recorded). R's `set.seed()` cannot be reproduced in other languages, and the current positional assignment reshuffles folds whenever the data grows. The author key must handle co-authored books.
  * Fresh download counts go in their own table: `(gutenberg_id, snapshot_date, source, downloads)`. Answered 2026-11-07: the 2013 count was cumulative downloads as of the scrape (id 215 holds 3,433); Project Gutenberg now publishes only a trailing 30-day count (site totals: 12.9M last 7 days against 62.5M last 30 days, a ratio of 4.84, so the window rolls rather than resetting monthly). A refresh therefore measures current demand, not lifetime success.
  * **Selection rule, answered 2026-11-07.** The 2013 corpus is not a median split. Per genre the authors set a lower and an upper download threshold (paper Table 1: Adventure 17/100, Detective 25/90, Fiction 7/125, Historical Fiction 25/115, Love Stories 16/85, Poetry 9/70, Science Fiction 30/100, Short Stories 9/224), labelled books below the lower and above the upper, discarded the middle band, took 50 per class, and capped an author at 2 books per genre. Verified against the shipped data: 0 of 796 rows fall inside their genre's band, and max(FAILURE) equals the lower threshold in 6 of 8 genres. Reuse the rule rather than the median split: the median sits where the distribution is densest, so a cut there is decided by counting noise.
  * Order: folds table before expanding the dataset; counts refresh together with the expansion.
  * Done: `author5_v1` stored in `inst/extdata/author_folds.csv` (514 authors), verified against the pipeline's `fold_assignments`; `data-raw/author-folds.R` refuses to overwrite it. Next: make the pipeline read the table instead of recomputing the seed; documentation for appending new authors; export it with the Parquet dataset.

&#9989; Genre-calibration result filed (`data-raw/genre-calibration.R` → `data-raw/genre-calibration.csv`), 2026-11-07. The bookshelf→genre mechanism was scored against the 2013 labels on the 753 of 754 books that carry any bookshelf entry (19836, *The Verse-Book of a Homely Woman*, has an empty one). This is the item `PREREGISTRATION.md` §12.1 requires before the window opens.
  * Exact set agreement is 24.6% using every allow-list row and 41.7% after dropping judgment calls. Exact matching is the wrong headline — a book can carry several Stonybrook genres and several mapped shelves — so the per-genre numbers are the result.
  * Per genre, recall / precision: Poetry 0.89 / 1.00, Science Fiction 0.98 / 0.78, Historical Fiction 0.85 / 0.69, Detective/Mystery 0.72 / 0.79, Adventure 0.75 / 0.49, Short Stories 0.57 / 0.51, Fiction 0.82 / **0.17**, Romance 0.25 / 0.39.
  * Both failures are properties of Project Gutenberg's metadata, not of `assignGenre()`. The site-level "Novels" shelf is on 477 of 753 books (63.3%) and is the only source of Fiction, so mapped Fiction is a residual, not a category. Only 25 of 99 Romance-labelled books carry a Romance shelf, so Romance cannot be recovered from shelves. Dropping flagged rows erases Fiction entirely (recall 0) and leaves 169 books unassigned.
  * Usable as a mechanism output for the wave: Poetry, Science Fiction, Detective/Mystery, Historical Fiction. **Decision needed before 2026-12-31:** whether Fiction becomes the residual level rather than a genre, whether Romance stays in the eight, and whether the two weakest genres need a second source (Wikidata or Open Library subjects).

&#9989; Pre-register the 2027–2028 label wave (`PREREGISTRATION.md`), 2026-11-07: outcome, eligibility, instrumentation, label rule, folds, primary endpoint, power and audit gates fixed in a commit before any download series exists for the window.
  * **Wave, not intake-year cohort.** Decided 2026-11-07: books added in one year number ~430 across the eight genres (~54 per genre), which a 20th/80th trim would cut to ~11 per class — too few to label. The wave instead admits every eligible book as of 2026-12-31 and gives all of them the same 24-month window (2027-01-01 to 2028-12-31), so exposure is equal by construction without shrinking the pool. Books added during the window join the next wave.
  * `date_added` is the catalogue's `Issued` date (the ebook release date), not the original publication year; separating publication era from upload era needs Wikidata or MARC and is listed as an open dependency in §12.
  * Currency is a share of Project Gutenberg's total downloads in the same months, not a raw count, so waves stay comparable as the platform's scale changes.

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
