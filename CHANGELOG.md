# Dataset changelog

This tracks changes to the *dataset itself* — what a `tar_read(novels)` produces — separately from ordinary code changes to the pipeline. Each entry only changes one thing about what "success" means or what data backs it, so a version can be adopted independently of the others.

## v1.1 — source-integrity fix (this release)

Same titles, genres, fold structure, and success/failure labels as the original Ashok, Feng & Choi (2013) corpus. No relabeling. Text-source corrections only, plus two rows dropped as duplicates (below):

- Fixed a join bug that inflated 798 correct book observations to 884 by cross-multiplying genre-directory file copies against genre-specific metadata rows.
- Fixed 5 books whose `gutenberg_id` pointed at a LibriVox audio-recording catalog entry instead of the text edition. Three (`19513`, `19640`, `19836`) have their text replaced with a real edition from a different id; `19836`'s title is corrected, because *The Verse-Book of a Homely Woman* stands in for the unavailable collection *Early Spring*. The other two (`19678`, `19782`) are **dropped**, because the real books are already in the corpus as `215` and `157`. The dataset is now 796 rows over 754 books (was 798 over 756). The `download_count` and label of the three patched rows are still the audio entries' pending a refresh. An earlier version of this fix re-downloaded the contaminated ids and changed nothing; see `librivox_replacements` and `librivox_drops` in `R/targetFunctions.R`.
  *Effect on results:* mean test AUC rose by 0.002–0.015 at every `alpha` (mean +0.0085), e.g. 0.739 → 0.744 at `alpha = 0.01` and 0.686 → 0.695 at `alpha = 1`. No `alpha` moved by more than two standard errors, so this is within fold-to-fold noise and is not claimed as an improvement. Only 2 of 514 authors changed fold. Old and new baselines are in `baselines/`.
- Added `inst/extdata/author_folds.csv`: the author-grouped fold assignment (`author5_v1`: seed 42, five folds) stored as data, one row per author, so any language can reproduce the folds. It is the assignment the pipeline already used, so all baselines stay valid, and it is append-only (a positional seed reshuffles 75–80% of existing authors whenever the author count changes). `novels` itself is unchanged.
- Stripped Gutenberg license header/footer boilerplate from at least 22 books that shipped with it unstripped.
- Removed the dataset's static `fold` column from downstream use; CV fold assignments are now generated fresh, from a documented, seeded function, rather than trusted as opaque shipped state. (The original `fold` column is still present in parsed metadata for anyone who wants to reproduce the original paper's exact published per-genre CV numbers on the unmodified corpus.)

## v1.0 — original corpus, as parsed

The Ashok, Feng & Choi (2013) EMNLP corpus, faithfully parsed from its original hierarchical metadata format, with no text-source corrections applied. Known to contain the issues fixed in v1.1.

---

Planned, not yet released — see the README's Roadmap:

- A relabeled fork using updated (2026-era) Gutenberg download counts.
- A relabeled fork blending download counts with Goodreads ratings via a tunable weight.
- A larger, Gutenberg-derived corpus beyond the original ~800 books.

Each of these will be its own version when it lands, not bundled into v1.1.
