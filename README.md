# novels

Reproducing and extending [*Success with Style: Using Writing Style to Predict the Success of Novels*](https://aclanthology.org/D13-1181.pdf) (Ashok, Feng & Choi, EMNLP 2013) — a style-based classifier that predicts whether a novel will be more or less "successful," measured via Project Gutenberg download counts.

This project builds the same dataset from the original paper's source metadata, then trains and evaluates a TF-IDF / elastic-net classifier on it as a modern R reproduction. The upstream corpus has several real data-quality issues (see below); this pipeline fixes them, with each fix backed by an assert that catches the problem if it ever reoccurs. Every number in the Results section below was produced after all current fixes were applied.

## Pipeline

Built with [`targets`](https://books.ropensci.org/targets/). To run it end to end:

```r
library(targets)
tar_make()
```

Key targets:

| Target | What it is |
|---|---|
| `rawdataset_path` | Downloads and unpacks the source corpus (metadata + book text) — this archive is left untouched; all corrections below are applied as explicit pipeline code, not baked into the archive |
| `novel_metadata` | Parses the original hierarchical metadata file into a flat tibble (genre, fold, success/failure label, Gutenberg ID, download count), joined against `gutenbergr::gutenberg_metadata`, deduplicated on `(gutenberg_id, genre, fold)` |
| `novels` | Deduplicates one row per book from the raw file listing, patches known LibriVox-contaminated text, strips Gutenberg license boilerplate, joins against `novel_metadata` — 798 rows |
| `split_indices` / `train_data` / `test_data` | Author-level train/test split — no author's books appear in both sets |
| `cv_foldid` | A single, fixed inner-CV fold assignment shared across every `alpha` value, so alphas are compared on identical partitions |
| `vectorizer`, `dtm_train_raw`, `dtm_test_raw` | Vocabulary and document-term matrices via [`text2vec`](https://text2vec.org) |
| `tfidf_fit`, `dtm_train`, `dtm_test` | TF-IDF weighting, fit on train only |
| `model_fit_*`, `eval_train_*`, `eval_test_*` | A `glmnet` elastic-net classifier, branched across a range of `alpha` values (0.01–1.0) |
| `alpha_sweep_summary` | Test-set AUC and nonzero-coefficient count at each `alpha` |

`tar_visnetwork()` gives a full dependency graph; `tar_manifest()` lists every target and its command.

## The source data has known issues

The original metadata file (a plain-text, human-readable format with a hierarchical `DATASET` / `FOLD` structure) and the underlying corpus text both had real problems. **Fixed, with a regression assert guarding each one:**

- **The final metadata line is truncated mid-record** in the source file — `parseMetaData()` detects and drops any record missing a `DownloadCount` field rather than silently keeping a partial one.
- **Co-authored books produce spurious duplicate rows** when joined against `gutenbergr::gutenberg_metadata`, which has one row per (book, author) pair. `novel_metadata` is deduplicated on `(gutenberg_id, genre, fold)` to collapse these correctly, without collapsing the intentional cross-genre repeats (see next item).
- **`list.files()` finds one text-file copy per genre-directory a book belongs to.** Joining that against `novel_metadata` (already one row per genre) on `gutenberg_id` alone cross-multiplied mismatched genre/fold combinations (798 correct rows inflated to 884). Fixed by deduplicating the file listing to one row per book before the join, with a row-count assert guarding it.
- **5 books had their `gutenberg_id` pointing at a LibriVox audio recording's catalog entry instead of the text edition** — confirmed by a full-corpus grep for the literal string "librivox" (which cannot appear in genuine prose predating LibriVox's 2005 founding), and independently corroborated by [Maharjan et al. (2017)](https://aclanthology.org/E17-1114.pdf), who found the same 5-book count in this corpus. Text for these is refetched directly from Gutenberg via `gutenbergr::gutenberg_download()`. See `known_contamination.csv` for the full record.
- **At least 22 books had unstripped Gutenberg license header/footer boilerplate** — more widespread than [Maharjan et al. (2017)](https://aclanthology.org/E17-1114.pdf)'s independent finding of 53 affected books in a different, larger derivative suggested for this smaller corpus. Stripped uniformly across all books via a hand-rolled, case-insensitive matcher (see next point for why hand-rolled).
- **This corpus's text was already lowercased during the original corpus prep**, discovered while debugging the boilerplate fix (`gutenbergr::gutenberg_strip()`, which expects Gutenberg's standard uppercase markers, would have silently matched nothing against this already-lowercased text). Worth knowing if you do anything downstream that depends on capitalization — sentence-boundary detection, proper-noun/NER work, etc. will not behave as expected on this text as-is.

**Still open:**

- **Genre membership is intentionally many-to-many.** The same book can legitimately appear under multiple genres, each with its own independent fold structure — this is the corpus's design (per-genre balanced classification), not a duplication bug. Don't naively deduplicate on Gutenberg ID alone.
- **`fold` is defined per `(book, genre)` pair, not per book.** A single book can carry different fold assignments across its different genre memberships. This pipeline currently trains one pooled classifier across all genres and uses a fixed, independently-generated fold assignment (`cv_foldid`, shared across the whole `alpha` sweep) rather than the dataset's own static fold column. That's a deliberate choice, not an oversight: a static fold column shouldn't be propagated into forked/corrected datasets at all — it couples data to one specific modeling methodology and offers no guarantee of remaining valid once the underlying data changes (see Roadmap for the principled alternative).

## Results

TF-IDF + `glmnet` (elastic net), swept across `alpha`, on the author-level held-out test set — after all fixes above (correct row count, shared CV partition, contamination removed):

| alpha | test AUC | nonzero coefficients |
|---|---|---|
| 0.01 | 0.813 | 19685 |
| 0.03 | 0.820 | 5389 |
| 0.05 | 0.818 | 3216 |
| 0.07 | 0.809 | 2513 |
| 0.1 | 0.784 | 1348 |
| 0.3 | 0.733 | 333 |
| 0.5 | 0.713 | 248 |
| 0.7 | 0.672 | 123 |
| 0.9 | 0.663 | 106 |
| 1.0 | 0.661 | 104 |

Test AUC peaks at `alpha = 0.03` (0.820) — this location has now held steady across three independent rounds of correction (the join fix, the shared-CV-fold fix, and this contamination fix), which is reasonable evidence it reflects something real about the task rather than an artifact of any one bug.

The contamination fix changed the sweep's shape unevenly: the low-`alpha` end (0.01–0.1) barely moved, while the high-`alpha` end (0.5–1.0) improved substantially (e.g. `alpha = 0.5` rose from 0.665 to 0.713). A plausible mechanism: contaminated text (boilerplate, audiobook descriptions repeated near-verbatim across documents) creates highly correlated spurious features, which lasso-leaning regularization handles poorly — it tends to arbitrarily pick one of several near-identical noise terms and zero out the rest, in a way that can distort which real terms survive too. Ridge-leaning regularization is far more tolerant of that same noise, which may explain why the low-`alpha` end had less room to improve once the noise was actually removed.

## Roadmap

- Train/test AUC gap at each `alpha` — not yet checked on the corrected data. Particularly worth checking whether `alpha = 0.01`'s near-full-vocabulary model (19,685 nonzero coefficients) is genuinely generalizing as well as its test AUC suggests, or overfitting despite scoring well.
- **Use the dataset's native fold structure for cross-validation, per genre, rather than the current fixed-but-arbitrary `cv_foldid`.** Requires switching to per-genre classifiers (fold is only unambiguous within a single genre) and regenerating fold assignments via a documented, seeded function with asserted invariants (author non-overlap, etc.) rather than shipping the original static fold column — see the "still open" note above for why. Would make results directly comparable to the original paper's published per-genre numbers.
- A `genre = "all"` track for the pooled/cross-genre model, treated as a faux genre subject to the same integrity asserts as real genres (rather than a special case), with its own independently-generated fold assignment.
- Add an embedding-based feature set (e.g. `nomic-embed-text` via Ollama) as an alternative to TF-IDF, and re-run the regularization sweep against it — the TF-IDF result above shouldn't be assumed to transfer to a much lower-dimensional, non-sparse feature space. Note that general-purpose semantic embeddings are topic-dominated by construction and may not isolate *style* the way this project's stylometric aims would want — style-specific embedding methods (e.g. Wegmann et al.'s style embeddings, LUAR) may be a better fit for that specific goal than general embeddings.
- A versioned, forkable release of this corrected dataset (distinct from any label-redefinition forks, like updated download counts or a Goodreads-blended success label) hosted somewhere with real permanence guarantees — Zenodo (DOI-backed) or the Hugging Face Datasets Hub, not just this repo alone.
- Document the shipped dataset per [R packages: data documentation](https://r-pkgs.org/data.html).

## Related work

- Ashok, V. G., Feng, S., & Choi, Y. (2013). [*Success with Style: Using Writing Style to Predict the Success of Novels*](https://aclanthology.org/D13-1181.pdf). EMNLP.
- Maharjan, S. et al. (2017). [*A Multi-task Approach to Predict Likability of Books*](https://aclanthology.org/E17-1114.pdf). EACL.
- Maharjan, S. et al. (2018). *Letting Emotions Flow: Success Prediction by Modeling the Flow of Emotions in Books*. NAACL. ([code](https://github.com/sjmaharjan/emotion_flow))
- Gorelick, H. et al. (2021). *Predicting the Success of Literary Fiction*. RANLP.
