# novels

Reproducing and extending [*Success with Style: Using Writing Style to Predict the Success of Novels*](https://aclanthology.org/D13-1181.pdf) (Ashok, Feng & Choi, EMNLP 2013) — a style-based classifier that predicts whether a novel will be more or less "successful," measured via Project Gutenberg download counts.

This project builds the same dataset from the original paper's source metadata, then trains and evaluates a TF-IDF / elastic-net classifier on it as a modern R reproduction. The upstream corpus has several real data-quality issues (see below); this pipeline fixes them, with each fix backed by an assert that catches the problem if it ever reoccurs. The Results table below is read back from the `targets` store (`tar_read(cv_summary)`), not copied by hand, and the source archive is pinned by SHA-256, so the numbers and the bytes they came from cannot drift apart silently.

## Pipeline

Built with [`targets`](https://books.ropensci.org/targets/). To run it end to end:

```r
library(targets)
tar_make()
```

Configuration lives in `.env` — copy `.env.example` and fill it in. Only the
archive's location goes there (so it can move, e.g. from Backblaze to
HuggingFace, without a code change). The archive's SHA-256 is pinned as
`archive_sha256` in `_targets.R` and checked before unpacking, so a moved
mirror or a corrupted download aborts instead of silently changing the corpus.

Key targets (213 in total):

| Target | What it is |
|---|---|
| `archive_url`, `archive_sha256` | Where the archive lives (from `.env`) and the digest it must have — both are targets, so both appear in the dependency graph |
| `rawdataset_path` | Downloads, verifies and unpacks the source corpus — the archive is left untouched; all corrections below are applied as explicit pipeline code, not baked into the archive |
| `parsed_metadata` | Parses the original hierarchical metadata file into a flat tibble (genre, fold, success/failure label, Gutenberg ID, download count), with a file-conservation assert against the directory tree |
| `novels` | One row per (book, genre): deduplicates the file listing to one row per book, patches known LibriVox-contaminated text, strips Gutenberg boilerplate, joins against the metadata — 798 rows over 756 books |
| `fold_assignments` | Five author-safe folds, generated once and indexed into by every per-fold target |
| `train_data_*` / `test_data_*` | Per-fold splits, deduplicated per book within the fold |
| `vectorizer_*`, `dtm_{train,test}_raw_*` | Unigram vocabulary and document-term matrices |
| `tfidf_fit_*`, `dtm_{train,test}_*` | TF-IDF weighting, fit on train only |
| `cv_foldid_*` | This fold's own inner-CV fold assignment, author-grouped and shared across every `alpha` in that fold |
| `model_fit_*`, `eval_train_*`, `eval_test_*` | Elastic-net fit and AUC for each `alpha` (0.01–1.0) in each fold |
| `cv_results`, `cv_summary` | The 50 per-(fold, `alpha`) AUCs, and their mean/SD/min/max per `alpha` |

`tar_visnetwork()` gives a full dependency graph; `tar_manifest()` lists every target and its command.

**Grain of the corpus.** Several counts are easy to confuse, so they are stated once here:

| Grain | Count | Where it lives |
|---|---|---|
| files | 800 | directory tree |
| books (distinct Gutenberg IDs) | 758 | directory tree |
| metadata blocks | 799 | `novel_meta.txt` |
| parseable records | 798 | `parsed_metadata` |
| rows in `novels` | 798 | book × genre |
| distinct books in `novels` | 756 | `novels$gutenberg_id` |
| books with more than one genre row | 41 (42 extra rows; at most 3 placements each) | `novels` |

The difference between 758 books on disk and 756 in `novels` is exactly two books, 23177 and 23178, whose text exists but whose metadata records are the damaged ones described below.

## The source data has known issues

The original metadata file (a plain-text, human-readable format with a hierarchical `DATASET` / `FOLD` structure) and the underlying corpus text both had real problems. **Fixed, with a regression assert guarding each one:**

- **The last two metadata lines are incomplete.** The first is truncated mid-record in the source file — `parseMetaData()` detects and drops any record missing a `DownloadCount` field rather than silently keeping a partial one. The second metadata line is missing altogether. The corresponding data, the text file 23178.txt, exists in a subdirectory of the dataset.
- **Co-authored books produce spurious duplicate rows** when joined against `gutenbergr::gutenberg_metadata`, which has one row per (book, author) pair. `novel_metadata` is deduplicated on `(gutenberg_id, genre, fold)` to collapse these correctly, without collapsing the intentional cross-genre repeats (see next item).
- **`list.files()` finds one text-file copy per genre-directory a book belongs to.** Joining that against `novel_metadata` (already one row per genre) on `gutenberg_id` alone cross-multiplied mismatched genre/fold combinations (798 correct rows inflated to 884). Fixed by deduplicating the file listing to one row per book before the join, with a row-count assert guarding it.
- **5 books had their `gutenberg_id` pointing at a LibriVox audio recording's catalog entry instead of the text edition** — confirmed by a full-corpus grep for the literal string "librivox" (which cannot appear in genuine prose predating LibriVox's 2005 founding), and independently corroborated by [Maharjan et al. (2017)](https://aclanthology.org/E17-1114.pdf), who found the same 5-book count in this corpus. Text for these is refetched directly from Gutenberg via `gutenbergr::gutenberg_download()`. See `known_contamination.csv` for the full record.
- **Gutenberg license header/footer boilerplate is removed, but the matcher is not exhaustive.** [Maharjan et al. (2017)](https://aclanthology.org/E17-1114.pdf) hand-reviewed what appears to be this same 800-book corpus and report 58 affected books — 53 carrying the license repeated verbatim plus five more — and their five match this corpus's five LibriVox-contaminated books exactly. This pipeline's automated matcher flags 22. The gap measures the matcher's recall, not the corpus: it recognises only the standard `*** START/END OF THIS PROJECT GUTENBERG EBOOK ***` markers, so variants and truncated markers pass through. Stripping is applied uniformly to every book via a hand-rolled, case-insensitive matcher (see the next point for why hand-rolled).
- **This corpus's text was already lowercased during the original corpus prep**, discovered while debugging the boilerplate fix (`gutenbergr::gutenberg_strip()`, which expects Gutenberg's standard uppercase markers, would have silently matched nothing against this already-lowercased text). Worth knowing if you do anything downstream that depends on capitalization — sentence-boundary detection, proper-noun/NER work, etc. will not behave as expected on this text as-is.

**Still open:**

- **Genre membership is intentionally many-to-many.** The same book can legitimately appear under multiple genres, each with its own independent fold structure — this is the corpus's design (per-genre balanced classification), not a duplication bug. Don't naively deduplicate on Gutenberg ID alone.
- **The evaluation is author-disjoint by choice, and that is a constraint on the number, not a property of the task.** 152 of 514 authors have more than one book (394 books, 52% of the corpus), and for those books the *other* books' labels alone predict this one with AUC 0.95 (leave-one-out mean of the author's remaining labels). Training and scoring on the same authors would therefore inflate AUC mostly by recognising author popularity rather than style, so folds keep every author on one side. The cost is that the 11 authors with genuinely mixed outcomes cannot teach the model "this author is inconsistent" — too thin a signal, at 11 authors, to learn from anyway. A title-grouped split would measure the author-overlap estimand instead; it is listed in the Roadmap as an optional comparison, not a correction. This choice biases the reported AUC downward, never upward.
- **`fold` is defined per `(book, genre)` pair, not per book.** A single book can carry different fold assignments across its different genre memberships. This pipeline currently trains one pooled classifier across all genres and uses independently generated, author-grouped folds (`fold_assignments`, with a per-fold inner-CV `cv_foldid_*` shared across that fold's whole `alpha` sweep) rather than the dataset's own static fold column. That's a deliberate choice, not an oversight: a static fold column shouldn't be propagated into forked/corrected datasets at all — it couples data to one specific modeling methodology and offers no guarantee of remaining valid once the underlying data changes (see Roadmap for the principled alternative, and the leakage measurement below).

## Results

TF-IDF (unigrams) + `glmnet` (elastic net), five author-safe folds: the model is fit on four folds and scored on the fifth. This table is `tar_read(cv_summary)` from the store in this repository — not a hand-copied number:

| alpha | mean test AUC | SD across folds | range | mean nonzero coefficients |
|---|---|---|---|---|
| 0.01 | 0.739 | 0.075 | 0.643–0.853 | 4294 |
| 0.03 | 0.732 | 0.060 | 0.655–0.823 | 2601 |
| 0.05 | 0.738 | 0.052 | 0.666–0.802 | 1916 |
| 0.07 | 0.737 | 0.045 | 0.675–0.793 | 1732 |
| 0.10 | 0.726 | 0.038 | 0.680–0.785 | 1536 |
| 0.30 | 0.710 | 0.032 | 0.666–0.756 | 764 |
| 0.50 | 0.694 | 0.020 | 0.659–0.708 | 426 |
| 0.70 | 0.689 | 0.021 | 0.653–0.707 | 268 |
| 0.90 | 0.688 | 0.023 | 0.649–0.708 | 252 |
| 1.00 | 0.686 | 0.022 | 0.649–0.704 | 243 |

Read plainly: the fold-to-fold spread (SD 0.02–0.075) is as large as any difference among the low alphas, so `alpha` ≤ 0.1 is indistinguishable at this corpus size. The one clear effect is the decline at high `alpha`, where lasso-leaning models give up about five AUC points (0.739 → 0.686). Naming a single "best" `alpha` from this table would be selecting noise, which is exactly what the repeated folds exist to prevent. The 50 individual per-fold AUCs are in `cv_results`.

An earlier single-split, trigram run peaked at 0.820 (`alpha = 0.03`). It is not comparable to this table — different features, and one split rather than five — and survives only in git history.

**In-sample train AUC is ~1.00 at every `alpha`** (0.990–1.000), against 0.69–0.74 on held-out folds — a gap of 0.26–0.31 that barely moves across the sweep. With 243–4,294 nonzero coefficients over roughly 600 training documents per fold, the model has enough capacity to memorise its training set at any regularisation strength here, so the gap does not discriminate between alphas; held-out AUC is the only informative comparison. Both sides of the gap come from the same fit (`eval_train_*` and `eval_test_*` share each `model_fit_*`), so they cannot drift apart.

## Roadmap

- **Use the dataset's native fold structure for cross-validation, per genre, rather than the current generated `fold_assignments`/`cv_foldid_*`.** Requires switching to per-genre classifiers (fold is only unambiguous within a single genre). Would make results directly comparable to the original paper's published per-genre numbers. The native fold column cannot simply be pooled instead: **36 of the 41 multi-genre books sit in different native folds for different genres**, so a pooled model using that column would train and test on identical text. The generated author-grouped folds do not have this defect — measured against the current `fold_assignments`, **0 of 756 books appear in more than one test fold**.
- A `genre = "all"` track for the pooled/cross-genre model, treated as a faux genre subject to the same integrity asserts as real genres (rather than a special case), with its own independently-generated fold assignment.
- **Optional comparison: a title-grouped split alongside the author-grouped one.** Same books, but only books are kept out of each other's folds, so an author may appear on both sides — the estimand becomes "another book by a known author". The gap between the two AUCs is the value of author track record. This is a comparison, not a correction: the author-grouped number is the one that supports a claim about style.
- Add an embedding-based feature set (e.g. `nomic-embed-text` via Ollama) as an alternative to TF-IDF, and re-run the regularization sweep against it — the TF-IDF result above shouldn't be assumed to transfer to a much lower-dimensional, non-sparse feature space. Note that general-purpose semantic embeddings are topic-dominated by construction and may not isolate *style* the way this project's stylometric aims would want — style-specific embedding methods (e.g. Wegmann et al.'s style embeddings, LUAR) may be a better fit for that specific goal than general embeddings.
- A versioned, forkable release of this corrected dataset (distinct from any label-redefinition forks, like updated download counts or a Goodreads-blended success label) hosted somewhere with real permanence guarantees — Zenodo (DOI-backed) or the Hugging Face Datasets Hub, not just this repo alone.
- Document the shipped dataset per [R packages: data documentation](https://r-pkgs.org/data.html).
- A hybrid continous variable that amalgates Gutenberg download counts with other measures of success, including particularly Gutenberg download counts but also considering New York Times top 100 lists, Nobel and Pulitzer Prize winners, books that made it into movies, and stylistic similarities to revealed word, e.g. the Old and New Testament. As a feature the proportion of these signals may get a "knob" that is tuned by a person or an algorithm.
- **Challenge the case-sample model and the 50/50-per-genre binomial label; model the download count instead.** The label is a per-genre median split of a count ranging 0–11,765 (median 29, skew 7.3): failures span 0–30 downloads, successes 70–11,765 with a mean of 596. The clean gap between 30 and 70 says the corpus was constructed into two bands rather than sampled from Gutenberg, so the binary label faithfully encodes *which band* and discards all ordering inside the success band — which is exactly where "a very successful book versus a modest one" lives. Two consequences: the threshold is relative to each genre's sample (all eight are 0.50/0.50), and the sample is case-control, so regression predictions would need weights, or a natural sample, before they meant anything about Gutenberg as a whole. Next step: regress `log1p(download_count)` with the existing TF-IDF features and folds, scored by Spearman and by AUC after thresholding at the same cut, so the binomial number stays comparable. Poisson, or a two-part band + magnitude model, is probably the honest structure given the gap and the 11,765 outlier.
- Convert the target dataset format from R's RDA format to Parquet so that the dataset is language neutral. 
- Explore other conventional narrative attributes and habits of "literature" including:
  - Literary allusion, think of Joyce's religious allusions, his apple tree in a building courtyard and Buck Mulligan's razor and razor bowl.
  - Chekhov's gun better expressed as a juggler with knives in the air. How many does he have up in the air at one time, which one is coming down next, and what or why is the juggler doing tossing up another one in the air? This is the basis of suspense and author's credibility. Each time a gun goes off or a knife returns to earth, the write fulfills a promise to his reader.
  - Homage. Is it a useful signal when a work deliberately mirrors another work as in the example of Virginia Woolf's Mrs. Dalloway and Michael Cunningham's The Hours, Joyce's Ulysses and the Odyssey, Ring Lardner's short story, The Champion, and the rags-to-riches stories of Horatio Alger?

## Related work

- Ashok, V. G., Feng, S., & Choi, Y. (2013). [*Success with Style: Using Writing Style to Predict the Success of Novels*](https://aclanthology.org/D13-1181.pdf). EMNLP.
- Maharjan, S. et al. (2017). [*A Multi-task Approach to Predict Likability of Books*](https://aclanthology.org/E17-1114.pdf). EACL.
- Maharjan, S. et al. (2018). *Letting Emotions Flow: Success Prediction by Modeling the Flow of Emotions in Books*. NAACL. ([code](https://github.com/sjmaharjan/emotion_flow))
- Gorelick, H. et al. (2021). *Predicting the Success of Literary Fiction*. RANLP.
