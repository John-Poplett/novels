# Pre-registration: label wave 2027–2028

**Status.** Written 2026-11-07, before any download series exists for the
window it describes. The commit that adds this file is the registration. Any
change made after the window opens is an amendment under section 11, dated and
published, and the analysis is reported both ways.

The document is deliberately literal. "Wave" means a labelling run over a fixed
calendar window; "added" means the date Project Gutenberg issued the ebook. No
term here is a metaphor for something else.

---

## 1. The claim under test

The project's question is whether writing style predicts the success of a novel.
Two claims are separated, because only the second one is interesting:

- **H1 (learnability).** A text model predicts the label better than chance
  within the wave.
- **H2 (style specifically).** The text model predicts the label better than a
  bibliographic baseline that never reads the book.

H1 can be true for a label that is mostly a proxy for era, length and genre.
H2 is the claim the project exists to test, and it is the primary endpoint.
If H2 fails, the honest publication is a dataset and a negative result, not a
style claim.

## 2. Outcome definition

**Success** is the downloads a book accumulates during the wave window,
divided by Project Gutenberg's total downloads in the same months.

- The window is **2027-01-01 to 2028-12-31**, 24 months, identical for every
  book in the wave.
- Per-month counts are obtained from the trailing 30-day counter by taking one
  snapshot every 30 days, so consecutive snapshots are disjoint windows and the
  sum is the window total without deconvolution.
- The site total is read from the same source on the same days. The divisor is
  per million downloads, so the outcome is "share of platform attention", not a
  raw count.
- This is **not** lifetime success. It is attention during two named years. The
  documentation must say so wherever the label is used.

The divisor exists because platform scale changes; without it, waves are not
comparable and the 2027–2028 window would be a measurement of Gutenberg's
traffic growth rather than of books.

## 3. Population and eligibility

A book is eligible for the wave if **all** of the following hold at
2026-12-31:

1. It is in the Project Gutenberg catalogue as `Type == "Text"` and
   `Language == "en"`.
2. `date_added` (the catalogue's `Issued` date) is on or before 2026-12-31, so
   that at least one month of its life on the platform precedes the window.
3. It is assigned to at least one of the eight content genres by the versioned
   allow-list (`inst/extdata/genre_allowlist.csv`, or its successor, pinned by
   hash at freeze).
4. It has extractable text.

Books added during the window do not join it. They join the next wave. This is
the cost of equal exposure and it is accepted.

**Why the wave is not an intake year.** Books added in 2027 number roughly 430
in the eight genres (5,581 genre-subject English texts arrived 2014–2026, about
430 a year). Split across eight genres that is ~54 per genre; a 20th/80th
percentile trim would leave ~11 per class, too few to label. The wave admits
every eligible book, which keeps exposure equal without shrinking the pool.

## 4. Instrumentation

- One snapshot per month of the **entire** catalogue, on the same day of the
  month, at a fixed UTC hour.
- Stored append-only as `(gutenberg_id, snapshot_date, source, downloads)`.
  Nothing is ever overwritten.
- Provenance per snapshot: source URL, retrieval timestamp, SHA-256 of the
  payload, row count.
- Missing months are recorded as missing and never interpolated. A gap of more
  than one month in the window invalidates the wave (section 11).
- The counter's definition is recorded with the data: unique address-days,
  deduplicated per address per day, addresses exceeding 100 ebooks in a day
  excluded as robots.
- **Backfill is forbidden.** A series that starts late does not get a
  reconstructed prefix.

## 5. Label construction

Fixed order of operations. No step may be reordered after the window opens.

1. Sum the 24 monthly counts per book in the window.
2. Divide by the site total for the same months; multiply by 1e6. This is
   `share_per_million`.
3. Within each genre, compute the 20th and 80th percentiles of
   `share_per_million` across eligible books.
4. **Noise separation.** The band must satisfy
   `c_hi - c_lo >= 4 * sqrt(c_lo)` in downloads, where `c_lo` and `c_hi` are
   the cuts expressed as counts. If it does not, widen the band symmetrically
   (10th/90th, then 5th/95th) until it does, and report which percentile was
   used. This is a property of the counts, not of the outcome.
5. **Class size.** Each genre must yield at least 50 books per class. If it
   does not at the chosen percentile, merge it with its declared partner
   (Adventure + Historical Fiction; Detective/Mystery + Fiction; Romance +
   Poetry; Science Fiction + Short Stories) and report the merge. A genre that
   still fails is dropped from the wave and reported.
6. **Author cap.** At most 2 books per author per genre, selected with
   `set.seed(42)` over a sorted author list. This mirrors the 2013 corpus,
   where the cap was 2 per genre.
7. Books in the band are **unlabelled**, not assigned to either class.

Percentiles are used rather than absolute thresholds because the scale of the
counter is not stable across years; the 2013 thresholds (7–224 downloads) are
meaningless against today's counts.

The band is deliberately wide. A book inside it is one whose class the counter
cannot settle, and leaving it unlabelled converts measurement noise into lost
coverage instead of a wrong label. The 2013 corpus's own audit measured the
cost: 84 books sit within ±2 standard errors of a median cut, against 11 near a
90th-percentile cut.

## 5.5 Form and audience attributes

Two things that could be called genres are recorded as **attributes** instead.
Neither enters the label; either can be added, corrected or ignored without
touching the outcome definition. They are supplementary columns in the released
data and support pre-declared robustness analyses, nothing more.

**`form` — length class.** One value per book, from a word count of the same
normalised text the features are built from: `flash` (≤ 1,000 words),
`short story` (1,001–15,000), `novella` (15,001–50,000), `novel` (> 50,000).
Measured on the 2013 corpus (`stringi::stri_count_words()`): 5 flash, 202
short story, 138 novella, 409 novel. Flash fiction is not a genre because there
is no pool: 5 of 754 books, 0 under 500 words, and the median corpus book is
56,972 words. More importantly, length is already a predictor — one length
feature reaches AUC 0.632 on the 2013 label. Making form a genre would put a
predictor on the label axis and make the bibliographic baseline partly
circular, which would hollow out the one comparison (H2) this project exists
to make.

**`children_or_ya_shelf` — audience attribute.** A boolean, true when the
catalogue's bookshelf list carries `Category: Children & Young Adult Reading`.
Project Gutenberg has no Young Adult-only shelf: the only related subject
heading, "young adult", appears on 5 of 61,930 English texts, while this
combined shelf appears on 6,346 of them. The shelf therefore cannot separate
young-adult books from children's books, and it cross-cuts content genre — in
the 2013 corpus 48 books carry it across seven of the eight genres (Adventure
19, Detective/Mystery 13, Short Stories 11, Fiction 2, one each in three
others). A ninth genre would double-count those books and break the 50/50
balance per genre. As an attribute it answers a robustness question instead:
does the text model still beat the bibliographic baseline inside the children
and young-adult subset?

The attribute is deliberately not called `is_ya`. The name says what is
measured: a combined children-and-young-adult shelf, not a young-adult one.

Neither attribute is part of eligibility (section 3), the label rule (section
5) or the primary endpoint (section 7).

## 6. Folds and models

- **Folds**: 5 author-grouped folds from `inst/extdata/author_folds.csv`, or a
  successor table with the same property, pinned by hash at freeze. The
  frozen `fold` column in `novels` is not author-disjoint (85 of 514 authors
  span folds) and must not be used.
- **Text model**: TF-IDF (unigrams, as in the existing pipeline) with
  `glmnet`, `family = "binomial"`, `type.measure = "auc"`, inner `foldid`
  grouped by author. Alpha selected by inner cross-validation, not by the outer
  test folds; the selected alpha is reported.
- **Bibliographic baseline**: logistic regression on
  `genre + log10(text length) + log10(date_added as an ordinal) + publication
  year`, if publication year is available at freeze. If it is not, the baseline
  is `genre + log10(text length) + log10(date_added)` and the omission is
  reported.
- **Both models** are fitted on the same rows and the same folds.

## 7. Primary analysis and power

**Endpoint.** Paired difference in held-out AUC, text minus bibliographic,
computed once per fold and averaged over the 5 folds. Positive means the text
model is better.

**Inference.** A 95% bootstrap CI over books, resampling whole authors, not
rows, so the CI respects the fold structure. The paired t across 5 folds is
reported as a secondary check.

**Minimum detectable effect.** From the 2013 corpus, the paired fold-level SD
was 0.068 at ~150 test books per fold. With the wave's expected ~2,000 books
(~400 per fold), the SD scales as the square root of test size: about 0.042.
At 5 folds, one-sided α = 0.05 and power 0.8, the detectable difference is
`(2.132 + 0.842) * 0.042 / sqrt(5) ≈ 0.056`.

**Declared minimum effect of interest: 0.05 AUC.** The design cannot detect
anything smaller, and this limitation is to be stated in any publication
rather than resolved by adding folds after the fact.

## 8. Decision rules

- **H2 is supported** only if the bootstrap CI for the paired difference lies
  entirely above 0 *and* the point estimate is at least 0.05.
- **H2 is rejected** if the CI includes 0 or the estimate is below 0.05.
- **Inconclusive** is a permitted and reportable outcome.
- A result on H1 with H2 rejected is reported as "the label is learnable from
  the bibliographic record", which is a statement about the label.

## 9. Audit gates

The five tests in `data-raw/label-audit.R` are run on the constructed label
before any modelling. A wave that fails a gate is reported, not silently fixed.

| gate | rule |
|---|---|
| artefacts | report zero-download books, count under 5 and under 30, top-decile share, duplicate titles. No book with 0 downloads may enter the labelled set. |
| stability | bootstrap over books, rule re-derived per resample: flip rate ≤ 2%. |
| reliability | empirical month-to-month dispersion over the first 12 snapshots; if it exceeds Poisson, the band is widened under section 5 step 4 *before* labelling, and the change is recorded in the snapshot log. |
| predictability | the bibliographic baseline AUC is reported whether or not H2 is supported. |
| agreement | not available for this wave. No second source has sufficient coverage (Wikidata P2034 reaches 15.4% of the 2013 corpus; Open Library ratings reach 4.4% with at least five ratings). Reported as absent, not as passed. |

## 10. Multiplicity

One primary endpoint (section 7). Every additional comparison — sensitivity
windows, per-genre models, the author-productivity model — is labelled
exploratory and reported without significance claims. The five audit gates are
not hypotheses and are not corrected.

## 11. Deviations

- Amendments are dated, appended to this file, and published with the analysis.
- An amendment that changes the outcome definition, the percentile rule, the
  folds or the endpoint means the wave is reported **both** as registered and
  as amended.
- A snapshot gap longer than one month inside the window invalidates the wave;
  the next eligible window is 2030–2031.
- If the counter's definition changes at source (for example, the 30-day window
  is replaced by a cumulative count), the wave is suspended and the registration
  is revised before restarting. A changed counter is a different instrument.

## 12. Not covered by this registration

These are real dependencies, and they must be resolved and recorded **before
the window opens**. They are listed so that their absence cannot be discovered
after the fact:

1. **Genre assignment for books outside the 2013 corpus.** The allow-list
   exists; its calibration against the 754 known books must be filed before
   the window opens.
   *Filed 2026-11-07, before the freeze:* `data-raw/genre-calibration.R` scores
   it on 753 books (19836 has no bookshelf entry). Per-genre recall/precision:
   Poetry 0.89/1.00, Science Fiction 0.98/0.78, Historical Fiction 0.85/0.69,
   Detective/Mystery 0.72/0.79, Adventure 0.75/0.49, Short Stories 0.57/0.51,
   Fiction 0.82/0.17, Romance 0.25/0.39. The "Novels" shelf sits on 63.3% of
   books and is the only source of Fiction; only 25.3% of Romance books carry a
   Romance shelf. **Open decision due before 2026-12-31:** Fiction as residual
   rather than level, and Romance's place in the eight. See SCHEMA.md.
2. **Publication year.** The catalogue carries the ebook date, not the original
   publication date. Without a second source (Wikidata or MARC), the baseline
   cannot separate publication era from upload era, and that limitation is
   reported. Measured 2026-11-07: the RDF's own imprint fields cover only about
   17% of records (`pgterms:marc260` 13,381, `pgterms:marc906` 12,980 of
   79,762), so the omission stands unless that source is added before the
   freeze.
3. **Withdrawals.** Books leave the catalogue. A `withdrawn_on` column is
   required; withdrawn books are reported, not silently dropped.

Form and audience attributes are defined in section 5.5. They are not listed
here because they are not dependencies: they change no label, and a wave can
run with or without them.

## 13. Freeze and publication

| date | event |
|---|---|
| 2026-11-30 | instrumentation running; first snapshot taken (trial, not part of any window) |
| 2026-12-31 | registration frozen; eligibility fixed from the catalogue of that date |
| 2027-01-01 | window opens |
| 2028-12-31 | window closes |
| 2029-01-31 | last snapshot for the window retrieved; label constructed |
| 2029-03-31 | audit run; models fitted; results published with this document |

The first snapshot was taken **2026-11-07** (trial), ahead of the date below and
not part of any window; the series then runs monthly. It is stored as
`data-raw/snapshots/2026-11-07/`.

The 12-month window (2027 only) is reported as a pre-declared sensitivity
analysis alongside the primary 24-month result. It is not a fallback: the
primary result stands or falls on 24 months.
