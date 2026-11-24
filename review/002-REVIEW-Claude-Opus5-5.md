# Architectural review, round 2

2026-11-16. Responds to `001-BRIEF.md` and `001-FEEDBACK-Claude-Opus5-5.md`,
with material reused from `000-REVIEW-opencode-deepseek-4-1.md`. Every number
comes from a file in the repo, from that transcript (cited by line), or from
`review/002-band-analysis.R`, which is read-only and runs from the repo root.

---

## 0. Answers to the two clarification requests

### 0.1 Why 500/500 per genre is hard

The starting figure is smaller than it looks. **Project Gutenberg has about
80,000 records, not 2 million.** The trial snapshot holds 79,762 (provenance in
`data-raw/snapshots/2026-11-07/`), and about 62,000 of them are English text.
The 2.36 million is the UCSD Goodreads dataset. From those 62,000 a genre corpus
passes through four filters, and each one shrinks it:

| step | what it does | Science Fiction, as an example |
|---|---|---|
| 1. genre membership | book carries the genre's subject heading or shelf | 3,231 (subject) or 3,946 (shelf) |
| 2. author cap | at most 2 books per author per genre | about halves it: 889 / 1,238 authors |
| 3. band | 20/80 keeps the bottom 20% and top 20%, discards the middle 60% | 40% survive |
| 4. balance | take min(failures, successes) | 269 (subject) or 395 (shelf) per class |

So a 20/80 band with a cap of 2 needs about 2,500 capped books per genre to
yield 500 per class. No genre has that many by subject heading, and only
Adventure does by shelf. Section 3 shows what each lever buys. The short version:
**the band and the cap were designed for 2013's tiny counts. At today's counts
they can be relaxed safely, and the genre source matters more than either.**

### 0.2 The grid-drift bug

> **Status, 2026-11-17: fixed.** The fix landed on `master` as `39b987f` and
> is on the Mac. The fix proposed in the first draft of this section was
> withdrawn; what replaced it is described below under "The fix as landed".

The pre-registration fixes 25 snapshot dates in advance: 2026-11-07 plus
multiples of 30 days (`PREREGISTRATION.md` §2). The code did not compute those
dates. It asked one question: "is the newest snapshot at least 30 days old?"
(`R/downloadSnapshots.R:147` before the fix). Then it dated the new snapshot
with the day the job happened to run (`data-raw/snapshot-downloads.R:96`).

When the job runs every day, the two rules agree. The opencode session
simulated this and found them identical (transcript ~line 3992). They parted
company the first time a day was missed:

| | due date (pre-registered) | job actually runs | next due date, as the code computes it |
|---|---|---|---|
| on time | 2026-12-07 | 2026-12-07 | 2027-01-06 ✓ |
| Mac asleep on 12-07 | 2026-12-07 | 2026-12-08 | **2027-01-07** ✗, and every date after it is off by one |

One missed evening moved the whole remaining series off the published grid,
permanently. Each further miss added another day of drift, and the
pre-registration would then have named snapshots that do not exist. The
function's own docstring described the behaviour as a feature ("catches up on
its next run instead of skipping a month"), and it was half right: catching up
is good, but it must catch up *to the grid*.

**Why there is no perfect fix.** The counter only ever reports the last 30
days. Once a snapshot is δ days late, the series cannot get back onto the grid
without either counting δ days twice or leaving a hole. The first draft of
this section proposed dating a late snapshot by its grid date. That was
withdrawn, because it would label data for the wrong 30 days. Of the policies
left, the one chosen keeps the pre-registered dates:

**The fix as landed** (`39b987f`; `PREREGISTRATION.md` §4 and a §11 amendment
dated 2026-11-16):

- `SNAPSHOT_ANCHOR` is 2026-11-07. `nextSnapshotDate()` returns the first
  `anchor + 30k` after a given date, and `snapshotDue()` uses it.
- A late snapshot is written under the **day it was taken**, with the gap
  warned about and recorded, as before.
- The next snapshot is due on the **next grid date**, so its interval is
  30 − δ days. That is the only short interval the writer accepts. It warns
  that δ days are counted twice, records this in the provenance (`on_grid`),
  and does not adjust the data. Any other short interval is still refused as
  an overlap.
- `snapshotDue()` and `writeDownloadSnapshot()` share one rule, so the job never
  downloads the 121 MB feed only to have the write refused.

| | due date (pre-registered) | job actually runs | next due date, after the fix |
|---|---|---|---|
| on time | 2026-12-07 | 2026-12-07 | 2027-01-06 ✓ |
| Mac asleep on 12-07 | 2026-12-07 | 2026-12-08 (written, 1-day gap) | **2027-01-06** ✓ (29-day interval, 1 day counted twice) |

The alternative, treating a missed date as missing and waiting for the next
grid date, was rejected. Each miss would then cost a whole month, and under §4
more than one gap invalidates the wave. Six regression tests cover the change,
including the skipped day above and a day-by-day check that the scheduler and
the writer agree. The main one failed on the old code and passes on the new.

The fix makes a missed day cheap. It does not make missed days rarer: that is
what the collector safeguards in §7 are for.

---

## 1. Summary and release map

| release | content | where |
|---|---|---|
| **v1.04** (minor, optional) | the 2013 texts with every correction (boilerplate, LibriVox swaps), the 2013 labels frozen, per-genre training as in the paper as an option | Zenodo, as a derivative |
| **v2.00** (major, semver) | new schema (Parquet, entity tables), case-preserved texts fetched fresh from PG, expanded pool, new labels side by side | Zenodo (full); CRAN (flash demo under the cap) |

The five recommendations that matter most:

1. Fix the grid drift and add a second collector before 2026-12-07 (§0.2, §7).
   *Status 2026-11-17: grid drift fixed (`39b987f`); each snapshot is now copied
   to gpu-box nightly. A second independent collector is still open.*
2. Make the genre source the catalogue's `Category:` shelves, plus subjects,
   rather than the old allow-list. Relax the band to 30/70 and the cap to 3.
   That gives 500 per class in six of seven genres (§3).
3. Treat the 2013 corpus as a frozen milestone (v1.04) and a regression test,
   not as a template (§2).
4. Split Short Stories into single stories and collections. In the 2013 data
   the split nearly *is* the label (§10).
5. Build the hybrid label as several named components plus a knob, and keep
   the knob out of the H2 test (§8).

---

## 2. The "honour the original" constraint

The intent you remembered is written down in several places, and it compounds:

| where | text | effect |
|---|---|---|
| `AGENTS.md:27` | "Preserve backward compatibility but **not** at the expense of justifiable improvements" | balanced on paper, but agents read the first half |
| `TODO.md:11-12` | qualify the expansion code by reproducing the existing dataset "from scratch"; quality "corresponds to how faithfully it reproduces" it | makes 2013 fidelity the acceptance test for *new* data |
| `TODO.md:14` | "Maintain the original 50/50 split of success and failure" | read as exactly 50 per class |
| `TODO.md:62` | `fold`, `download_count`, `response` "stay in `novels`, frozen" | sensible, but it becomes "nothing new in `novels`" |
| `PREREGISTRATION.md` §5 steps 5–6 | ≥50 per class; cap of 2 that "mirrors the 2013 corpus" | carries 2013's quota logic into a census design |
| `SCHEMA.md` (Goodreads paragraph) | Goodreads "cannot supply a reception measure … scraping would breach its terms" | a ToS fact that has been treated as a hard stop |

**Recommendation.**

- Rewrite `TODO.md:14` as "balanced classes (equal proportions per genre)".
- Replace `TODO.md:11-12` with "the v1.04 build reproduces the 2013 *id set and
  labels* exactly (a regression test); the v2 builder is judged on its own
  audit gates".
- Amend §5 steps 5–6 with dated §11 entries. The registration has not frozen
  (2026-12-31), so this is still an edit, not a deviation.

A 2013 rule should be justified on its own merits. The cap of 2 is a good
example: it was a quota tool for a 100-book sample. With author-grouped folds,
leakage is already handled, so the cap only controls how much a prolific
author dominates. A cap of 3 does that job.

---

## 3. The 10× expansion, quantitatively

Run `Rscript review/002-band-analysis.R`.

- **Pool:** English texts with a 30-day count above zero in the 2026-11-07
  snapshot.
- **Genre source:** either LCSH subject patterns or the matching `Category:`
  shelf in gutenbergr 0.5.2.
- **Rule:** the per-genre percentile band, then the author cap per genre.
- **Reported:** min(failures, successes).
- **Caveat:** genre membership is a pattern or shelf match, not the
  pre-registered allow-list, so these are feasibility estimates.

### 3.1 Books per class, shelf source

| genre | pool | 20/80 cap 2 | 20/80 cap 3 | **30/70 cap 3** | 40/60 cap 3 | median, cap 3 | median, no cap |
|---|---|---|---|---|---|---|---|
| Adventure | 7,266 | 748 | 858 | **1,131** | 1,337 | 1,472 | 3,622 |
| Detective/Mystery | 1,962 | 217 | 253 | **354** | 438 | 483 | 980 |
| Historical Fiction | 3,848 | 425 | 490 | **667** | 785 | 903 | 1,920 |
| Poetry | 3,201 | 372 | 398 | **576** | 750 | 900 | 1,598 |
| Romance | 2,218 | 322 | 352 | **519** | 622 | 718 | 1,106 |
| Science Fiction | 3,946 | 395 | 460 | **617** | 717 | 799 | 1,965 |
| Short Stories | 3,306 | 363 | 427 | **600** | 773 | 892 | 1,650 |
| genres ≥ 500 | | 1 | 1 | **6** | 6 | 6 | 7 |

The same grid with **LCSH subjects** is much smaller: pools of 713–3,679, and
at most 5 of 7 genres reach 500 even with a median split and no cap. Historical
Fiction (713) and Romance (945) cannot reach it at all. **Which source defines
genre membership matters more than the band.** The current allow-list
(`inst/extdata/genre_allowlist.csv`) keys on older shelf names. Mapped through
it, Adventure returns 240 books against 7,576 on `Category: Adventure`. Re-key
it against the `Category:` vocabulary in the 2026-12-31 `pg_catalog.csv`
before the freeze.

### 3.2 Is it safe to widen the band? Counting noise says yes

The band exists to keep counting noise from deciding labels. In 2013 the
thresholds were 7–224 cumulative downloads, so noise was real. Today's 30-day
cuts sit at **c_lo ≈ 410–540 and c_hi ≈ 530–860**. The Poisson probability
that a re-measured labelled book lands in the *other* class is below 1e-12 at
30/70 and below 1e-4 at 40/60, for either genre source. The pre-registered rule
`c_hi − c_lo ≥ 4√c_lo` holds everywhere at 30/70. It fails at 40/60 only for
Science Fiction (both sources) and Short Stories (subject source), and at the
median split for every genre, where c_lo = c_hi. Summing 25 snapshots over the wave makes the counts about 25
times larger and the relative noise about 5 times smaller.

**But Poisson is the wrong worry. The counter has a floor.** Across the whole
catalogue the 30-day count has its 1st percentile at 178, its 10th at 336 and
its median at 564. Nearly every book gets a few hundred downloads a month
whether or not anyone reads it: residual crawlers, mirrors and aggregators that
slip past the robot filter. A 20th-percentile "failure" at about 420 is a book
at the floor. The 80th-percentile "success" at about 760 is less than twice
that. So the risk is not counting noise but **floor variability**: whether the
floor is stable, per book, from month to month. That is the reliability gate
in §9. The 2026-12-07 snapshot gives its first month-to-month pair *before*
the window opens. Compute the rank stability of each book between 11-07 and
12-07, and set the band from that, not from Poisson.

The counter does agree with the old label. For the 754 corpus books, Spearman
between the 2013 cumulative count and the 2026 30-day count is **0.745**, and
the 30-day count alone separates the 2013 classes with **AUC 0.898**. The two
measures largely agree, which supports the interim plan in §11.

### 3.3 Effect of n, band and response type on classifier performance

**n.** A log-linear fit to `data-raw/learning-curve.csv` gains about 0.03 AUC
per doubling of training books (α 0.05: 0.745 at 603 books). Extended to 5×
and 10× n, that gives **≈0.82 and ≈0.85**. Treat these as optimistic upper
bounds. The fit uses four points over a 4× range, the curve will flatten, and
the label will not be the 2013 one.

**Band.** A narrower band includes books whose true success is close to the
cut, so the task gets harder even with zero noise. AUC should *fall* as the
band narrows. The 2013 corpus cannot measure this, because its middle band was
discarded when it was built: 0 of 796 rows fall inside the band (`TODO.md`).
Larger n pushes AUC up and a narrower band pushes it down, and only an
experiment can net the two. The interim label in §11 makes that experiment
cheap. Precision and recall at a threshold move with AUC, but they also depend
on class balance, which stays proportional either way.

**Continuous response.** Model `log(share_per_million)` or within-genre rank.
- **Gains:** no discarded band, so every eligible book is used; ordering within
  the success tail is kept (the README Roadmap's "very successful versus
  modest"); and the noise rule disappears, replaced by a reliability weight.
- **Losses:** AUC is no longer the natural metric. Use Spearman or NDCG for the
  primary result, and report AUC and the IR statistics after thresholding at
  the 20/80 cuts, so the result stays comparable with the binary.
- **Caveat:** a band-sampled corpus still needs weights before predictions
  mean anything about Gutenberg as a whole. The wave admits every eligible
  book, so it is a census, not a band sample, and the caveat lapses.

**Recommendation.**
- Shelf plus subject membership; a 30/70 band (confirmed by the 12-07 rank
  stability); a cap of 3; balance as a proportion.
- Detective/Mystery either merged with its declared partner or accepted at
  about 350 per class.
- Continuous response as the primary exploratory analysis. The binary at the
  pre-registered cuts stays the H2 endpoint, since changing the endpoint now
  would cost the pre-registration its credibility.
- Revisit the MDE in §7: it assumed about 2,000 books, and the shelf pool at
  30/70 gives about 9,000 labelled.

---

## 4. Folds outside the dataset

Agreed. The `(author_key, scheme, fold)` table is the right shape. Two
corrections from the opencode session (transcript ~lines 1146–1175):

- **Co-authored books exist.** Three Dumas/Maquet titles mean 515 authors, not
  514. `author_folds.csv:71` keys the pair as one combined string.
- **Groups, not authors, get folds.** If `492` and `3930` were keyed separately
  and landed in different folds, their shared books would sit on both sides of
  the split.

**Design.**
- `authors(author_id = gutenberg_author_id, name)`.
- `book_authors(book_id, author_id)`.
- `author_groups(author_group_id, author_id)`, built by union-find over shared
  books.
- `folds(author_group_id, scheme, fold)`, append-only.

A surrogate `author_group_id` survives PG merging or splitting author records
without rewriting folds. Book folds are derived by joins, never stored. Each
milestone pins the table's SHA-256 in its manifest (§6). Tests:
- every group has exactly one fold per scheme;
- no book spans folds;
- existing rows are byte-identical after an append.

---

## 5. Parquet

Store one table per entity: `books`, `book_authors`, `authors`,
`author_groups`, `genre_membership`, `attributes` (word count, form,
`children_or_ya`, collection flag), `label_defs`, `labels`, `folds`,
`download_snapshots`. **Texts go in their own table**
(`texts(book_id, text, text_sha256, repair_version)`), so metadata stays tiny
and the CRAN demo can ship a subset. The texts must be case-preserving; see §12.

**nanoparquet or arrow.** Neither is installed on this machine. The comparison
below is from the packages' documentation as I know it. Check it against the
current releases before adopting.

| | nanoparquet | arrow |
|---|---|---|
| install | no dependencies, small, compiles anywhere CRAN does | large C++ library; binaries usually available, source builds slow |
| what it does | read/write a data frame to one Parquet file; common codecs | datasets: partitioned directories, lazy `dplyr` queries, streaming, every type |
| fit here | package-side reader/writer for small entity tables and the CRAN demo | the snapshot series (one partition per date) and full-corpus work in `data-raw` |

**Recommendation.** Put nanoparquet in `Imports` for the package's reader and
writer, and arrow in `Suggests` for the snapshot series and the full corpus.
`duckdb` is a third option worth a look: it queries hive-partitioned Parquet
with SQL or `dbplyr` and is a single install. Keep `data/novels.rda` only as a
thin convenience export for v1.04 users. Parquet does not solve the size
problem: `TODO.md` measured it only about 11% smaller than the bzip2 RDA. The
CRAN demo solves size (§10).

---

## 6. Milestones and an evolving schema

Agreed in round 1. The release is a manifest plus a git tag plus a Zenodo DOI:

```json
{ "dataset": "novels", "version": "2.0.0", "schema_version": 2,
  "code": {"git_tag": "v2.0.0", "commit": "…"},
  "sources": {"archive_sha256": "f775ad52…", "pg_catalog_sha256": "…",
              "snapshots": ["2026-11-07", "…"]},
  "tables": {"books.parquet": {"rows": 0, "sha256": "…"}, "…": {}},
  "label_defs": ["dl2013_band", "share_2027_28_band", "hybrid_v1"] }
```

Labels are **definitions plus data**. `label_defs` describes each rule as
parameters (source, window, band, cap). `labels(book_id, genre, label_def,
value)` holds the results. A new measure of success adds rows. It never
replaces a column. The 2013 label becomes one `label_def` among several.

---

## 7. Download aggregation, and collecting without a hosted service

What already works: the outcome is a share of platform downloads; the series is
append-only and carries provenance; the interval is enforced; backfill is
forbidden.

**The job is not scraping.** It fetches one file, the RDF feed PG publishes for
offline catalogues, once every 30 days. That is the feed's intended use. Your
concern is still fair: a cloud IP fetching it on a schedule could be throttled,
and the series cannot afford a block. The fail-safe options that need no
hosted service, cheapest first:

1. **Fix the grid drift** (§0.2), so a missed day costs one gap rather than the
   rest of the series. *Done 2026-11-17 (`39b987f`).*
2. **Wake the Mac on schedule:** `sudo pmset repeat wakeorpoweron MTWRFSU 21:55:00`
   ahead of the 22:00 cron.
3. **Commit and push after each snapshot,** from the script or a post-hook.
   Today the series exists only in the Mac's working tree; the one tracked
   snapshot was committed by hand (`4874c63`). That is the cheapest backup.
   *Done differently, 2026-11-17: committing on the Mac would break the
   never-commit-on-`dev` rule, so the Mac's cron now copies
   `data-raw/snapshots/` to gpu-box with rsync every night, append-only and
   retried daily (`TODO.md`). Snapshots reach git, and leave the house, when
   they are committed on gpu-box through a `data/` branch.*
4. **A second, independent collector on this Linux workstation,** writing to
   its own directory (`data-raw/snapshots-linux/`) on the same grid. Two
   residential machines on two networks are unlikely to fail on the same day.
   Merge rule: prefer the primary, use the secondary for a date only when the
   primary is missing, and record `collector` in the provenance. Agreement on
   dates both machines captured is a free reliability check.
5. **A dead-man's switch:** the job pings a heartbeat URL (healthchecks.io, or
   a cron `MAILTO`) on success, so a missed day raises an alert the same night
   rather than silence. The log is a heartbeat only for someone who reads it.
6. **A written gap policy:** today one missed month in the window invalidates
   a 2-year wave. Consider "up to two isolated missing snapshots allowed, the
   share computed over the snapshots present with the same divisor, sensitivity
   analysis reported". Amend before the freeze if you adopt it.

**Smaller points.**

- **The first window snapshot looks back before the window.** The 2027-01-06
  snapshot covers 2026-12-07 to 2027-01-06, mostly before the window opens.
  This is documented in §2 ("750 days") and harmless, since every book gets
  the same treatment.
- **Fetch timing.** PG regenerates the RDF daily, and the fetch hour relative
  to that regeneration decides which 30 days the counter covers. Record the
  RDF's own generation timestamp, if it has one, in the provenance.
- **The share is heavy-tailed.** The 99th percentile is 5,992 against a median
  of 564, so a handful of titles dominate the divisor. Use within-genre rank
  or log share for the continuous analysis.

---

## 8. Hybrid success, with a knob

**Components.** Each component is one table keyed by `book_id`, carrying its
source, retrieval date and coverage:

| marker | source | coverage risk for PG-era books |
|---|---|---|
| PG share of downloads (wave) | snapshots | none |
| Goodreads ratings count, mean | UCSD 2017 dump; scraping (see below) | 41.9% of the 2013 corpus by title+author match (316 books, 214 with ≥5 ratings; transcript ~2862) |
| author productivity (≥2 works) | PG catalogue, Wikidata | full, but it is a property of the author: it lifts the bibliographic baseline to AUC 0.816 (`TODO.md`, label audit) |
| reprints and translations | Wikidata (editions, P629 "edition or translation of"), Open Library editions | medium; Wikidata's PG id (P2034) covers only 1,822 of 79,746 ebooks, so match on title+author |
| film adaptations, once or more | Wikidata P144 ("based on") on film items | good for canon, sparse for the tail; *The Blue Lagoon* is the kind of case it finds |
| antecedents and homage | Wikidata P144/P737 ("influenced by"), plus text similarity | sparse; better as a discovery output than an input |
| NYT and Booker lists | list archives | thin: nearly all are post-1930, past PG's public-domain line |
| canon lists, prizes | Wikidata P166 ("award received"), curated lists | sparse; strongest at the very top |

**Knob designs**, in order of ambition:

- **(a) Weighted blend.** Rank-normalise each component within genre. Then
  `S(w) = Σ w_k z_k`, with `w` on the simplex and a missing component dropped
  from that book's sum (with a coverage weight). Simple and auditable.
- **(b) Latent factor.** An IRT, PCA or factor model across components, with
  `w` estimated from their covariance. This yields "the shared notion of
  success" and per-component loadings.
- **(c) A learned knob (the discovery tool).** Sweep `w` over a fixed grid on
  the simplex. For each `w`, fit the text model with *inner* folds and score
  it on the outer folds, then report the surface of held-out Spearman or AUC.
  The answer to "which notion of success does style predict best?" is a map,
  not a number. The fully learned version, reduced-rank regression or CCA
  between text features and components, literally turns the knob during
  training.

**The trap.** A knob chosen because the text model predicts it best has been
selected on text predictability, so a text-beats-baseline result under that
knob proves nothing. Fix the grid in advance, treat it as exploratory (§10
multiplicity), and keep H2 on the pre-registered label.

**Goodreads, stated plainly.**

- The UCSD Book Graph (Wan & McAuley, about 2017) has ratings, review text,
  popular shelves (including young-adult shelving) and similar-book links.
  **It has no download counts.** `ratings_count` is the nearest popularity
  proxy. It is released for academic use, so derived scores can be used in
  research, but redistributing its rows in the dataset needs checking against
  its terms.
- Its Young Adult subset is large, but PG's public-domain line limits the
  overlap: 14.1% of the children/YA pool (transcript ~2865).
- Scraping current Goodreads is feasible with existing tools, and it is against
  Goodreads' terms. The realistic exposure is account or IP blocking and an
  unpublishable raw table, not the code itself. If you scrape: rate-limit
  hard, store only derived ranks, and never ship raw scraped rows.
- `SCHEMA.md` is factually right about the terms. Reword it so it reads as a
  constraint on redistribution, not a ban on the measure.

---

## 9. Stylometric features, in two maintained sets

As amended: two feature sets, maintained and reported separately.

**A. Prediction set (AUC first).**
- TF-IDF unigrams (current).
- Character n-grams (robust to tokenisation and OCR noise).
- General embeddings (the TEI-server TODO).
- **Style embeddings** (Wegmann et al.; LUAR), which target style rather than
  topic.
- Judged by held-out AUC and Spearman, with no claim of interpretability.

**B. Interpretable set (insight first).** Report with SHAP-style attributions
and a plain table of the most influential features:

| ingredient | feature | tool |
|---|---|---|
| voice | function-word profile, Burrows' Delta distance to genre centroid | `stylo`, base R |
| syntax | sentence length mean and variance, dependency depth, POS n-grams | `udpipe` (R), spaCy |
| texture | MATTR lexical richness, readability, dialogue ratio | `koRpus`/`quanteda.textstats` |
| shape | sentiment arc (rise and fall), arc type | `syuzhet` |
| promise and payoff (the juggler) | entity introductions versus later re-mentions; open threads at each point; gap between introduction and payoff | BookNLP (Python, allowed by `AGENTS.md`) |
| allusion and homage | similarity of passages to the King James Bible, Shakespeare, Homer; quotation detection | `text2vec`/`textreuse`, a local reference corpus |
| zeitgeist | vocabulary dated against period corpora (how "of its time" the language is) | Google Books n-gram dates, or a diachronic embedding |

**Case preservation in v2 opens a new feature family:** proper-noun density,
size of the cast of named characters, place-name density. None of these can be
computed from the downcased 2013 texts.

---

## 10. Form, Fiction → Novel, and the Short Stories fix

Agreed. Content genre and form are two axes. In v2, relabel 2013 Fiction as
**Novel**; `Category: Novels` sits on 18,890 English texts. Keep "Fiction" as
the umbrella over flash, short story, novella and novel. Store `word_count`
and derive `form` from it, so the boundaries can be fuzzy and reported, not
enforced. Form stays out of the bibliographic-baseline argument (§5.5
circularity), but it is offered as a browsable category, because readers use
it.

**Short Stories: your suspicion is right, and it is worse than a mix.** In
`data/novels.rda`:

| Short Stories (2013) | n | success rate | median 2013 downloads | "stories / tales / sketches" in title |
|---|---|---|---|---|
| ≤ 8,000 words | 50 | **0.38** | 8 | 1 |
| > 8,000 words | 48 | **0.65** | 282 | 24 |

The word-count quartiles are 4.4k, 7.6k and 50k: a single story and a
collection differ by an order of magnitude. Within this genre, "is it a
collection?" almost decides the label. A classifier can score well by
detecting length. The same pattern runs through Science Fiction: 100 of 100 SF
books carry the LCSH heading "Short stories" (transcript ~4172).

**Recommendation.** Your third option, binning, with one refinement: base the
collection flag on content, not on length alone. A book is a collection when
it has more than 8,000 words **or** its subjects or title say "stories, tales,
sketches", **or** a table-of-contents heuristic finds several titled parts.
Label within each bin, so each bin's download counts measure comparable
things. Record the flaw in `SCHEMA.md` for v1.04, and fix it in v2.00.

**The CRAN demo.** Build it from the flash subset (≤1,500 words; 13 books in
the 2013 corpus, more in the expanded pool) plus a whole-book sample per genre,
and check it against the cap from fresh PG texts before committing. Point the
full corpus at the Zenodo DOI.

---

## 11. Interim development while the wave accrues

**The value of the v1.04 ids and 2013 counts, with fresh texts: high.** Every
non-label piece of v2 can be built and tested end to end now:

- the case-preserving text fetch and boilerplate stripping (judged against
  v1.04's cleaned text);
- the Parquet entity tables, the manifest, and folds keyed on author groups;
- both feature sets;
- per-genre training compared with the paper's numbers;
- the Short Stories binning test.

**Limits:** the labels are 2013 cumulative counts on a band-built corpus, so
results about the band or the continuous response cannot be read from it.

**A second interim label fills that gap:** the 2026-11-07 30-day snapshot (and
12-07 when it lands) over the expanded shelf pool. It is current, it covers
thousands of books, and it agrees with the 2013 label (Spearman 0.745, AUC
0.898). It is the natural bed for the experiments in §3.3:
- band 20/80 against 30/70 against the median;
- cap 2 against 3;
- binary against continuous.

Mark it "interim, not pre-registered" everywhere it appears.

**UCSD Goodreads as an interim component: yes, on the overlap,** as the
agreement gate the label audit currently skips, and as the first non-PG
component of the hybrid. It cannot stand in for download counts, because it
has none.

---

## 12. Pitfalls, ranked by severity

| # | pitfall | severity | one-line fix |
|---|---|---|---|
| 1 | Grid drift: `snapshotDue()` counts from the latest snapshot and the snapshot is dated by run day (`R/downloadSnapshots.R:147`, `data-raw/snapshot-downloads.R:96`) | fixed 2026-11-17 (`39b987f`) | anchor plus 30k; late snapshot keeps its real date, next one returns to the grid (§0.2) |
| 2 | One collector, on a laptop, with no automatic push; "no backfill" plus "one gap invalidates" | critical | push after each snapshot, second collector, `pmset` wake, heartbeat (§7) |
| 3 | Genre allow-list keyed to old shelf names; Adventure 240 mapped against 7,576 shelved | high | re-key to `Category:` shelves from the 2026-12-31 catalogue (§3.1) |
| 4 | Short Stories conflates single stories and collections; length nearly decides the label | high | collection flag, label within bins (§10) |
| 5 | 500/500 infeasible under 20/80 + cap 2 + subject membership | high | shelf source, 30/70, cap 3, proportional balance (§3) |
| 6 | 2013-fidelity wording in `TODO.md:11-14` and pre-registration §5 steers new work | high | reword; 2013 becomes a v1.04 regression test (§2) |
| 7 | Counter floor (10th percentile 336/month) is uncharacterised; the Poisson noise rule passes trivially | high | rank stability from the 11-07 and 12-07 snapshots before the freeze (§3.2) |
| 8 | Source texts downcased (2013 archive; `R/getIterator.R:14`) | high for v2 | fetch case-preserving texts; move lowercasing into feature code only |
| 9 | Fold key is the author name string; the co-author group is one pasted key (`author_folds.csv:71`) | medium | `author_group_id` by union-find (§4) |
| 10 | MDE in pre-registration §7 premised on about 2,000 books | medium | recompute from the frozen pool size before 2026-12-31 |
| 11 | SCHEMA.md's Goodreads sentence is read as a ban | medium | reword as a redistribution constraint (§8) |
| 12 | RDA-only output; CRAN size unresolved | medium | Parquet tables plus flash demo (§5, §10) |
| 13 | Metaprogrammed `_targets.R` (`bquote`/`tar_target_raw`) | medium | `tarchetypes::tar_map()` over folds, dynamic `pattern = cross()` for alpha |
| 14 | Science Fiction boilerplate fingerprint (54/100 texts, `TODO.md`) | medium | extend `stripGutenbergBoilerplate()`, add a regression test |
| 15 | Loose files: `xx`, `Rplots.pdf`, ignored-but-present `novels.R` | low | delete or move to `tmp/` |

**On item 13, the evidence differs from the stated concern.** The store's own
metadata (`targets::tar_meta()`) shows 216 targets and 459 s of total build
time. `tar_outdated()` finished in 1.7 s with **0 targets outdated**.
Re-sourcing the metaprogrammed `_targets.R` costs seconds, and targets still
skips everything that is up to date. The metaprogramming hurts readability and
debugging, which is reason enough to replace it, but it is not why a full run
is slow. The time goes to:

- **the 50 model fits: 218 s;**
- **per-fold re-tokenisation:** vectorizers 84 s and training DTMs 75 s,
  because every fold re-tokenises the same texts;
- `novels` (41 s) and `novels_data` (19 s, an 80 MB file).

**Recommended speed-ups:**
- tokenise once per book and subset per fold;
- run the 50 independent fits in parallel with a `crew` controller;
- split `novels` into metadata and texts, so text hashing happens once.

---

## Appendix: how the numbers were produced

- `review/002-band-analysis.R`, read-only. Its sections:
  - pools and the band × cap grid (LCSH and shelf sources), cuts, the noise
    rule and Poisson flip rates;
  - the learning-curve projection;
  - counter quantiles;
  - the 2013-versus-2026 agreement.

  Run with `Rscript review/002-band-analysis.R`, or
  `GENRE_SOURCE=shelf` for one source.
- Short Stories split: `stringi::stri_count_words()` on `data/novels.rda`,
  `genre == "Short Stories"`.
- Pipeline timings: `targets::tar_meta()`, `targets::tar_outdated()` on the
  store in this checkout (2026-11-16).
- Transcript references: `review/000-REVIEW-opencode-deepseek-4-1.md`, line
  numbers approximate.
