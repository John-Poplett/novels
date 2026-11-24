# Plan: write `review/REVIEW.md` (architectural review per `review/BRIEF.md`)

## Context
The user wants a few pages (target 5–7) of architectural observations and
recommendations against the nine goals in `review/BRIEF.md`. Deliverable is one
new, uncommitted file: `review/REVIEW.md`. No code, data or other docs change.

Clarifications received:
- "50/50" means a balanced *proportion*; the user suspects agents have read it
  as an absolute 50 per class. Find and quote where the "honour the original"
  intent is written, and say whether it is slowing progress.
- "Convert fiction to novels" = relabel the 2013 "Fiction" genre as "Novel".
  Form categories (flash/short/novella/novel) mean something to readers. Don't
  over-engineer the boundaries.
- Hybrid success: user is keen, open to scraping Goodreads, and wants a tunable
  "knob" (possibly learned during training) that works as a discovery tool.
- Snapshot cron runs on the user's MacBook. List it as a pitfall. The pitfall
  list may go past 10.

## Evidence already gathered (read-only)
- **"Preserve the original" sources:** `AGENTS.md:27` (backward compatibility);
  `TODO.md:11-14` ("reproduce the existing dataset from scratch",
  "Maintain the original 50/50 split"); `TODO.md:62` (freeze
  `fold/download_count/response` in `novels`); `PREREGISTRATION.md` §5 steps 5–6
  ("≥50 per class", "cap 2… mirrors the 2013 corpus"); `SCHEMA.md` asserts
  Goodreads scraping "would breach its terms" and so rules out the source.
- **Pool feasibility.** Rough counts from gutenbergr 0.5.2: English text, LCSH
  match, author cap 2. Capped pools: Adventure 789, Detective 548,
  Historical 422, Romance 693, Poetry 1763, SciFi 1315, Short 2048,
  Fiction 7746. A 20/80 trim keeps 40% of each pool, so 500/500 needs about
  2,500 capped books per genre. Only Fiction gets there; most genres land at
  roughly 85–350 per class.
- **Allow-list drift.** `inst/extdata/genre_allowlist.csv` keys on old bookshelf
  names, but current metadata uses "Category: X" shelves. Mapped Adventure
  returns 240 books against 7,576 on "Category: Adventure". Flag as "verify
  against the 2026-12-31 `pg_catalog.csv`".
- **Grid bug.** `snapshotDue()` and `writeDownloadSnapshot()`
  (`R/downloadSnapshots.R:142,165`) measure 30 days *from the latest
  snapshot*, not from the 2026-11-07 anchor. One late run shifts every later
  snapshot off the 25 dates that PREREGISTRATION §2 lists. The code only warns
  about the gap.
- **Single collector.** The series lives only in the MacBook's working tree, and
  snapshots are git-tracked but nothing commits or pushes them automatically.
  The rules "no backfill" plus "a gap of more than a month invalidates" make one
  laptop a single point of failure for a 2-year wave.
- **Other facts:** folds are keyed by author *name string*
  (`R/createKFoldSplit.R`, `inst/extdata/author_folds.csv`) and co-authors are
  unresolved. `writeNovelsData()` writes bzip2 RDA only. `_targets.R` builds
  targets with `bquote`/`tar_target_raw`, where `tarchetypes::tar_map` plus
  dynamic branching would do. The MDE in §7 assumes about 2,000 books, but the
  feasibility counts suggest fewer after trimming.

## Document outline (`review/REVIEW.md`)
1. **Summary**: the five recommendations that matter most.
2. **The "honour the original" constraint**: quote the sources above. Argue
   that the 2013 corpus should be one *frozen milestone* (v1.x) and not a
   template for new waves. Keep balance as a proportion. Make reproduction of
   2013 a regression test, not a design constraint.
3. **10× expansion**: feasibility table, then options. Proportional balance
   with unequal n per genre; class weights instead of exact quotas; drop the
   middle-band discard when the outcome is continuous; relax the author cap
   with author-grouped folds as the guard; genre merges. Also the allow-list
   drift risk.
4. **Folds out of the dataset**: endorse the `(author_key, scheme, fold)`
   table. Key it on gutenbergr `author_id` (with a co-author rule) rather than
   the name. Hash-pin per milestone. Make the per-book fold derived, not stored.
5. **Parquet**: one table per entity: `books`, `texts`, `genre_membership`,
   `labels(label_def, …)`, `folds`, `download_snapshots`, `attributes`. Use
   `arrow` or `nanoparquet`, partition the snapshots, keep RDA as a thin
   convenience export. Texts are stored separately so metadata stays small for
   CRAN.
6. **Evolving schema and reproducible milestones**: a release manifest
   (`manifest.json`: schema version, table hashes, source archive/catalogue
   hashes, code git tag), paired with git tags. Labels as *definitions plus
   data*, so new success measures are added, not substituted. Zenodo DOI per
   milestone. Consider pins/DVC-lite, but recommend manifest + tags as enough.
7. **Download aggregation critique**: the design (share of platform,
   append-only, provenance) is sound. Problems: the grid-drift bug; the single
   collector (add a redundant GitHub Actions/VPS collector and
   auto-commit/push); the first window snapshot covers Dec 7–Jan 6, before
   the window opens; the snapshot hour relative to PG's daily RDF regeneration;
   the Poisson assumption (robots, spikes); a share dominated by a few titles
   (suggest rank/percentile within genre); and whether "no backfill" should
   allow a documented, flagged gap. Alternatives: rank-based outcome, a log
   share with a mixed model, a 12-month interim read.
8. **Hybrid success with a knob**: candidate components (PG share, Goodreads
   rating count and mean, Open Library, Wikidata awards/adaptations, library
   holdings/WorldCat, canon lists). Candidate knobs:
   (a) a weighted z-score/rank blend with weight w;
   (b) a latent-factor (IRT/PCA) score;
   (c) a learned w: sweep w as a hyperparameter and report the AUC/Spearman
   surface, which is the discovery tool ("which notion of success does style
   predict?"), with a warning about selecting w on test folds.
   Goodreads: scraping options (existing dumps such as the UCSD Goodreads
   dataset; tools; rate limits), ToS and legal risk stated plainly, and
   correction of the `SCHEMA.md` claim. Coverage risk for obscure PG titles.
9. **Stylometric features for AUC and insight**: function-word profiles and
   Burrows' Delta, sentence-length/dependency complexity (`udpipe`/spaCy),
   readability, dialogue ratio, lexical richness (MATTR), POS n-grams, narrative
   arc / sentiment trajectory (`syuzhet`), style embeddings (LUAR, Wegmann).
   Map these to the README roadmap ideas: allusion → intertextual similarity to
   the Bible/Shakespeare; Chekhov's gun → entity introduction and payoff
   tracking (BookNLP); zeitgeist → period-vocabulary drift. Prefer
   interpretable features plus SHAP-like reporting over raw AUC.
10. **Flash, novella, and Fiction → Novel**: two axes, *content genre* ×
    *form*. Relabel 2013 Fiction as Novel in a new milestone, with the frozen
    original kept. Keep "Fiction" as the umbrella. Offer form as a browsable
    category but keep it out of the bibliographic-baseline argument (circularity,
    §5.5). Fuzzy boundaries are fine: store word count and derive form. Flash
    has no pool on PG (13 books), so it needs another source or must be
    presented as a demo subset.
11. **Pitfalls ranked by severity** (about 12):
    1. grid drift in `snapshotDue`;
    2. MacBook as the only collector, with no push;
    3. allow-list vocabulary drift;
    4. 500/500 is infeasible under the current rules;
    5. the 2013-fidelity constraint embedded in TODO and the pre-registration;
    6. the Goodreads ToS claim blocks the hybrid;
    7. folds keyed by name string, co-authors unresolved;
    8. MDE premised on about 2,000 books;
    9. RDA-only output, with CRAN size unresolved;
    10. the `bquote` metaprogramming in `_targets.R`;
    11. boilerplate fingerprint (54/100 SciFi);
    12. stale/ignored clutter (`xx`, `Rplots.pdf`, `novels.R` ignored but
       present).

    Each gets a one-line fix.

## Remaining read-only checks while writing
- Confirm the bookshelf vocabulary that `data-raw/genre-calibration.R` reads
  (pg_catalog vs gutenbergr), to firm up or soften pitfall 3.
- Skim `data-raw/snapshot-downloads.R` for how `snapshot_date` is chosen (UTC
  date vs grid date).
- Look at `inst/extdata/author_folds.csv` for co-author keys.

## Verification
- Every quantitative claim in the review cites a file:line or the query used.
- Run `make lint`-equivalent hygiene on the new file (LF, no trailing blanks).
- Do not commit. Report the file path and the top findings to the user.
