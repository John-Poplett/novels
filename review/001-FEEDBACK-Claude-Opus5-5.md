# Feedback


> ## Document outline (`review/REVIEW.md`)
> 1. **Summary**: the five recommendations that matter most.
> 2. **The "honour the original" constraint**: quote the sources above. Argue
>    that the 2013 corpus should be one *frozen milestone* (v1.x) and not a
>    template for new waves. Keep balance as a proportion. Make reproduction of
>    2013 a regression test, not a design constraint.

Agreed. A minor 1.04 release. It should include a dataset based on
the same set of texts as the original except where all of the current
corrections and data cleaning (for example, boilerplate removel,
substitutions of the true intended text for librivox metadata remain.
The code for this release may be optionally modified to train each
genre on genre-specific datasets in alighment with the original.
The dataset, as a derivative of the original dataset, can and should
be published on Zenodo.


> 3. **10× expansion**: feasibility table, then options. Proportional balance
>    with unequal n per genre; class weights instead of exact quotas; drop the
>    middle-band discard when the outcome is continuous; relax the author cap
>    with author-grouped folds as the guard; genre merges. Also the allow-list
>    drift risk.

Agreed, if I read this rightly the upper and lower bands are **too
restrictive** despite titles in excess of 2 million altogether and
continued growth. Looking forward to a quantitative analysis of the
given options and a recommendation. Is widening of the bands
sufficient to expand worthy titles? Looking forward to the analysis
of the likely effects of a transition to a continous variable
response type and the consequences of dropping the upper and lower
band limits  on classifier performance (AUC and IR stats).

> 4. **Folds out of the dataset**: endorse the `(author_key, scheme, fold)`
>    table. Key it on gutenbergr `author_id` (with a co-author rule) rather than
>    the name. Hash-pin per milestone. Make the per-book fold derived, not stored.

Yes, please.

> 5. **Parquet**: one table per entity: `books`, `texts`, `genre_membership`,
>    `labels(label_def, …)`, `folds`, `download_snapshots`, `attributes`. Use
>    `arrow` or `nanoparquet`, partition the snapshots, keep RDA as a thin
>    convenience export. Texts are stored separately so metadata stays small for
>    CRAN.

Yes, please. Interested in the tradeoff betewen nanoparquet and arrow.

> 6. **Evolving schema and reproducible milestones**: a release manifest
>    (`manifest.json`: schema version, table hashes, source archive/catalogue
>    hashes, code git tag), paired with git tags. Labels as *definitions plus
>    data*, so new success measures are added, not substituted. Zenodo DOI per
>    milestone. Consider pins/DVC-lite, but recommend manifest + tags as enough.


Agree.

> 7. **Download aggregation critique**: the design (share of platform,
>    append-only, provenance) is sound. Problems: the grid-drift bug; the single
>    collector (add a redundant GitHub Actions/VPS collector and
>    auto-commit/push); the first window snapshot covers Dec 7–Jan 6, before
>    the window opens; the snapshot hour relative to PG's daily RDF regeneration;
>    the Poisson assumption (robots, spikes); a share dominated by a few titles
>    (suggest rank/percentile within genre); and whether "no backfill" should
>    allow a documented, flagged gap. Alternatives: rank-based outcome, a log
>    share with a mixed model, a 12-month interim read.

PG will detect and block scraping from a hosted service. So don't think this is
feasible.  What are other fail-safe options?

> 8. **Hybrid success with a knob**: candidate components (PG share, Goodreads
>    rating count and mean, Open Library, Wikidata awards/adaptations, library
>    holdings/WorldCat, canon lists). Candidate knobs:
>    (a) a weighted z-score/rank blend with weight w;
>    (b) a latent-factor (IRT/PCA) score;
>    (c) a learned w: sweep w as a hyperparameter and report the AUC/Spearman
>    surface, which is the discovery tool ("which notion of success does style
>    predict?"), with a warning about selecting w on test folds.
>    Goodreads: scraping options (existing dumps such as the UCSD Goodreads
>    dataset; tools; rate limits), ToS and legal risk stated plainly, and
>    correction of the `SCHEMA.md` claim. Coverage risk for obscure PG titles.


Other markers of success:

* Title from is from an author who has two or more published works.
* NYT top 100 lists.
* Booker Top x lists for novels and subgenres.
* Titles that have been reprinted or translated into other languages
* Titles that were made into movies once or two or more times (e.g., The Blue Lagoon)
* Titles that have antecedents (e.g., Catcher in the Rye -> King Dork, Mrs. Dalloway -> The Hours)

> 9. **Stylometric features for AUC and insight**: function-word profiles and
>    Burrows' Delta, sentence-length/dependency complexity (`udpipe`/spaCy),
>    readability, dialogue ratio, lexical richness (MATTR), POS n-grams, narrative
>    arc / sentiment trajectory (`syuzhet`), style embeddings (LUAR, Wegmann).
>    Map these to the README roadmap ideas: allusion → intertextual similarity to
>    the Bible/Shakespeare; Chekhov's gun → entity introduction and payoff
>    tracking (BookNLP); zeitgeist → period-vocabulary drift. Prefer
>    interpretable features plus SHAP-like reporting over raw AUC.

Agree with an amendment to maintain and improve feature sets into
to two groups: AUC / prediction oriented feature sets and a feature
set that "prefers interpretable features plus SHAP-like reporting".

> 10. **Flash, novella, and Fiction → Novel**: two axes, *content genre* ×
>     *form*. Relabel 2013 Fiction as Novel in a new milestone, with the frozen
>     original kept. Keep "Fiction" as the umbrella. Offer form as a browsable
>     category but keep it out of the bibliographic-baseline argument (circularity,
>     §5.5). Fuzzy boundaries are fine: store word count and derive form. Flash
>     has no pool on PG (13 books), so it needs another source or must be
>     presented as a demo subset.

Agreed. THe 'demo subset' could be culled from the "flash" genre. In any event, the
goal is to honor CRAN's 50 MB upper limit and its presumptive goal to keep datasets
light as CRAN is mirrored all over the globe.

> 11. **Pitfalls ranked by severity** (about 12):
>     1. grid drift in `snapshotDue`;
>     2. MacBook as the only collector, with no push;
>     3. allow-list vocabulary drift;
>     4. 500/500 is infeasible under the current rules;
>     5. the 2013-fidelity constraint embedded in TODO and the pre-registration;
>     6. the Goodreads ToS claim blocks the hybrid;
>     7. folds keyed by name string, co-authors unresolved;
>     8. MDE premised on about 2,000 books;
>     9. RDA-only output, with CRAN size unresolved;
>     10. the `bquote` metaprogramming in `_targets.R`;
>     11. boilerplate fingerprint (54/100 SciFi);
>     12. stale/ignored clutter (`xx`, `Rplots.pdf`, `novels.R` ignored but
k        present).

Agree. These need to be addressed.

Re: 4 and 5. Okay to relax constraints / rules as proposed in this document to
make larger datasets possible.

Re: 10. Not a fan of the bquote metaprogramming as it is difficult
to read and debug. Also, it defeats the performance advantage of
targets's  dependency system as metaprogramming forces the generation
of code every time it reaches that particular target.  As it a full
run of the  targets pipeline is much slower than it needs to be.

## Clarifications Given

* An instance of the repo and the resulting dataset is published on CRAN.
  The current 'novels' entry already exceeds CRAN's 50 MB cap. Our hope and
  intention is  use the flash category to slide in under that cap. This aligns
  with your idea of a 'demo' daaaset. In any case, our target is to publish the full
  dataset on Zenodo.

 * An optional, minor 1.04 release is contemplated. It should include a dataset based on the same set of texts
   as the original except where all of the current corrections and data cleaning (for example, boilerplate removel,
   substitutions of the true intended text for librivox metadata remain. The code for this release may be optionally
   modified to train each genre on genre-specific datasets in alighment with the original. The dataset, as a derivative
   of the original dataset, can and should be published on Zenodo.

 * The next releaee is a major and breaks away from the original schema. Its label will
   follow the rules of subrev.org (per Zenodo requirements), e.g., v2.00. It will eliminate
   downcasing from the dataset since otherwise we lose proper nouns and whatever signal they
   carry.

## Clarification Requests

* Please explain more plainly what constraints prevent us from reaching an heretofore presumed modest
10x expansion of 50/50 per genre to  500/500. PG has over 2m titles.

* Please explain more plainly the "grid-drift bug".

## Possible Refinements

* After browsing the [UCSD GoodReads Dataset project
page](https://cseweb.ucsd.edu/~jmcauley/datasets/goodreads.html)
it appears that the availability of a "genre", Young Adult Fiction,
has substantial representation.

## Open Items

Interim Development describes considerations for advancing the goals
of the project while the download count aggregator does its magic.

* Interim development: What value is there, assuming it is feasible,
in modeling the new dataset by using the same set of titles (gutenberg_id)
and download counts from the 1.04 dataset to generate forward-looking
proof-of-concepts and progress on all of the other specified features.
Note that the text would be not reused from the 1.04 dataset as it
has been downcased and in future releeases of the dataset the
requirement is to preserve case.

* Interim development: Evaluate the use of the USCD GoodReads Dataset,
if it preserves by-title download-counts, as an interim source while
our download count aggregator progresses to completion.

* I suspect that the short-story genre as constituted in the 2013
collection is flawed. The short story genre **should** consist of
individual stories alone.  The word counts of the text files under
the Short_Stories folder, suggest a mix of individual short stories
and collections. If that is the case, we are treating individual
stories and collections as though the download counts are a surrogate
measure for the same thing and that is the flaw.

A practical fix is to strip out collections by culling out any "short story"
from the corpus that exceeds 8000 words (7,500 plus a  500 word buffer).

An even cheaper fix would be to do nothing other than document the flaw.

Alternatively, we could bin individual stories separately from short story
collections. Then the download counts for each of these two categories
would be more trustworthy. Also, the split is not far off from even:

> john@Mac:~/MachineLearning/novels/data-raw/novels/Short_Stories
> $ find . -type f \( -name "*.txt" -o -name "*.TXT" \) ! -empty -exec wc -w {} + | awk 'NR==1{next} $1>8000 {print}' | cut -d' ' -f2- | wc -l
>       47
> john@Mac:~/MachineLearning/novels/data-raw/novels/Short_Stories
> $ find . -type f \( -name "*.txt" -o -name "*.TXT" \) ! -empty -exec wc -w {} + | awk 'NR==1{next} $1<=8000 {print}' | cut -d' ' -f2- | wc -l
>       53

This latter notion ssems like thet best of these three.

Other ideas for solutions to this problem are welcome.

