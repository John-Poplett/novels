# Schema for "novels"

## The "genre" category
The Stonybrook original genre categories are listed
on the left and the proposed new categories appear
on the right.

It is not yet known how or if these categories were drawn out
of the Gutenberg corpus, particularly the gutenberg_metadata::gutenberg_bookshelf
field.

```bash
> $ egrep -i dataset novel_meta.txt
> ******************DATASET: Adventure ***********************************
> ******************DATASET: Detective/Mystery ***********************************
> ******************DATASET: Fiction ***********************************
> ******************DATASET: Historical Fiction ***********************************
> ******************DATASET: Love Story ***********************************
> ******************DATASET: Poetry ***********************************
> ******************DATASET: Sci-Fi ***********************************
> ******************DATASET: Short Stories ***********************************
```

* Adventure -> Adventure
* Detective/Mystery -> Detective/Mystery
* Fiction -> Fiction
* Historical Fiction -> Historical Fiction
* Love Story -> Romance
* Poetry -> Poetry
* Sci-Fi -> Science Fiction (or Science Fiction and Fantasy, or Speculative Fiction)
* Short Stories -> Short Stories

One style of genre shaping is based on size of a literary narrative. Thus, short stories, novels, novellas, and flash fiction legitimately belong to the super-class "Fiction". At the same time, "Historical fiction" is also for the most part a subset of novel.

The original Stonybrook  application of the "Fiction" label is very probably better classed as "novel". It may be in a future release of the dataset, the current Fiction will be reworked as "Novel" and Fiction will become an amalgam of Novel, Historical Fiction, Short Story, and Young Adult.

Novel is any fiction narrative work that exceeds 50,000 words. Novella is
15,001 to 50,000 words, short story 1,501 to 15,000, and flash 1,500 words or
fewer (published definitions vary between 1,000 and 1,500; this takes the wider
cut).

Two candidate categories are not content genres. One is an attribute, the
other a separate secondary track (see `PREREGISTRATION.md` §5.5–5.6 for the
measurements and the rule):

* **Form** — `flash` (≤ 1,500 words), `short story` (1,501–15,000), `novella`
  (15,001–50,000), `novel` (> 50,000). Measured on the 2013 corpus: 13, 194,
  138 and 409 books. Flash is thirteen books, none under 500 words, twelve of
  them Science Fiction, so there is no pool to label; and length alone reaches
  AUC 0.632, so making it a genre would put a predictor on the label axis and
  partly circularise the non-text baseline. The 1,500 ceiling is the wider end
  of the 1,000–1,500 range used in published definitions.
* **Children and Young Adult** — a boolean attribute `children_or_ya` and, in
  the wave, a separate secondary track. It is true when either signal fires:
  the "Children & Young Adult Reading" shelf (6,346 English texts) or an LC
  "juvenile" subject subdivision (6,632); their union is 8,613, and 4,365
  records carry both. They are different cataloguing practices, so the union is
  the honest marker — the shelf alone misses 4 of 36 Tom Swift, 5 of 25 Motor
  Boys and 3 of 4 Moving Picture Boys. Young adult fiction is a legitimate
  standalone category, and LC (13–18) and the book trade both separate it from
  children's fiction; Project Gutenberg's records do not, and exactly 1 of
  61,930 English texts carries the heading "Young adult fiction". No PG signal
  isolates the 13–18 band, so the track is the umbrella and is named for it.
  In the 2013 corpus 71 books carry a signal; the 48 shelf books alone sit
  across seven of the eight genres (Adventure 19, Detective/Mystery 13, Short
  Stories 11, Fiction 2, one each in three others), so the track is analysed
  separately rather than pooled as a ninth genre.

What PG actually holds is early juvenile series, not contemporary YA: Tom Swift
36, Rover Boys 31, Motor Boys 25, Radio Boys 21, Bobbsey Twins 19, Outdoor
Girls 14 — and **Hardy Boys 0, Nancy Drew 0, The Catcher in the Rye 0**. The
first two have not been uploaded; Salinger is not public domain in the US. The
track is therefore "Children and Young Adult" as the catalogue defines it, and
the publication must say so.

Goodreads cannot supply a reception measure: its API closed in December 2020
and scraping would breach its terms. Open Library ratings reach only 4.4% of
the corpus with five or more ratings; see the calibration note below.

## Calibration against the 2013 labels (2026-11-07)

`assignGenre()` plus `inst/extdata/genre_allowlist.csv` were scored on the 753
corpus books that carry a bookshelf entry (19836 has an empty one). This is the
mechanism proposed for books outside the 2013 corpus, so its agreement with the
only independently produced labels is the only evidence it will ever have. Read
that evidence with the caveat below in hand: the reference is a quota sample, so
of the two columns only recall carries meaning.

Exact set agreement is 24.6% with every allow-list row and 41.7% after dropping
the judgment calls. Exact matching is a poor headline because a book can carry
several Stonybrook genres and several mapped shelves; the per-genre figures are
the result (`data-raw/genre-calibration.R`, output in
`data-raw/genre-calibration.csv`, all rows in):

| genre | recall | precision |
| --- | --- | --- |
| Poetry | 0.89 | 1.00 |
| Science Fiction | 0.98 | 0.78 |
| Historical Fiction | 0.85 | 0.69 |
| Detective/Mystery | 0.72 | 0.79 |
| Adventure | 0.75 | 0.49 |
| Short Stories | 0.57 | 0.51 |
| Fiction | 0.82 | 0.17 |
| Romance | 0.25 | 0.39 |

### What this reference can and cannot measure

The corpus being scored against is a **quota sample, not a census**, and the
paper says so in section 2 ("Dataset Construction"): per genre it sets download
thresholds "to obtain at least 50 books for each class", then "for each genre,
we construct a dataset of 100 novels (50 per class)", with no author
contributing more than two books. The directory tree matches that exactly — 50
success and 50 failure files in each of the eight genre directories, 800 files
over 758 distinct ids (four of those files have no metadata row, so the shipped
table holds 796 genre rows over 754 books). The mechanism of labelling is
visible too: a book belongs to a genre when a copy of its text sits in that
genre's directory, and the 41 books in more than one genre are the ones that won
more than one quota.

That bounds the two columns differently:

* **Recall is measurable.** It asks whether the mechanism recovers the books the
  authors selected, and it can be read as a rate.
* **Precision is not.** A book absent from a genre directory may simply have lost
  a slot — once 50 successes were chosen for Short Stories, no further
  collection could enter it however obviously a collection it was. The negative
  class therefore contains true members, and precision below 1.00 is partly
  capacity rather than error. Fiction is the clearest case: 100 slots against a
  shelf that sits on 63.3% of the corpus.
* **Multi-genre books record a slot, not a judgement.** 12 books sit in both
  Science Fiction and Short Stories, Vonnegut's two titles among them, while the
  other 88 Science Fiction books are also short-story collections by their
  catalogue subjects and were labelled Science Fiction alone. The paper describes
  the Gutenberg catalogue as carrying a genre field (title, author, genre,
  language, download count) but never says how a book with two genres was
  resolved, so neither a dominance rule nor an oversight can be inferred from the
  data. The safe reading is that the corpus records which slots were filled, and
  that absence from a directory is weak evidence of non-membership.

Both weak rows are limits of Project Gutenberg's metadata, not of the mapping:

* **Fiction is a residual, not a category.** The site-level "Novels" shelf is
  on 477 of 753 books (63.3%) and is the only source of Fiction, so the mapping
  assigns Fiction to almost two books in three.
* **Romance cannot be recovered from shelves.** Only 25 of the 99
  Romance-labelled books carry a Romance shelf.

Dropping every judgment-call row removes Fiction completely (recall 0) and
leaves 169 books with no genre at all, so `allow_flagged = TRUE` is what the
wave needs even though it scores worse on exact matching.

**Consequence for the 2027-2028 wave.** Poetry, Science Fiction,
Detective/Mystery and Historical Fiction may be taken from shelves. Fiction may
not be treated as a level alongside them, and Romance needs a second source or
must leave the eight. That decision is due before 2026-12-31
(`PREREGISTRATION.md` §12.1). For every genre, the calibration is evidence about
**recall only** — precision against a quota sample must not be used as a quality
bar, nor tuned against, or the mechanism will be fitted to a file count.

Goodreads cannot serve as that second source: its API closed in December 2020
and scraping would breach its terms. Open Library ratings are usable in
principle but reach only 4.4% of the corpus with five or more ratings.

## Download snapshots

Project Gutenberg publishes a trailing 30-day flow, not a cumulative total: one
number per ebook, counted as unique address-days, with addresses fetching more
than 100 ebooks in a day excluded as robots. One snapshot a month is sufficient
because consecutive 30-day windows are disjoint, so the sum over K snapshots is
the downloads over K months without deconvolution. Daily snapshots would buy
daily resolution, which nothing here uses.

Source: `https://www.gutenberg.org/cache/epub/feeds/rdf-files.tar.bz2`, Project
Gutenberg's own dump, regenerated daily (121 MB compressed, 79,762 `pg<id>.rdf`
records on 2026-11-07). Gutendex reports the same numbers but is a third party;
a multi-year series should not sit downstream of it.

Layout under `data-raw/snapshots/`, append-only:

| file | columns |
| --- | --- |
| `<date>/downloads.csv.gz` | `gutenberg_id`, `snapshot_date`, `source`, `downloads` |
| `<date>/date_added.csv.gz` | `gutenberg_id`, `date_added` |
| `<date>/provenance.txt` | source URL, retrieval time (UTC), payload bytes and SHA-256, record count, total downloads |

`date_added` is the catalogue's `Issued` date: when the ebook was released, not
when the work was published. The RDF carries the original imprint for only
about 17% of records (`pgterms:marc260` 13,381, `pgterms:marc906` 12,980 of
79,762 on 2026-11-07), so separating publication era from upload era still
needs Wikidata or the MARC feed.

Withdrawals are deliberately not a column. A book present in one snapshot and
absent from the next was withdrawn; the analysis records that instead of
deleting the book from history.

The payload digest is recorded because the counter can change definition at
source, and a changed instrument must be distinguishable from a changed
reading. `writeDownloadSnapshot()` refuses to overwrite an existing date, and
`readGutenbergRdf()` refuses to return a table with missing counts.

The first snapshot is `2026-11-07`. It is a trial, not part of the 2027-2028
wave window.


