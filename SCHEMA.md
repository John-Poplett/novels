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
* **Children and Young Adult** — a boolean attribute
  `children_or_ya_shelf` and, in the wave, a separate secondary track.
  Project Gutenberg has no young-adult-only shelf: the subject heading "young
  adult" appears on 5 of 61,930 English texts, while the combined
  "Children & Young Adult Reading" shelf appears on 6,346, and 2,492 more were
  added 2014–2025. In the 2013 corpus that shelf sits on 48 books across seven
  of the eight genres (Adventure 19, Detective/Mystery 13, Short Stories 11,
  Fiction 2, one each in three others), so it is analysed separately rather
  than pooled as a ninth genre.

What PG actually holds is early juvenile series, not contemporary YA: Tom Swift
36, Rover Boys 31, Motor Boys 25, Radio Boys 21, Bobbsey Twins 19, Outdoor
Girls 14 — and **Hardy Boys 0, Nancy Drew 0, The Catcher in the Rye 0**. The
first two have not been uploaded; Salinger is not public domain in the US. The
track is therefore "Children and Young Adult" as the shelf defines it, and the
publication must say so.

Goodreads cannot supply a reception measure: its API closed in December 2020
and scraping would breach its terms. Open Library ratings reach only 4.4% of
the corpus with five or more ratings; see the calibration note below.

## Calibration against the 2013 labels (2026-11-07)

`assignGenre()` plus `inst/extdata/genre_allowlist.csv` were scored on the 753
corpus books that carry a bookshelf entry (19836 has an empty one). This is the
mechanism proposed for books outside the 2013 corpus, so its error rates on
books with a trusted label are the only evidence it will ever have.

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
(`PREREGISTRATION.md` §12.1).

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


