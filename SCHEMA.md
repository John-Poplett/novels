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

Novel could be identified as any non-fiction narrative work that
exceeds 50,000 words. Novella then subsequently would be 15,000 to 49,999 words, short story between 1000 and 15,000 words, flash, 1,000 words or less.

Some additional genre categories to consider--if mappings to gutenberg_metadata$bookshelf_metadata
can be worked--include:

* Children and Young Adult
* Flash Fiction

Further alignment with the online rating book rating system, GoodReads, is of interest particulary if we plan to
based our response value from a combination of Gutenberg download counts and Goodread's rating system. The
proposal to transition from the Love Stories category to Romance is a nod to this goal.

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

