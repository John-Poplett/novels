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
