# Instructions

You are asked to review and critique this repo in consideration of
the goals listed below. produce a few pages of observations
and recommendations, leaning towards architectural considerations,
and ideas for improvement in the given context.

#  Goals

The ones to concentrate on here are:

* Expand # of examples for each genre in the dataset 10x, i.e.. for each genre
  go from 50/50 to 500/500.
* Split hard-coded folds out of the original dataset but provide means to record them for the sake of reproducable
  results.
* Recast the dataset in parquet in preference to R specific RDA.
* Devise code and schema(s) that permit the dataset to evolve but at the same
  preserve enough state to reproduce different milestone versions of the dataset.
* Review and critique the current method for aggregating new download counts per
  PREREGISTRATION.md. Suggests alternatives if necessary.
* Propose alternative,  hybrid definitiona of success (e.g., a value produced from a combination of GoodReads ratings and Gutenberg Project (GP) download counts.
* Propose new or existing  stylometric feature to improve AUC and other IR
  stats not just to improve AUC but to gain insights into what makes up the ingredients for success (see last bullet item of the ROADMAP section for ideas.
* Expand dataset categories to include flash fiction and novellas.
  Evaluate what it would take to convert fiction--as it exists in the 2013 dataset--to novels and "fiction"--more accurately--as a superset of flash,
  short stories, novellas and novels.
* Catalog pitfalls in the current state of the repo as they are discovered but
  don't go on an Easter egg hunt. Rank them in order of severity, and trim the
  list of 10 items or less.

Concentrate mostly  on _targets.R, README.md, PREREGISTRATION.md, and TODO.md to get a sense of the current state of the project
and its direction.
