# One-off: store the author-grouped fold assignment as data.
#
# Writes inst/extdata/author_folds.csv (columns author, scheme, fold) from the
# packaged dataset. The assignment is exactly today's: seed 42, five folds,
# authors in order of first appearance, so every existing baseline stays valid.
#
# The table is APPEND-ONLY. It must never be regenerated, because the
# positional assignment reshuffles most existing authors whenever the number
# of authors changes (measured: 75-80% of them move, even for one new author).
# When the dataset grows, append rows for the new authors only and leave the
# existing rows alone. This script therefore refuses to overwrite.
#
# Run from the package root:  Rscript data-raw/author-folds.R

pkgload::load_all(quiet = TRUE)

path <- "inst/extdata/author_folds.csv"
if (file.exists(path)) {
  stop(path, " already exists. Fold assignments are append-only: add rows for ",
       "new authors by hand and leave existing rows alone.", call. = FALSE)
}

novels <- local({
  load("data/novels.rda")
  novels
})

dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
readr::write_csv(authorFoldTable(novels), path)
cat("wrote", path, "\n")
