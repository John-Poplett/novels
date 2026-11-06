# One-off: snapshot the three LibriVox replacement texts.
#
# patchContaminatedText() replaces the text of three contaminated ids with a
# real text edition fetched under a different id. Fetching at run time made the
# corpus non-hermetic: the pinned archive no longer determined the dataset.
#
# This script stores the replacement text as shipped data, compressed, so the
# pipeline reads bytes from git instead of the network. The SHA-256 of each
# compressed file is pinned in R/targetFunctions.R and verified before use.
#
# The snapshot is frozen. Regenerating it changes the corpus, so this script
# refuses to overwrite an existing directory.
#
# Run from the package root:  Rscript data-raw/librivox-replacements.R

pkgload::load_all(quiet = TRUE)

dir <- "inst/extdata/librivox_replacements"
if (dir.exists(dir) && length(list.files(dir)) > 0) {
  stop(dir, " already exists. The replacement texts are pinned data: ",
       "changing them changes the corpus and every baseline. Remove the ",
       "directory deliberately if that is really what you mean.", call. = FALSE)
}

replacements <- novels:::librivox_replacements
downloaded <- gutenbergr::gutenberg_download(
  unname(replacements), meta_fields = character(0)
)

dir.create(dir, recursive = TRUE, showWarnings = FALSE)

for (i in seq_along(replacements)) {
  source_id <- replacements[[i]]
  text <- downloaded$text[downloaded$gutenberg_id == source_id] |>
    paste(collapse = "\n") |>
    stringi::stri_trans_tolower()

  path <- file.path(dir, paste0(source_id, ".txt.xz"))
  con <- xzfile(path, "wb", compression = 9)
  writeLines(text, con)
  close(con)

  cat(path, digest::digest(path, algo = "sha256", file = TRUE), "\n")
}
