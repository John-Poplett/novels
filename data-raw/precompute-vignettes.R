# Precompute the package vignettes against the full corpus.
#
# Each vignettes/<name>.Rmd.orig is the live source. Knitting it produces
# vignettes/<name>.Rmd, in which every code chunk has already been run and the
# results and figures are static text. That file is what ships, so R CMD build
# and CRAN never have to model the corpus.
#
# Run from the package root, before a release:
#   Rscript data-raw/precompute-vignettes.R
# The generated .Rmd files and vignettes/figures/ are committed.

# Knit one .Rmd.orig into a sibling .Rmd. Chunks are evaluated in the
# vignette's own directory so that figure links come out relative to it.
precompute_vignette <- function(orig, header = character()) {
  force(header)
  stopifnot(file.exists(orig), grepl("\\.Rmd\\.orig$", orig))
  name <- sub("\\.Rmd\\.orig$", "", basename(orig))
  dir <- normalizePath(dirname(orig))

  owd <- setwd(dir)
  old_opts <- knitr::opts_chunk$get()
  on.exit({
    setwd(owd)
    knitr::opts_chunk$restore(old_opts)
  })
  knitr::opts_chunk$set(fig.path = paste0("figures/", name, "-"))

  out <- paste0(name, ".Rmd")
  knitr::knit(basename(orig), output = out, quiet = TRUE, envir = new.env())

  readLines(out) |>
    insert_after_yaml(header) |>
    writeLines(out)

  file.path(dir, out)
}

# An HTML comment ahead of the YAML would break the front matter, so the
# provenance note goes immediately after its closing delimiter.
insert_after_yaml <- function(lines, header) {
  fences <- which(lines == "---")
  if (length(header) == 0L || length(fences) < 2L) {
    return(lines)
  }
  append(lines, header, after = fences[2])
}

provenance_header <- function(corpus_digest) {
  sha <- tryCatch(
    system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(e) NA_character_,
    warning = function(w) NA_character_
  )
  c(
    "",
    sprintf(
      "<!-- Precomputed from the matching .Rmd.orig by data-raw/precompute-vignettes.R on %s. Do not edit by hand. novels %s, git %s, corpus digest %s. -->",
      format(Sys.Date()),
      as.character(utils::packageVersion("novels")),
      if (length(sha) == 1L) sha else NA_character_,
      corpus_digest
    ),
    ""
  )
}

precompute_vignettes <- function(dir = "vignettes", expected_rows = 796L) {
  origs <- list.files(dir, pattern = "\\.Rmd\\.orig$", full.names = TRUE)
  if (length(origs) == 0L) {
    stop(
      "No *.Rmd.orig files in '", dir, "'. Rename each vignette to ",
      "<name>.Rmd.orig first, so the generated .Rmd does not overwrite its source.",
      call. = FALSE
    )
  }

  pkgload::load_all(quiet = TRUE)

  # The point of precomputing is to report results on the whole corpus; a
  # sample would produce plausible-looking numbers that mean something else.
  rows <- nrow(novels::novels)
  if (rows < expected_rows) {
    stop(
      "novels::novels has ", rows, " rows; expected at least ", expected_rows,
      ". Precompute against the full corpus.",
      call. = FALSE
    )
  }

  header <- provenance_header(digest::digest(novels::novels))
  origs |>
    purrr::map_chr(precompute_vignette, header = header) |>
    purrr::set_names(basename(origs))
}

if (sys.nframe() == 0L) {
  print(precompute_vignettes())
}
