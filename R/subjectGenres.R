# Genre strata from Library of Congress subject headings (the catalogue's
# Subjects field), the successor to the bookshelf allow-list. Calibrated
# against the 2013 labels by data-raw/genre-calibration-subjects.R.

# A heading with one of these subdivisions is about the form, not an example
# of it ("Fiction -- History and criticism", "Romances -- Stories, plots,
# etc."), and never assigns a stratum.
SUBJECT_EXCLUDE <- paste(
  "History and criticism", "Technique", "Bibliography", "Study and teaching",
  "Stories, plots, etc.", sep = "|"
)

# Fiction strata other than Fiction itself; each implies Fiction.
FICTION_STRATA <- c("Adventure", "Detective/Mystery", "Historical Fiction",
                    "Romance", "Science Fiction", "Short Stories")

#' The subject-heading genre map
#'
#' Reads `inst/extdata/genre_subjects.csv`: one row per stratum, with a
#' regular expression matched against each subject heading.
#'
#' @returns a tibble with columns `stratum`, `pattern`, `note`
#' @noRd
subjectGenreMap <- function() {
  relative <- file.path("inst", "extdata", "genre_subjects.csv")
  path <- system.file("extdata", "genre_subjects.csv", package = "novels")
  if (!nzchar(path) || !file.exists(path)) path <- findInSourceTree(relative)
  if (!nzchar(path)) {
    stop("Shipped subject-genre map not found: ", relative, ". Run from the ",
         "package root (or install the package) and try again.", call. = FALSE)
  }
  map <- readr::read_csv(path, col_types = readr::cols(.default = "c"),
                         na = character(), progress = FALSE)
  stopifnot("genre_subjects.csv must have columns stratum, pattern, note" =
              identical(names(map), c("stratum", "pattern", "note")))
  map
}

#' Assign catalogue-level genre strata from subject headings
#'
#' Multi-label by design: each stratum is its own dataset, so a book may sit
#' in several. A heading assigns a stratum when it matches the stratum's
#' pattern and carries none of [SUBJECT_EXCLUDE]'s subdivisions. Every
#' fiction stratum ([FICTION_STRATA]) implies Fiction. Novels needs the text
#' and is added by [novelStratum()].
#'
#' @param subjects one book's `Subjects` field, headings separated by `"; "`
#' @param map from [subjectGenreMap()]
#' @returns a sorted character vector of strata, possibly empty
#' @noRd
assignSubjectGenres <- function(subjects, map = subjectGenreMap()) {
  stopifnot("subjects must be a single string" =
              is.character(subjects) && length(subjects) == 1)
  if (is.na(subjects) || !nzchar(subjects)) return(character())
  headings <- strsplit(subjects, "; ", fixed = TRUE)[[1]]
  headings <- headings[!grepl(SUBJECT_EXCLUDE, headings)]
  hit <- vapply(map$pattern, function(p) any(grepl(p, headings)), logical(1))
  strata <- map$stratum[hit]
  if (any(strata %in% FICTION_STRATA)) strata <- c(strata, "Fiction")
  sort(unique(strata))
}

#' The form class of a text, by word count (PREREGISTRATION.md section 5.5)
#'
#' @param words word counts
#' @returns a character vector: flash, short story, novella or novel
#' @noRd
textForm <- function(words) {
  stopifnot("words must be non-negative numbers" =
              is.numeric(words) && all(is.na(words) | words >= 0))
  as.character(cut(words, c(-Inf, 1500, 15000, 50000, Inf),
                   labels = c("flash", "short story", "novella", "novel")))
}

#' Add the Novels stratum: fiction of novel length
#'
#' @param strata one book's strata, from [assignSubjectGenres()]
#' @param words the book's word count
#' @returns `strata`, with `"Novels"` added when the book is Fiction and over
#'   50,000 words
#' @noRd
novelStratum <- function(strata, words) {
  stopifnot("words must be a single number" = is.numeric(words) && length(words) == 1)
  if ("Fiction" %in% strata && !is.na(words) && textForm(words) == "novel") {
    strata <- sort(c(strata, "Novels"))
  }
  strata
}
