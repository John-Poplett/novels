# Calibrate the subject-heading genre strata against the 2013 labels.
#
# The successor to data-raw/genre-calibration.R (bookshelves). The 2013
# authors used "the Gutenberg catalog" for genre (paper, section 2) without
# naming the field; the Library of Congress subject headings in the
# catalogue's Subjects field recover their labels far better than the
# bookshelves do, which is the evidence for this mechanism.
#
# Strata (inst/extdata/genre_subjects.csv, R/subjectGenres.R) are multi-label
# by design: each is its own dataset. Fiction is all fiction; Novels is
# fiction over 50,000 words (PREREGISTRATION.md section 5.5), so it needs the
# text and is scored only on the corpus. The 2013 "Fiction" genre is scored
# against both, because the question is which of the two it was.
#
# Only recall is reported against 2013: the corpus is a quota sample (100
# books per genre, 50 per class), so a book's absence from a genre directory
# may mean a full quota, and precision is not measurable (SCHEMA.md).
#
#   Rscript data-raw/genre-calibration-subjects.R [CATALOGUE.csv.gz]
#
# CATALOGUE defaults to the frozen catalogue, data-raw/catalogue-freeze/
# pg_catalog.csv.gz. Before the freeze, pass a pre-freeze copy; its SHA-256
# is recorded with the result, so the calibration says which catalogue it
# was run against. Writes data-raw/genre-calibration-subjects.csv.

suppressMessages({
  library(dplyr)
  library(targets)
})
pkgload::load_all(quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
catalogue_path <- if (length(args) >= 1) args[1] else
  "data-raw/catalogue-freeze/pg_catalog.csv.gz"
if (!file.exists(catalogue_path)) {
  stop("No catalogue at ", catalogue_path, ". Pass a pg_catalog.csv.gz path, ",
       "or run data-raw/freeze-catalogue.R first.", call. = FALSE)
}
catalogue_sha256 <- digest::digest(catalogue_path, algo = "sha256", file = TRUE)
catalogue <- readCatalogue(catalogue_path)
map <- subjectGenreMap()

# ---------------------------------------------------- recall against 2013 ---

novels_tbl <- tar_read(novels)
books <- novels_tbl |>
  dplyr::distinct(gutenberg_id, .keep_all = TRUE) |>
  dplyr::transmute(gutenberg_id = as.character(gutenberg_id),
                   words = stringi::stri_count_words(text))
labels <- novels_tbl |>
  dplyr::transmute(gutenberg_id = as.character(gutenberg_id),
                   genre = as.character(genre)) |>
  dplyr::distinct()

subjects <- setNames(catalogue$Subjects, catalogue$`Text#`)
missing <- setdiff(books$gutenberg_id, names(subjects))
if (length(missing) > 0) {
  stop(length(missing), " corpus books are not in the catalogue (",
       paste(utils::head(missing), collapse = ", "), "...).", call. = FALSE)
}
strata <- Map(function(id, words) {
  novelStratum(assignSubjectGenres(subjects[[id]], map), words)
}, books$gutenberg_id, books$words)
names(strata) <- books$gutenberg_id

# 2013 genre -> the stratum it is scored against.
scored <- tibble::tribble(
  ~genre_2013,          ~stratum,
  "Adventure",          "Adventure",
  "Detective/Mystery",  "Detective/Mystery",
  "Historical Fiction", "Historical Fiction",
  "Romance",            "Romance",
  "Science Fiction",    "Science Fiction",
  "Short Stories",      "Short Stories",
  "Poetry",             "Poetry",
  "Fiction",            "Novels",
  "Fiction",            "Fiction"
)
recall <- scored |>
  dplyr::rowwise() |>
  dplyr::mutate(
    ids = list(labels$gutenberg_id[labels$genre == genre_2013]),
    n_truth = length(ids),
    true_positive = sum(vapply(ids, function(id) stratum %in% strata[[id]], logical(1))),
    recall = true_positive / n_truth
  ) |>
  dplyr::ungroup() |>
  dplyr::select(-ids)

# ------------------------------------------------- catalogue stratum sizes ---

eligible <- catalogue$Type %in% "Text" & catalogue$Language %in% "en" &
  !is.na(as.Date(catalogue$Issued, format = "%Y-%m-%d")) &
  as.Date(catalogue$Issued, format = "%Y-%m-%d") <= CATALOGUE_FREEZE_DATE
catalogue_strata <- lapply(catalogue$Subjects[eligible], assignSubjectGenres, map = map)
sizes <- tibble::tibble(stratum = c(map$stratum, "Novels")) |>
  dplyr::mutate(catalogue_books = vapply(stratum, function(s) {
    if (s == "Novels") NA_integer_ else
      sum(vapply(catalogue_strata, function(x) s %in% x, logical(1)))
  }, integer(1)))

result <- recall |>
  dplyr::full_join(sizes, by = "stratum") |>
  dplyr::mutate(catalogue_sha256 = catalogue_sha256,
                catalogue_eligible = sum(eligible),
                catalogue_with_any_stratum = sum(lengths(catalogue_strata) > 0))
readr::write_csv(result, "data-raw/genre-calibration-subjects.csv")

cat("catalogue:", catalogue_path, "\n  sha256", catalogue_sha256, "\n")
cat("  eligible English texts:", format(sum(eligible), big.mark = ","),
    " with any stratum:", format(sum(lengths(catalogue_strata) > 0), big.mark = ","), "\n\n")
print(as.data.frame(result[, c("genre_2013", "stratum", "n_truth", "true_positive",
                               "recall", "catalogue_books")]), row.names = FALSE)
cat("\nNovels needs the text, so its catalogue size is known only after the",
    "texts are fetched.\nwrote data-raw/genre-calibration-subjects.csv\n")
