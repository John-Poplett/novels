# Calibrate the bookshelf-derived genre assignment against the 2013 labels.
#
# The 2027-2028 label wave (see PREREGISTRATION.md, section 12) needs genres
# for books that are not in the 2013 corpus. The proposed mechanism maps a
# book's Project Gutenberg bookshelf entries to a target genre through
# inst/extdata/genre_allowlist.csv. Before that mechanism is trusted on books
# with no ground truth, it is scored on the 753 books that do have one.
#
# Two settings are reported, and neither is "the right one":
#
#   allow_flagged = TRUE   uses the whole allow-list, including the
#                          judgment-call rows. The generic "Novels" shelf is
#                          one of those rows and it maps to Fiction.
#   allow_flagged = FALSE  drops every judgment call. Fiction then has no
#                          mappings left at all, and 169 books get no genre.
#
# The interesting output is per genre: a mechanism can be excellent at
# Poetry and useless at Romance, and the aggregate hides that.
#
# Run from the package root after `tar_make()`:
#
#   Rscript data-raw/genre-calibration.R
#
# It writes data-raw/genre-calibration.csv and prints the summary.

suppressMessages({
  library(dplyr)
  library(readr)
  library(targets)
})
pkgload::load_all(quiet = TRUE)

# ------------------------------------------------------------------- data ---

novels_tbl <- tar_read(novels)

# One row per (book, Stonybrook genre): the trusted labels.
novel_metadata <- novels_tbl |>
  dplyr::select(gutenberg_id, genre)

# One row per (book, bookshelf entry). The shipped field is a single
# "/"-separated string in which content shelves are written as
# "Category: Adventure" and a few site-level shelves are not prefixed.
# assignGenre() expects the prefix stripped, one entry per row.
bookshelf_data <- novels_tbl |>
  dplyr::distinct(gutenberg_id, .keep_all = TRUE) |>
  dplyr::select(gutenberg_id, title, gutenberg_bookshelf) |>
  tidyr::separate_longer_delim(gutenberg_bookshelf, delim = "/") |>
  dplyr::rename(Category = gutenberg_bookshelf) |>
  dplyr::mutate(Category = sub("^Category: ", "", Category)) |>
  dplyr::filter(!is.na(Category), Category != "")

allowed <- genreAllowlist()
book_ids <- sort(unique(novels_tbl$gutenberg_id))
genres <- levels(novels_tbl$genre)

truth_by_book <- split(as.character(novel_metadata$genre), novel_metadata$gutenberg_id)

# ---------------------------------------------------------------- scoring ---

score_setting <- function(allow_flagged) {
  compared <- compareGenreAssignment(
    novel_metadata, bookshelf_data, allowed,
    allow_flagged = allow_flagged
  )

  # split() drops nothing here: every compared book has at least one label.
  assigned_by_book <- strsplit(compared$assigned_genres, "; ", fixed = TRUE)
  names(assigned_by_book) <- compared$gutenberg_id

  per_genre <- dplyr::bind_rows(lapply(genres, function(g) {
    truth <- vapply(book_ids, function(id) {
      g %in% truth_by_book[[as.character(id)]]
    }, logical(1))
    assigned <- vapply(book_ids, function(id) {
      entry <- assigned_by_book[[as.character(id)]]
      !is.null(entry) && g %in% entry
    }, logical(1))

    true_positive <- sum(truth & assigned)
    data.frame(
      genre = g,
      n_truth = sum(truth),
      n_assigned = sum(assigned),
      true_positive = true_positive,
      recall = true_positive / sum(truth),
      precision = if (sum(assigned) > 0) true_positive / sum(assigned) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))

  list(
    allow_flagged = allow_flagged,
    n_books = nrow(compared),
    exact_agreement = mean(compared$agrees),
    n_unassigned = sum(compared$assigned_genres == ""),
    per_genre = per_genre
  )
}

results <- lapply(c(TRUE, FALSE), score_setting)

summary_rows <- dplyr::bind_rows(lapply(results, function(r) {
  r$per_genre |>
    dplyr::mutate(
      allow_flagged = r$allow_flagged,
      n_books = r$n_books,
      exact_agreement = r$exact_agreement,
      n_unassigned = r$n_unassigned,
      .before = 1
    )
}))

readr::write_csv(summary_rows, "data-raw/genre-calibration.csv")

# --------------------------------------------------------------- reporting ---

cat("\n=== Genre calibration against the 753 labelled books ===\n")
for (r in results) {
  cat(sprintf(
    "\nallow_flagged = %s: exact set agreement %.1f%%; no genre assigned to %d books\n",
    r$allow_flagged, 100 * r$exact_agreement, r$n_unassigned
  ))
  out <- r$per_genre
  out$recall <- round(out$recall, 3)
  out$precision <- round(out$precision, 3)
  print(out, row.names = FALSE)
}

# Two facts that explain the shape of the table and determine what the wave
# can use. Both are printed rather than stored: they are properties of the
# shipped metadata, not of either setting.
all_books <- unique(bookshelf_data$gutenberg_id)
has_shelf <- function(shelf) {
  present <- unique(bookshelf_data$gutenberg_id[bookshelf_data$Category == shelf])
  mean(all_books %in% present)
}

romance_ids <- unique(novel_metadata$gutenberg_id[novel_metadata$genre == "Romance"])
romance_shelf <- unique(bookshelf_data$gutenberg_id[bookshelf_data$Category == "Romance"])

cat(sprintf(
  "\nBooks carrying the generic \"Novels\" shelf: %d of %d (%.1f%%)\n",
  length(intersect(all_books, unique(bookshelf_data$gutenberg_id[
    bookshelf_data$Category == "Novels"
  ]))), length(all_books), 100 * has_shelf("Novels")
))
cat(sprintf(
  "Romance-labelled books carrying a \"Romance\" shelf: %d of %d (%.1f%%)\n",
  length(intersect(romance_ids, romance_shelf)), length(romance_ids),
  100 * length(intersect(romance_ids, romance_shelf)) / length(romance_ids)
))
cat("\nwrote data-raw/genre-calibration.csv\n")
