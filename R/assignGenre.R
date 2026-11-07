#' Title assignGenre
#'
#' Maps a book's gutenberg_bookshelf entries to one or more target genres,
#' via the allow-list built from a full pass over the observed Category
#' factor levels (see genre_allowlist.csv). Multi-label by construction --
#' a book can legitimately land in more than one genre, matching the
#' corpus's existing intentional many-to-many design (see novels-project
#' notes on why folds/genre membership are never treated as one-per-book).
#'
#' Rows in the allow-list with an empty target_genre are exclusions
#' (periodicals, non-English sub-taxonomies, nonfiction subjects, etc.)
#' and never contribute a genre. Filtered with is.na() as well as != "" --
#' readr::read_csv()'s default na argument treats an empty CSV field as
#' NA, not "", so relying on != "" alone lets NA rows survive (NA != ""
#' evaluates to NA, and data-frame subsetting with an NA logical index
#' inserts an all-NA row rather than dropping it, which shows up
#' downstream as a literal "NA" pseudo-genre).
#'
#' Rows flagged flag_for_review == TRUE are still applied by default --
#' they represent judgment calls made during the allow-list's
#' construction, not confirmed-wrong mappings -- but allow_flagged =
#' FALSE lets you see how sensitive the result is to those specific
#' judgment calls before trusting them.
#'
#' @param bookshelf_entries character vector of one book's
#'   gutenberg_bookshelf entries (already split one-per-row/value, matching
#'   the shape produced upstream: one row per (gutenberg_id, bookshelf
#'   entry), Category already stripped of any "Category: " prefix)
#' @param allowlist the allow-list tibble/data.frame (gutenberg_bookshelf_category,
#'   target_genre, flag_for_review, note); see genreAllowlist() for the
#'   canonical copy shipped with the package
#' @param allow_flagged if FALSE, ignore allow-list rows marked
#'   flag_for_review == TRUE, so you can see which assigned genres are
#'   robust to dropping the judgment-call mappings entirely
#' @returns a character vector of assigned genres (possibly length 0, for
#'   a book whose bookshelf entries are all excluded/unmapped; possibly
#'   length > 1, for a book with multiple genre-mapped entries)
#' @export
assignGenre <- function(bookshelf_entries, allowlist, allow_flagged = TRUE) {
  candidates <- allowlist[allowlist$gutenberg_bookshelf_category %in% bookshelf_entries, ]
  candidates <- candidates[!is.na(candidates$target_genre) & candidates$target_genre != "", ]
  if (!allow_flagged) {
    candidates <- candidates[!candidates$flag_for_review, ]
  }
  unique(candidates$target_genre)
}
