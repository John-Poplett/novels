#' Title compareGenreAssignment
#'
#' Runs assignGenre() against every book already in the existing corpus and
#' compares its output to Stonybrook's original genre label(s) for that
#' book. This is the calibration step: since every book here already has a
#' known, trusted genre, disagreements are the interesting output, not
#' errors to suppress -- they tell you whether the bookshelf-derived
#' assignment is more or less defensible than Stonybrook's original label,
#' before this same mechanism gets trusted to assign genre to books
#' outside the existing corpus where there's no ground truth to check
#' against at all.
#'
#' A book can have multiple Stonybrook genre memberships already (the
#' corpus's existing intentional multi-genre design), so both sides are
#' compared as sets, not single values.
#'
#' @param novel_metadata the parsed metadata tibble (gutenberg_id, genre
#'   columns; one row per (book, Stonybrook genre) combination, as
#'   currently produced upstream)
#' @param bookshelf_data long-format bookshelf data (gutenberg_id, Category
#'   columns; one row per (book, bookshelf entry), matching the shape
#'   already built via the earlier Category mutate/split -- e.g. what was
#'   shown as `x` in the console: gutenberg_id, gutenberg_bookshelf,
#'   Category)
#' @param allowlist the allow-list tibble/data.frame, e.g. read from
#'   genre_allowlist.csv
#' @param genre_rename_map a named character vector translating
#'   Stonybrook's raw genre labels into the same target_genre naming
#'   convention the allow-list uses, so the comparison is apples-to-apples
#'   rather than surfacing naming mismatches as false disagreements.
#'   Two confirmed renames are seeded as defaults
#'   (Detective/Mystery -> Mystery/Detective, Love Story -> Romance) --
#'   confirm the remaining entries against `levels(novel_metadata$genre)`
#'   directly before trusting this default for the others; unlisted raw
#'   genre values pass through unchanged
#' @param allow_flagged passed through to assignGenre() -- set FALSE to
#'   see which disagreements persist even after dropping every
#'   judgment-call mapping from the allow-list
#' @returns a tibble, one row per gutenberg_id: stonybrook_genres and
#'   assigned_genres (each a comma-joined string), genres_only_in_stonybrook,
#'   genres_only_in_assigned, and agrees (TRUE if the two sets match exactly)
#' @export
compareGenreAssignment <- function(novel_metadata, bookshelf_data, allowlist,
                                     genre_rename_map = c(
                                       "Detective/Mystery" = "Mystery/Detective",
                                       "Love Story" = "Romance"
                                     ),
                                     allow_flagged = TRUE) {

  stonybrook_by_book <- novel_metadata |>
    dplyr::mutate(
      genre_renamed = dplyr::if_else(
        as.character(genre) %in% names(genre_rename_map),
        genre_rename_map[as.character(genre)],
        as.character(genre)
      )
    ) |>
    dplyr::group_by(gutenberg_id) |>
    dplyr::summarize(stonybrook_genres_set = list(unique(genre_renamed)), .groups = "drop")

  assigned_by_book <- bookshelf_data |>
    dplyr::group_by(gutenberg_id) |>
    dplyr::summarize(
      assigned_genres_set = list(assignGenre(Category, allowlist, allow_flagged = allow_flagged)),
      .groups = "drop"
    )

  dplyr::inner_join(stonybrook_by_book, assigned_by_book, by = "gutenberg_id") |>
    dplyr::mutate(
      only_in_stonybrook = purrr::map2(stonybrook_genres_set, assigned_genres_set, setdiff),
      only_in_assigned = purrr::map2(assigned_genres_set, stonybrook_genres_set, setdiff),
      agrees = purrr::map2_lgl(
        stonybrook_genres_set, assigned_genres_set,
        \(x, y) length(only_in_stonybrook <- setdiff(x, y)) == 0 &&
          length(setdiff(y, x)) == 0
      ),
      stonybrook_genres = purrr::map_chr(stonybrook_genres_set, paste, collapse = "; "),
      assigned_genres = purrr::map_chr(assigned_genres_set, paste, collapse = "; "),
      genres_only_in_stonybrook = purrr::map_chr(only_in_stonybrook, paste, collapse = "; "),
      genres_only_in_assigned = purrr::map_chr(only_in_assigned, paste, collapse = "; ")
    ) |>
    dplyr::select(gutenberg_id, stonybrook_genres, assigned_genres,
                   genres_only_in_stonybrook, genres_only_in_assigned, agrees)
}
