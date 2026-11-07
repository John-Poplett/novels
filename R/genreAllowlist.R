#' Read the shipped genre allow-list
#'
#' The allow-list maps observed `gutenberg_bookshelf` category labels to the
#' corpus's target genres. Rows with an empty `target_genre` are deliberate
#' exclusions (periodicals, nonfiction subjects, non-English sub-taxonomies),
#' and rows with `flag_for_review = TRUE` are judgment calls that
#' `assignGenre(allow_flagged = FALSE)` can drop.
#'
#' Shipping the file with the package is what makes the exported genre API
#' usable: `assignGenre()` and `compareGenreAssignment()` take the allow-list
#' as an argument, and this accessor supplies the canonical copy.
#'
#' @returns a tibble with `gutenberg_bookshelf_category`, `target_genre`,
#'   `flag_for_review` and `note`
#' @seealso assignGenre, compareGenreAssignment
#' @export
genreAllowlist <- function() {
  relative <- file.path("inst", "extdata", "genre_allowlist.csv")

  path <- system.file("extdata", "genre_allowlist.csv", package = "novels")
  if (!nzchar(path) || !file.exists(path)) {
    path <- findInSourceTree(relative)
  }
  if (!nzchar(path)) {
    stop(
      "Shipped genre allow-list not found: ", relative, ". ",
      "Run from the package root (or install the package) and try again.",
      call. = FALSE
    )
  }

  allowlist <- readr::read_csv(path, show_col_types = FALSE)

  required <- c("gutenberg_bookshelf_category", "target_genre",
                "flag_for_review", "note")
  missing <- setdiff(required, names(allowlist))
  if (length(missing) > 0) {
    stop(
      "Genre allow-list is missing column(s): ", paste(missing, collapse = ", "),
      ". Expected: ", paste(required, collapse = ", "), ".",
      call. = FALSE
    )
  }

  allowlist
}
