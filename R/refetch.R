# Re-fetching the 2013 texts from a Project Gutenberg mirror (TODO.md,
# "Pristine text from scratch"). Network-free helpers; the fetching itself is
# data-raw/refetch-2013.R.

#' A book's directory on a Project Gutenberg mirror
#'
#' Mirrors file book `n` under one directory per leading digit of `n`, then
#' `n` itself: 215 is `2/1/215`, 1234 is `1/2/3/1234`. Books below 10 sit
#' under `0/`.
#'
#' @param id a single positive whole-number Gutenberg id
#' @returns the directory, relative to the mirror's root, without slashes at
#'   either end
#' @noRd
mirrorDir <- function(id) {
  if (!is.numeric(id) || length(id) != 1 || is.na(id) || id < 1 || id != round(id)) {
    stop("id must be a single positive whole number, not ", deparse(id), ".",
         call. = FALSE)
  }
  digits <- strsplit(format(id, scientific = FALSE, trim = TRUE), "")[[1]]
  head <- if (length(digits) == 1) "0" else digits[-length(digits)]
  paste(c(head, format(id, scientific = FALSE, trim = TRUE)), collapse = "/")
}

#' The entries of a mirror's directory listing
#'
#' @param html the listing page, as a character vector of lines
#' @returns the linked names, in page order, without the parent link;
#'   directories keep their trailing slash
#' @noRd
listingEntries <- function(html) {
  hrefs <- unlist(regmatches(html, gregexpr('href="[^"?#]+"', html)))
  names <- utils::URLdecode(sub('^href="([^"]+)"$', "\\1", hrefs))
  names <- names[!names %in% c("../", "./", "/") & !grepl("^[a-z]+:|^/", names)]
  unique(names)
}

#' Which text file of a book to fetch
#'
#' A book's directory may hold the text in more than one encoding. The order
#' of preference is UTF-8 (`<id>-0.txt`), then 8-bit (`<id>-8.txt`), then
#' ASCII (`<id>.txt`), so the file kept is the fullest form of the text.
#'
#' @param entries the directory's entries, from listingEntries()
#' @param id the book's Gutenberg id
#' @returns a one-row data frame: `file` and its `encoding` ("UTF-8",
#'   "latin1" or "ASCII"); an error if the directory has none of them
#' @noRd
pickTextFile <- function(entries, id) {
  candidates <- data.frame(
    file = paste0(id, c("-0.txt", "-8.txt", ".txt")),
    encoding = c("UTF-8", "latin1", "ASCII")
  )
  hit <- candidates[candidates$file %in% entries, , drop = FALSE]
  if (!nrow(hit)) {
    stop("Book ", id, " has no plain-text file (", paste(candidates$file, collapse = ", "),
         ") in its mirror directory; it holds: ",
         paste(utils::head(entries, 10), collapse = ", "),
         ". Check the directory by hand.", call. = FALSE)
  }
  hit[1, , drop = FALSE]
}

#' The Gutenberg ids to re-fetch for the 2013 corpus
#'
#' One per book in the corpus. A patched LibriVox row is fetched from its
#' replacement text's id, because that is the text the corpus holds.
#'
#' @param ids the corpus's `gutenberg_id`s, repeats allowed
#' @param replacements named integer vector, corpus id -> source id
#' @returns a data frame of `gutenberg_id` and `source_id`, one row per
#'   distinct id, sorted
#' @noRd
refetchIds <- function(ids, replacements = librivox_replacements) {
  if (!length(ids) || anyNA(ids)) stop("ids must be non-missing Gutenberg ids.", call. = FALSE)
  ids <- sort(unique(as.integer(ids)))
  source <- ids
  hit <- as.character(ids) %in% names(replacements)
  source[hit] <- unname(replacements[as.character(ids[hit])])
  data.frame(gutenberg_id = ids, source_id = as.integer(source))
}
