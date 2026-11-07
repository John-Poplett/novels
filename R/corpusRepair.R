# Repairing known corpus defects: LibriVox rows, boilerplate, titles.


#' Title getLibrivoxContaminatedIds
#'
#' Flags gutenberg_ids whose text contains the literal string "librivox".
#' This cannot appear in the genuine prose of any book in this corpus --
#' LibriVox did not exist until 2005, and every book here predates it by at
#' least a century -- so a match reliably indicates the original corpus
#' grabbed a LibriVox recording's catalog metadata instead of the actual
#' text edition for that gutenberg_id. Confirmed against a full-corpus
#' manual grep; see librivox_replacements for the authoritative record.
#'
#' @param df tibble with gutenberg_id and text columns, one row per book
#' @returns integer vector of contaminated gutenberg_ids
#' @export
getLibrivoxContaminatedIds <- function(df) {
  df$gutenberg_id[grepl("librivox", df$text, ignore.case = TRUE)]
}

# The five LibriVox-contaminated ids: each points at an audio recording's
# catalogue entry, not a text edition. Three are patched with a real text
# edition; the other two are dropped.
#
# Patched: ids (names) and the text editions that replace them (values). A
# contaminated id cannot be its own replacement -- re-downloading it returns
# the same audio page. The replacements were chosen by hand and checked
# against Gutenberg. Note 19836 (Fay Inchfawn, "Early Spring") is replaced by
# a different work by the same author, "The Verse-Book of a Homely Woman"
# (3477), because the collection itself is not available as text.
librivox_replacements <- c(
  "19513" = 18857L, # A Journey to the Center of the Earth
  "19640" = 76L,    # Adventures of Huckleberry Finn
  "19836" = 3477L   # Early Spring -> The Verse-Book of a Homely Woman
)

# SHA-256 of each compressed replacement text, pinned so a changed snapshot is
# an error instead of a silently different corpus. The files live in
# inst/extdata/librivox_replacements/ and are regenerated only by
# data-raw/librivox-replacements.R, which refuses to overwrite them.
librivox_replacement_sha256 <- c(
  "18857" = "8821ecf18e4a2fdedbe840464478685b4bd8214df75c22a0b8438b8f67858a29",
  "76"    = "d904a7b919e4ed6c7bd2e1a98e06d58509a70ba04ea79d79bc0c99001e03ffca",
  "3477"  = "f588005794cb0adc636571b9292f099af600d1611c539350366e44710976c7d5"
)

# Dropped: the real book is already in the corpus under another id, so
# patching would only add a second copy of it (and the audio entry's download
# count is the recording's, not the book's).
librivox_drops <- c(
  19678L, # The Call of the Wild -- the text edition is 215
  19782L  # Daddy-Long-Legs -- the text edition is 157
)

#' Title dropByGutenbergId
#'
#' @param df tibble with a gutenberg_id column
#' @param ids integer vector of gutenberg_ids to remove
#' @returns df without the rows whose gutenberg_id is in ids
#' @noRd
dropByGutenbergId <- function(df, ids) {
  df[!df$gutenberg_id %in% ids, ]
}

# Titles that no longer describe the replaced text. Only 19836 is a different
# work, not another edition of the same one; the collection "Early Spring" is
# not available as text anywhere we could find. The row keeps its original
# download count and label until those are refreshed against the new book.
librivox_title_corrections <- c(
  "19836" = "The Verse-Book of a Homely Woman"
)

#' Title applyTitleCorrections
#'
#' @param df tibble with gutenberg_id and title columns
#' @param corrections named character vector: names are gutenberg_ids,
#'   values are the corrected titles
#' @returns df with title replaced for every row whose id is in corrections
#' @noRd
applyTitleCorrections <- function(df, corrections) {
  idx <- match(as.character(df$gutenberg_id), names(corrections))
  hit <- !is.na(idx)
  df$title[hit] <- unname(corrections[idx[hit]])
  df
}

#' Title patchContaminatedText
#'
#' Replaces the text of known-contaminated ids with a real text edition. The
#' replacement is stored under a *different* id (see `replacements`), because
#' the contaminated id is the audio recording itself. The contaminated id
#' stays the row's key, so folds, labels and metadata are untouched and only
#' the text changes.
#'
#' The replacement texts are pinned data, shipped compressed in
#' `inst/extdata/librivox_replacements/` and verified against
#' `librivox_replacement_sha256` before use. Earlier versions downloaded them
#' from Gutenberg on every run, which made the pinned archive insufficient to
#' determine the dataset. The snapshot is lowercased to match the rest of the
#' corpus.
#'
#' @param df tibble with gutenberg_id and text columns, one row per book
#' @param replacements named integer vector: names are contaminated ids,
#'   values are the ids of the text editions that replace them
#' @returns df with text replaced for any contaminated ids present
#' @export
patchContaminatedText <- function(df, replacements) {
  affected <- intersect(as.character(df$gutenberg_id), names(replacements))
  if (length(affected) == 0) return(df)

  source_ids <- unname(replacements[affected])
  replacement_text <- vapply(source_ids, readReplacementText, character(1))

  df$text[match(affected, as.character(df$gutenberg_id))] <- replacement_text

  message(sprintf("patchContaminatedText: replaced text for %d id(s): %s",
                  length(affected), paste(affected, collapse = ", ")))
  df
}

#' Locate one stored LibriVox replacement text
#'
#' Resolution order mirrors storedFoldPath(): an explicit
#' `options(novels.replacements_path = <directory>)` override, then the
#' installed package, then the source tree. The override is a directory
#' containing `<source_id>.txt.xz` files and is intended for tests.
#'
#' @param source_id gutenberg id of the text edition
#' @returns path to the compressed replacement text
#' @noRd
replacementTextPath <- function(source_id) {
  name <- paste0(source_id, ".txt.xz")

  override <- getOption("novels.replacements_path")
  if (!is.null(override)) {
    path <- file.path(override, name)
    if (!file.exists(path)) {
      stop(
        "Stored LibriVox replacement text not found at ", path, " ",
        "(set by options(novels.replacements_path)). ",
        "Point the option at inst/extdata/librivox_replacements, or unset it.",
        call. = FALSE
      )
    }
    return(path)
  }

  installed <- system.file(
    "extdata", "librivox_replacements", name, package = "novels"
  )
  if (nzchar(installed) && file.exists(installed)) {
    return(installed)
  }

  in_tree <- findInSourceTree(
    file.path("inst", "extdata", "librivox_replacements", name)
  )
  if (nzchar(in_tree)) {
    return(in_tree)
  }

  stop(
    "Stored LibriVox replacement text not found: ", name, ". ",
    "Run from the package root (or install the package) and try again. ",
    "Regenerate only via data-raw/librivox-replacements.R, and re-pin the ",
    "digest in librivox_replacement_sha256.",
    call. = FALSE
  )
}

#' Read and verify one stored LibriVox replacement text
#'
#' @param source_id gutenberg id of the text edition
#' @returns the replacement text, lowercased and joined with newlines
#' @noRd
readReplacementText <- function(source_id) {
  path <- replacementTextPath(source_id)
  key <- as.character(source_id)
  expected <- librivox_replacement_sha256[[key]]

  if (is.null(expected)) {
    stop(
      "No pinned SHA-256 for replacement id ", source_id, ". ",
      "Add it to librivox_replacement_sha256 when adding a replacement.",
      call. = FALSE
    )
  }

  actual <- digest::digest(path, algo = "sha256", file = TRUE)
  if (!identical(tolower(actual), tolower(expected))) {
    stop(
      "SHA-256 mismatch for ", path, ". ",
      "Expected ", expected, ", got ", actual, ". ",
      "The snapshot changed: restore it from git rather than re-pinning ",
      "unless the corpus is meant to change.",
      call. = FALSE
    )
  }

  con <- xzfile(path, "rb")
  on.exit(close(con), add = TRUE)
  paste(readLines(con, warn = FALSE), collapse = "\n")
}

#' Title stripGutenbergBoilerplate
#'
#' Strips Project Gutenberg's standard license header/footer boilerplate
#' from a single book's text. Hand-rolled and case-insensitive rather than
#' using gutenbergr::gutenberg_strip(): this corpus's text was already
#' lowercased during the original corpus prep (confirmed by inspecting
#' novels$text directly), but Gutenberg's actual markers are uppercase
#' ("*** START OF THIS PROJECT GUTENBERG EBOOK ***") -- gutenberg_strip()
#' likely matches against that standard casing internally and would
#' silently fail to match anything in this already-lowercased corpus.
#' Patterns below are taken directly from a full-corpus
#' `find . -name '*.txt' | xargs egrep "\*\*\* (start|end)"` scan, not
#' assumed. The trailing "*** start: full license ***" section (license
#' boilerplate running to end of file) is already excluded by cutting the
#' body off at the "end of...ebook" marker, which always precedes it.
#'
#' @param text a single string containing one book's full text
#' @returns the same text with header/footer boilerplate removed
#' @export
stripGutenbergBoilerplate <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]

  start_idx <- which(grepl("\\*\\*\\* ?start of this project gutenberg ebook", lines, ignore.case = TRUE))
  end_idx   <- which(grepl("\\*\\*\\* ?end of this project gutenberg ebook", lines, ignore.case = TRUE))

  body_start <- if (length(start_idx) > 0) max(start_idx) + 1 else 1
  body_end   <- if (length(end_idx) > 0) min(end_idx) - 1 else length(lines)

  if (body_start > body_end) return("")  # defensive: malformed/unexpected marker order

  paste(lines[body_start:body_end], collapse = "\n")
}
