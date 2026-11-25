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

#' Strip the pulp-magazine transcriber's note at the end of a book
#'
#' 73 books, mostly science fiction transcribed from magazines, end with a
#' block like "transcriber's note: this etext was produced from _astounding
#' science fiction_ march 1959. extensive research did not uncover any
#' evidence that the u.s. copyright on this publication was renewed. minor
#' spelling and typographical errors have been corrected without note."
#' It is provenance, not the author's text, and it leaks the label: in
#' Short Stories all 11 books carrying it are successes (Fisher p = 0.0005;
#' review of 2026-11-24).
#'
#' Only that block is removed: the last line reading "transcriber's note"
#' (colon optional), when what follows it to the end names the source
#' ("this etext was produced from") and the copyright renewal, and is at
#' most `max_chars` long, so a note can never take a chapter with it.
#' Trailing blank lines before it go too.
#'
#' @param text a single string containing one book's text
#' @param max_chars the longest block treated as a note (the corpus's
#'   longest is under 400 characters)
#' @returns the text without the note; unchanged if it has none
#' @noRd
stripTranscriberNote <- function(text, max_chars = 1000) {
  stopifnot("text must be a single string" = is.character(text) && length(text) == 1)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  heads <- which(grepl("^[[:space:]]*transcriber.?s[[:space:]]+notes?[[:space:]]*:?[[:space:]]*$",
                       lines, ignore.case = TRUE, perl = TRUE))
  if (!length(heads)) return(text)
  h <- max(heads)
  block <- paste(lines[h:length(lines)], collapse = " ")
  is_note <- nchar(block) <= max_chars &&
    grepl("this[[:space:]]+e-?text[[:space:]]+was[[:space:]]+produced[[:space:]]+from", block,
          ignore.case = TRUE, perl = TRUE) &&
    grepl("copyright[[:space:]]+on[[:space:]]+this[[:space:]]+publication[[:space:]]+was[[:space:]]+renewed",
          block, ignore.case = TRUE, perl = TRUE)
  if (!is_note) return(text)
  keep <- seq_len(h - 1)
  while (length(keep) && !nzchar(trimws(lines[keep[length(keep)]]))) keep <- keep[-length(keep)]
  paste(lines[keep], collapse = "\n")
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
#' Three boilerplate shapes are *not* bracketed by starred markers, and were
#' missed by an earlier version that required them:
#'
#'   * an unstarred footer, "end of the project gutenberg ebook of <title>,
#'     by <author>", which sits one line ABOVE the starred end marker in 37
#'     books (e.g. id 2727). Cutting at the starred line kept it, so the last
#'     line of those bodies named the title and the author.
#'   * a possessive footer without "ebook", "end of project gutenberg's
#'     <title>, by <author>", in 11 books.
#'   * a production credit, "produced by ...", placed AFTER the start marker
#'     rather than before it (id 2883), which put a Gutenberg line at the head
#'     of the kept body. 19 books carry such a credit.
#'
#' All three are treated as boilerplate here. Text that merely mentions
#' Project Gutenberg mid-sentence or mid-line is left alone -- the patterns are
#' anchored to the start of a line, which is what distinguishes the two.
#'
#' @param text a single string containing one book's full text
#' @returns the same text with header/footer boilerplate removed
#' @export
stripGutenbergBoilerplate <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]

  start_pat <- "^[[:space:]]*\\*+[[:space:]]*start of (this |the )?project gutenberg ebook"
  end_pat   <- "^[[:space:]]*(\\*+[[:space:]]*)?end of (this |the )?project gutenberg"

  start_idx <- which(grepl(start_pat, lines, ignore.case = TRUE, perl = TRUE))
  end_idx   <- which(grepl(end_pat, lines, ignore.case = TRUE, perl = TRUE))

  body_start <- if (length(start_idx) > 0) max(start_idx) + 1 else 1
  body_end   <- if (length(end_idx) > 0) min(end_idx) - 1 else length(lines)

  # Drop machine-generated lines at the head of the body. The book's own
  # title page ("the gold bag", "by carolyn wells") sits below these and is
  # deliberately kept: it is part of the scanned book, not of the licence.
  #
  # Bounded to the header region: "produced by" is also ordinary English and
  # opens real sentences elsewhere in the corpus (id 13720, "produced by dead
  # matter therein..."; id 19513, "produced by the noise of falling waters").
  # A Gutenberg credit only ever sits in the header, so position is the
  # discriminator. Only a *leading run* of credit lines is consumed -- skipping
  # to the last credit in a window would take real text with it.
  credit_pat <- "^[[:space:]]*(produced by |and the online distributed proofreading team|the project gutenberg ebook of )"
  is_credit <- function(i) grepl(credit_pat, lines[i], ignore.case = TRUE, perl = TRUE)

  if (body_start <= body_end) {
    first_text <- body_start
    while (first_text <= body_end && !nzchar(trimws(lines[first_text]))) {
      first_text <- first_text + 1
    }
    if (first_text <= body_end && is_credit(first_text)) {
      body_start <- first_text
      while (body_start <= body_end) {
        if (!nzchar(trimws(lines[body_start])) || is_credit(body_start)) {
          body_start <- body_start + 1
        } else {
          break
        }
      }
    }
  }

  if (body_start > body_end) return("")  # defensive: malformed/unexpected marker order

  paste(lines[body_start:body_end], collapse = "\n")
}

#' Restore the rows the 2013 metadata lost
#'
#' `novel_meta.txt` breaks off inside the line for 23177 and has none for
#' 23178, the archive's 800th file, though both texts are in the archive.
#' `inst/extdata/recovered_rows.csv` supplies them: genre, fold and class
#' from the file's folder, the count from the Wayback Machine's capture of
#' the book's page nearest the scrape (5 November 2012; PREREGISTRATION.md,
#' amendment of 2026-11-25). Title, author and language come from the
#' catalogue join, as for every other row.
#'
#' Each row is checked against the archive before it is added: its id is
#' not already in the metadata, its file is where the table says, the folder
#' names its class and fold (`success5`, `ss_fold5`), and the genre that
#' every book already parsed from the same genre folder shares is the row's.
#'
#' @param novel_metadata the parsed metadata (`parseMetaData()$data`)
#' @param dataset_path the unpacked archive
#' @param recovered the table, as read by `readRecoveredRows()`
#' @returns `novel_metadata` with the recovered rows appended
#' @noRd
recoverLostRows <- function(novel_metadata, dataset_path, recovered) {
  needed <- c("gutenberg_id", "genre", "fold", "response", "download_count", "path")
  missing <- setdiff(needed, names(recovered))
  if (length(missing)) {
    stop("The recovered-rows table lacks column(s) ", paste(missing, collapse = ", "),
         ". See inst/extdata/recovered_rows.csv.", call. = FALSE)
  }
  if (!nrow(recovered)) return(novel_metadata)
  fail <- function(i, why) {
    stop(sprintf("Recovered row %s: %s. Fix inst/extdata/recovered_rows.csv.",
                 recovered$gutenberg_id[i], why), call. = FALSE)
  }
  ids_by_folder <- function(folder) {
    files <- list.files(file.path(dataset_path, folder), pattern = "[0-9]+\\.txt$",
                        recursive = TRUE)
    as.integer(sub("\\.txt$", "", basename(files)))
  }
  for (i in seq_len(nrow(recovered))) {
    r <- recovered[i, ]
    if (r$gutenberg_id %in% novel_metadata$gutenberg_id) fail(i, "the metadata already has it")
    if (!file.exists(file.path(dataset_path, r$path))) fail(i, paste("no file at", r$path))
    parts <- strsplit(r$path, "/", fixed = TRUE)[[1]]
    if (length(parts) != 4) fail(i, "path is not genre/fold/class/file")
    class_dir <- paste0(if (r$response == 1L) "success" else "failure", r$fold)
    if (!identical(parts[3], class_dir)) {
      fail(i, sprintf("its folder is %s, but the table says %s", parts[3], class_dir))
    }
    if (!endsWith(parts[2], paste0("_fold", r$fold))) {
      fail(i, sprintf("its folder is %s, but the table says fold %d", parts[2], r$fold))
    }
    # A book in several genres has a copy in each genre's folder, so the
    # folder's genre is the one every parsed book in it shares.
    in_folder <- novel_metadata[novel_metadata$gutenberg_id %in% ids_by_folder(parts[1]), ]
    shared <- Reduce(intersect, split(as.character(in_folder$genre), in_folder$gutenberg_id))
    if (!identical(shared, as.character(r$genre))) {
      fail(i, sprintf("the books in %s share the genre %s, but the table says %s", parts[1],
                      if (length(shared)) paste(shared, collapse = "/") else "(none)", r$genre))
    }
  }
  added <- tibble::tibble(
    response = as.integer(recovered$response),
    download_count = as.integer(recovered$download_count),
    genre = factor(recovered$genre, levels = levels(novel_metadata$genre)),
    fold = as.integer(recovered$fold),
    gutenberg_id = as.integer(recovered$gutenberg_id)
  )
  if (anyNA(added$genre)) {
    stop("A recovered row's genre is not one of the corpus's: ",
         paste(setdiff(recovered$genre, levels(novel_metadata$genre)), collapse = ", "),
         ".", call. = FALSE)
  }
  dplyr::bind_rows(novel_metadata, added[names(novel_metadata)])
}

#' Read the recovered-rows table
#'
#' @param path the table; by default the installed copy, else the source tree's
#' @returns a data frame, one row per recovered book
#' @noRd
readRecoveredRows <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("extdata", "recovered_rows.csv", package = "novels")
    if (!nzchar(path)) path <- findInSourceTree(file.path("inst", "extdata", "recovered_rows.csv"))
  }
  if (!nzchar(path) || !file.exists(path)) {
    stop("inst/extdata/recovered_rows.csv not found; run from the package root or ",
         "install the package.", call. = FALSE)
  }
  as.data.frame(readr::read_csv(path, col_types = readr::cols(
    gutenberg_id = "i", fold = "i", response = "i", download_count = "i",
    .default = "c")))
}
