# Assembling the shipped corpus from metadata and text files.


tidyRawData <- function(dataset_path, parsed_metadata) {
  novel_metadata <- parsed_metadata$data

  files <- list.files(path = dataset_path, pattern = "[0-9]+\\.txt", recursive = TRUE)

  df <- data.frame(text = files) %>% mutate(
    gutenberg_id = as.integer(sub(".*?([0-9]+)\\.txt.*$", "\\1", text)),
    text = as.character(text)
  )

  # The corpus physically ships a copy of each text file inside every
  # genre-directory that book belongs to, so list.files() can return more
  # than one row per gutenberg_id. novel_metadata already carries the
  # legitimate one-row-per-genre expansion; df should carry exactly one row
  # per unique book, or the join below cross-multiplies mismatched
  # genre/fold combinations together.
  n_files_before <- nrow(df)
  df <- df |> distinct(gutenberg_id, .keep_all = TRUE)
  n_dropped <- n_files_before - nrow(df)
  if (n_dropped > 0) {
    message(sprintf(
      "tidyRawData: dropped %d duplicate genre-directory copies of already-seen books",
      n_dropped
    ))
  }

  #
  # Merge in metadata from gutenbergr, including gutenberg_bookshelf --
  # kept deliberately (not stripped) for the genre-mapping/corpus-
  # expansion work, which reads it directly (see assignGenre.R,
  # compareGenreAssignment.R).
  #

  novel_metadata <- dplyr::inner_join(novel_metadata, gutenbergr::gutenberg_metadata, by = c("gutenberg_id"))

  novel_metadata <- novel_metadata |>
    group_by(gutenberg_id, genre, fold) |>
    summarize(
      author = paste(author, collapse = "; "),
      gutenberg_author_id = paste(gutenberg_author_id, collapse = "; "),
      across(-c(author, gutenberg_author_id), first),
      .groups = "drop"
    )

  # NOTE: this is guaranteed to pass by construction, since the
  # group_by(gutenberg_id, genre, fold) |> summarize(...) step above already
  # collapses to one row per combination. Kept as a regression guard in case
  # that step is ever changed.
  is_distinct <- function(df, columns_to_group = c("gutenberg_id", "genre", "fold")) {
    nrow(df) == nrow(dplyr::distinct(df, dplyr::across(dplyr::all_of(columns_to_group))))
  }

  tar_assert_true(
    is_distinct(novel_metadata, c("gutenberg_id", "genre", "fold")),
    "Duplicate (gutenberg_id, genre, fold) combination found -- check join for fan-out"
  )

  #
  # Now load up the relevant text
  #
  df$text <- sapply(df$text, function(x) stringi::stri_enc_toutf8(readr::read_file(file.path(dataset_path, x))))

  #
  # A handful of books in the original corpus have their gutenberg_id
  # pointing at a LibriVox audio recording's catalog entry rather than the
  # text edition's -- confirmed via full-corpus grep for "librivox", which
  # cannot appear in genuine prose predating 2005. Applied once per unique
  # book here (df is still deduped, pre-join), not per genre row.
  contaminated_ids <- getLibrivoxContaminatedIds(df)

  tar_assert_true(
    setequal(contaminated_ids, c(as.integer(names(librivox_replacements)), librivox_drops)),
    "Set of librivox-contaminated ids differs from librivox_replacements + librivox_drops -- corpus may have changed"
  )

  df <- dropByGutenbergId(df, librivox_drops)
  novel_metadata <- dropByGutenbergId(novel_metadata, librivox_drops)
  df <- patchContaminatedText(df, librivox_replacements)

  # Check the outcome, not just the input: the baseline assertion above ran
  # before patching, so a patch that changed nothing used to pass silently.
  tar_assert_true(
    length(getLibrivoxContaminatedIds(df)) == 0,
    "librivox text remains after patchContaminatedText()"
  )

  # Strip Gutenberg's license header/footer boilerplate uniformly. A
  # full-corpus grep found this in at least 19 of 798 books -- far more
  # widespread than initially assumed -- so it's applied to every book
  # rather than deferred.
  df$text <- sapply(df$text, stripGutenbergBoilerplate)

  tar_assert_true(
    !any(grepl("\\*\\*\\* ?(START|END) OF", df$text, ignore.case = TRUE)),
    "Gutenberg boilerplate markers still present after stripping"
  )

  df <- dplyr::inner_join(df, novel_metadata, by = c("gutenberg_id"))
  df <- applyTitleCorrections(df, librivox_title_corrections)

  tar_assert_true(
    nrow(df) == nrow(novel_metadata),
    sprintf("Row count mismatch after joining df to novel_metadata: expected %d, got %d",
            nrow(novel_metadata), nrow(df))
  )

  tar_assert_true(!any(is.na(df$download_count)),
                  "rows missing download count values")

  # Convert to tibble
  novels <- as_tibble(df)

  names(novels) <- names(novels) %>% stri_enc_toutf8

  novels
}

#' Title writeNovelsData
#'
#' Saves the dataset to the package's data directory, so the pipeline is the
#' single way the shipped dataset is produced. Returns the path, for use with
#' a targets `format = "file"` target.
#'
#' @param novels the tidied dataset from tidyRawData()
#' @param path where to write it
#' @returns path
#' @noRd
writeNovelsData <- function(novels, path = "data/novels.rda") {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  save(novels, file = path, compress = "bzip2")
  path
}
