#' Title fetchDataSet
#'
#' Retrieves and unpacks the "golden" source corpus archive, refusing to use
#' it unless its SHA-256 matches the pinned digest. The URL is configuration
#' (the `NOVELS_ARCHIVE_URL` environment variable, see `.env.example`) so the
#' archive may move -- from Backblaze to HuggingFace, say -- without touching
#' package code. The digest is deliberately a literal in `_targets.R`, not an
#' environment variable: `targets` does not hash environment variables, so a
#' changed URL would be invisible to the dependency graph. The digest is what
#' makes switched bytes an error instead of a silent new result.
#'
#' @param data_path directory into which the source corpus is downloaded and
#'   unpacked (default "data-raw")
#' @param archive_url location of the 7z archive, defaulting to the
#'   `NOVELS_ARCHIVE_URL` environment variable
#' @param archive_sha256 expected SHA-256 of the archive, pinned in `_targets.R`
#' @returns relative path to the dataset
#' @seealso verify_sha256
#' @export
fetchRawData <- function(data_path = 'data-raw',
                         archive_url = Sys.getenv("NOVELS_ARCHIVE_URL"),
                         archive_sha256) {
  tar_assert_true(dir.exists(data_path))
  tar_assert_true(
    nzchar(archive_url),
    "NOVELS_ARCHIVE_URL is not set -- copy .env.example to .env and fill it in"
  )
  tar_assert_true(
    nzchar(archive_sha256),
    "archive_sha256 must be pinned in _targets.R, never read from the environment"
  )

  dataset_path <- file.path(data_path, 'novels')
  if (!file.exists(dataset_path)) {
    tmp <- tempfile(fileext = ".7z")
    on.exit(unlink(tmp), add = TRUE)
    download.file(archive_url, tmp, quiet = TRUE, mode = "wb")
    verify_sha256(tmp, archive_sha256, what = "source archive")
    archive_extract(tmp, data_path)
  }
  tar_assert_true(file.exists(dataset_path), "raw dataset retrieval failed.")
  dataset_path
}

#' Verify a file's SHA-256 against an expected value
#'
#' Used by fetchRawData() to pin the source archive. A mismatch aborts, and
#' the error names both digests so a legitimately moved or re-cut archive can
#' be told apart from a corrupted download.
#'
#' @param path path to the file to check
#' @param expected expected SHA-256, hex, case-insensitive
#' @param what label for the file in the error message
#' @returns the actual digest, invisibly
#' @keywords internal
verify_sha256 <- function(path, expected, what = "file") {
  actual <- digest::digest(path, algo = "sha256", file = TRUE)
  tar_assert_true(
    tolower(actual) == tolower(expected),
    sprintf(
      "%s failed SHA-256 verification\n  expected: %s\n  actual:   %s\n  file:     %s",
      what, expected, actual, path
    )
  )
  invisible(actual)
}


parseMetaData <- function(dataset_path) {
  meta_path <- file.path(dataset_path, "novel_meta.txt")
  lines <- readr::read_lines(meta_path)          # will warn: incomplete final line

  is_record   <- str_detect(lines, "^(SUCCESS|FAILURE):")
  is_complete <- str_detect(lines, "DownloadCount:")
  is_trunc    <- is_record & !is_complete

  # Header census -- structure, not defect
  tar_assert_identical(sum(str_detect(lines, "^\\*+DATASET:")),           8L)
  tar_assert_identical(sum(str_detect(lines, "^\\*+\\s*FOLD\\d+")),       40L)

  lines_clean <- lines[!(str_detect(lines, "^(SUCCESS|FAILURE)") & !is_complete)]

  # SUCCESS: FileName: 103.txt ,Title: around the world in 80 days ,Author: "verne, jules, 1828-1905" ,Language: en ,DownloadCount: 3260
  novel_metadata <- tibble(raw = lines_clean) |>
    mutate(
      genre = if_else(
        str_detect(raw, "^\\*+DATASET:"),
        str_trim(str_extract(raw, "(?<=DATASET:)[^*]+")),
        NA_character_
      ),
      fold = if_else(
        str_detect(raw, "^\\*+\\s*FOLD\\d+"),
        str_extract(raw, "\\d+"),
        NA_character_
      ),
      is_record = str_detect(raw, "^(SUCCESS|FAILURE)")
    ) |>
    fill(genre, fold, .direction = "down") |>
    filter(is_record) |>
    select(-is_record) |>
    separate_wider_regex(
      raw,
      patterns = c(
        response = "^(?:SUCCESS|FAILURE)",
        ":\\s*FileName:\\s+",
        id = "[0-9]+",
        "\\.txt\\s+,Title:.*?,DownloadCount:\\s+",
        download_count = "[0-9]+",
        ".*$"
      )
    ) |>
    mutate(
      response = as.integer(str_equal(response, "SUCCESS")),
      gutenberg_id = as.integer(id),
      fold = as.integer(fold),
      download_count = as.integer(download_count),
    ) |>
    select(-id)

  # Independent witness: the directory tree
  fs <- list.files(dataset_path, pattern = "[0-9]+\\.txt$",
                   recursive = TRUE, full.names = TRUE)
  ids_fs <- as.integer(sub("\\.txt$", "", basename(fs)))
  absent <- setdiff(ids_fs, novel_metadata$gutenberg_id)

  tar_assert_true(
    nrow(novel_metadata) + sum(ids_fs %in% absent) == length(ids_fs),
    sprintf(
      "file conservation failed: %d parsed rows + %d absent-file rows = %d, but %d files on disk. absent = {%s}",
      nrow(novel_metadata), sum(ids_fs %in% absent),
      nrow(novel_metadata) + sum(ids_fs %in% absent),
      length(ids_fs), paste(absent, collapse = ", ")
    )
  )

  novel_metadata$genre <- novel_metadata$genre |>
    str_replace_all("Love Story", "Romance") |>
    str_replace_all("Sci-Fi", "Science Fiction")

  novel_metadata$genre <- as.factor(novel_metadata$genre)

  list(
    data   = novel_metadata,   # tibble, unchanged content
    census = list(
      n_blocks    = sum(is_record),
      n_parseable = nrow(novel_metadata),
      n_files     = length(ids_fs),
      truncated   = lines[is_trunc],
      absent      = absent,
      ts          = Sys.time()
    )
  )
}

XparseMetaData <- function(dataset_path) {
  meta_data_file_path <- file.path(dataset_path, 'novel_meta.txt')

  lines <- read_lines(meta_data_file_path)

  is_complete <- str_detect(lines, "DownloadCount")
  n_incomplete <- sum(str_detect(lines, "^(SUCCESS|FAILURE)") & !is_complete)

  # should be exactly 1 given what we've now confirmed
  tar_assert_true(
    n_incomplete == 1,
    paste("Expected exactly 1 truncated trailing record, found", n_incomplete)
  )

  lines_clean <- lines[!(str_detect(lines, "^(SUCCESS|FAILURE)") & !is_complete)]

  # SUCCESS: FileName: 103.txt ,Title: around the world in 80 days ,Author: "verne, jules, 1828-1905" ,Language: en ,DownloadCount: 3260
  novel_metadata <- tibble(raw = lines_clean) |>
    mutate(
      genre = if_else(
        str_detect(raw, "^\\*+DATASET:"),
        str_trim(str_extract(raw, "(?<=DATASET:)[^*]+")),
        NA_character_
      ),
      fold = if_else(
        str_detect(raw, "^\\*+\\s*FOLD\\d+"),
        str_extract(raw, "\\d+"),
        NA_character_
      ),
      is_record = str_detect(raw, "^(SUCCESS|FAILURE)")
    ) |>
    fill(genre, fold, .direction = "down") |>
    filter(is_record) |>
    select(-is_record) |>
    separate_wider_regex(
      raw,
      patterns = c(
        response = "^(?:SUCCESS|FAILURE)",
        ":\\s*FileName:\\s+",
        id = "[0-9]+",
        "\\.txt\\s+,Title:.*?,DownloadCount:\\s+",
        download_count = "[0-9]+",
        ".*$"
      )
    ) |>
    mutate(
      response = as.integer(str_equal(response, "SUCCESS")),
      gutenberg_id = as.integer(id),
      fold = as.integer(fold),
      download_count = as.integer(download_count),
    ) |>
    select(-id)

  novel_metadata$genre <- novel_metadata$genre |>
    str_replace_all("Love Story", "Romance") |>
    str_replace_all("Sci-Fi", "Science Fiction")

  novel_metadata$genre <- as.factor(novel_metadata$genre)

  novel_metadata
}

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

#' Title buildDtm
#'
#' Uses getIterator() (the same tokenization -- lowercasing + word_tokenizer
#' -- that produced the vocabulary this vectorizer was built from) rather
#' than a plain text2vec::itoken() call, so vocabulary-building and
#' dtm-building always tokenize consistently.
#'
#' @param df a data frame with text and title columns
#' @param vectorizer a text2vec vectorizer, from buildVectorizer()
#' @returns a document-term matrix
#' @export
buildDtm <- function(df, vectorizer) {
  it <- getIterator(df$text, df$title)
  text2vec::create_dtm(it, vectorizer)
}

#' Title makeFoldid
#'
#' Precomputes a single, fixed CV fold assignment to be shared across every
#' alpha in the sweep. cv.glmnet draws its own random inner-CV folds by
#' default, and that draw is not reliably pinned by a set.seed() call
#' elsewhere in the pipeline (R's RNG stream just keeps advancing between
#' targets) -- so without this, each alpha's model could be scored against a
#' *different* fold assignment, adding noise on top of alpha's real effect.
#'
#' Grouped by author, not assigned per row: without this, a prolific
#' author's books can land in both cv.glmnet's inner-training and inner-
#' validation partitions simultaneously, letting lambda selection partially
#' fit to that author's own style rather than to genuinely held-out data --
#' the same leakage risk createSplit()/createKFoldSplit() already guard
#' against for the outer split, just previously left unguarded for the
#' inner one. Authors are assigned to folds first, then every row inherits
#' its author's fold, so no author ever spans more than one fold here.
#'
#' @param author character vector of authors, one per training row, in the
#'   same row order as the DTM this foldid will be used against
#' @param nfolds number of CV folds
#' @param seed random seed
#' @returns an integer vector of fold assignments, length length(author)
#' @export
makeFoldid <- function(author, nfolds = 5, seed = 42) {
  set.seed(seed)
  unique_authors <- unique(author)
  author_fold <- sample(rep(1:nfolds, length.out = length(unique_authors)))
  names(author_fold) <- unique_authors
  unname(author_fold[author])
}

#' Title fitGlmnetModel
#'
#' @param dtm_train TF-IDF-weighted training document-term matrix
#' @param response integer 0/1 response vector, same row order as dtm_train
#' @param alpha glmnet elastic net mixing parameter (0 = ridge, 1 = lasso)
#' @param foldid fixed CV fold assignment, from makeFoldid(), shared across
#'   every alpha so results are comparable to each other
#' @returns a cv.glmnet fit object
#' @export
fitGlmnetModel <- function(dtm_train, response, alpha, foldid) {
  tar_assert_true(all(response %in% c(0, 1)))
  glmnet::cv.glmnet(
    dtm_train, response,
    family = "binomial",
    alpha = alpha,
    foldid = foldid,
    # cv.glmnet's own default (type.measure = "deviance") would pick
    # lambda.min by minimizing binomial deviance, not by the AUC that
    # evaluateModel() reports -- explicit here so lambda selection and
    # reported performance are judged by the same criterion.
    type.measure = "auc",
    control = list(thresh = 1e-5, maxit = 1e4)
  )
}

#' Title evaluateModel
#'
#' Generates predictions from a fitted model against a given DTM, and scores
#' them with AUC. Predictions and response must be freshly computed together
#' here so that they cannot become misaligned by stale objects, which is the
#' precise class of bug that motivated moving this into targets.
#'
#' @param fit a cv.glmnet fit, from fitGlmnetModel()
#' @param dtm a TF-IDF-weighted document-term matrix to score against
#' @param response integer 0/1 response vector, same row order as dtm
#' @param alpha glmnet elastic net mixing parameter this fit was produced with
#' @returns a one-row tibble: alpha, auc, n_nonzero_coef
#' @export
evaluateModel <- function(fit, dtm, response, alpha) {
  pred <- as.numeric(predict(fit, newx = dtm, s = "lambda.min", type = "response"))
  stopifnot(length(pred) == length(response))

  auc <- as.numeric(pROC::auc(pROC::roc(response, pred, quiet = TRUE)))
  n_nonzero <- sum(coef(fit, s = "lambda.min") != 0)

  tibble::tibble(alpha = alpha, auc = auc, n_nonzero_coef = n_nonzero)
}

#' Title fitTfidf
#'
#' Fits TF-IDF weighting on a training document-term matrix. Returns the
#' fitted model itself (not just the transformed matrix), so that this
#' target's own stored value captures the fitted state -- any number of
#' downstream targets can then depend on it and correctly transform other
#' datasets (test, validation, future inference batches) with the same
#' fitted weights, without re-fitting.
#'
#' @param dtm_train_raw raw (non-weighted) training document-term matrix
#' @returns a list with the fitted `model` and the weighted training `dtm`
#' @export
fitTfidf <- function(dtm_train_raw) {
  model <- text2vec::TfIdf$new()
  list(
    model = model,
    dtm = model$fit_transform(dtm_train_raw)
  )
}

#' Title transformTfidf
#'
#' Applies an already-fitted TF-IDF model to a new raw document-term matrix.
#' Never fits -- takes the fitted model as an explicit argument, so the
#' dependency on "the model that was actually fitted" is real and tracked,
#' rather than relying on a shared mutable object that happens to have been
#' fitted somewhere else.
#'
#' @param tfidf_fit the list returned by fitTfidf()
#' @param dtm_raw raw (non-weighted) document-term matrix to transform
#' @returns the TF-IDF-weighted matrix
#' @export
transformTfidf <- function(tfidf_fit, dtm_raw) {
  tfidf_fit$model$transform(dtm_raw)
}

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
#' Replaces the text of known-contaminated ids with a real text edition,
#' fetched from Gutenberg via gutenbergr. The replacement is downloaded under
#' a *different* id (see `replacements`), because the contaminated id is the
#' audio recording itself. The contaminated id stays the row's key, so folds,
#' labels and metadata are untouched and only the text changes. Replacement
#' text is lowercased to match the rest of the corpus. The original archive
#' (fetchRawData()) is left untouched -- this correction is applied as an
#' explicit step here, so its provenance stays visible in code and git
#' history rather than baked into a binary blob.
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
  downloaded <- gutenbergr::gutenberg_download(source_ids, meta_fields = character(0))

  missing <- setdiff(source_ids, downloaded$gutenberg_id)
  if (length(missing) > 0) {
    stop("Gutenberg returned no text for replacement id(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  replacement_text <- function(source_id) {
    downloaded$text[downloaded$gutenberg_id == source_id] |>
      paste(collapse = "\n") |>
      stringi::stri_trans_tolower()
  }

  df$text[match(affected, as.character(df$gutenberg_id))] <-
    purrr::map_chr(source_ids, replacement_text)

  message(sprintf("patchContaminatedText: replaced text for %d id(s): %s",
                  length(affected), paste(affected, collapse = ", ")))
  df
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
