#' Title fetchDataSet
#'
#' @returns relative path to the dataset
#' @export
fetchRawData <- function(data_path = 'data-raw') {
  tar_assert_true(dir.exists(data_path))
  dataset_path <- file.path(data_path, 'novels')
  if (!file.exists(dataset_path)) {
    tmp <- tempfile(fileext = ".7z")
    download.file(
      "https://f005.backblazeb2.com/file/public-johnpoplett-09477/novels.7z",
      tmp,
      quiet = TRUE
    )
    archive_extract(tmp, "data-raw")
    unlink(tmp)
  }
  tar_assert_true(file.exists(dataset_path), "raw dataset retrieval failed.")
  dataset_path
}

parseMetaData <- function(dataset_path) {
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

  novel_metadata$genre <- as.factor(novel_metadata$genre)

  novel_metadata
}

tidyRawData <- function(dataset_path, novel_metadata) {
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
  # Merge in metadata from gutenbergr. Strip out gutenberg_bookshelf column
  # since it is incomplete and not of any obvious use.
  #

  novel_metadata <- dplyr::inner_join(novel_metadata, gutenberg_metadata, by = c("gutenberg_id"))

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
    "Duplicate (gutenberg_id, genre, fold) combination found — check join for fan-out"
  )

  #
  # Now load up the relevant text
  #
  df$text <- sapply(df$text, function(x) stringi::stri_enc_toutf8(readr::read_file(file.path('data-raw', 'novels', x))))

  #
  # A handful of books in the original corpus have their gutenberg_id
  # pointing at a LibriVox audio recording's catalog entry rather than the
  # text edition's -- confirmed via full-corpus grep for "librivox", which
  # cannot appear in genuine prose predating 2005. Applied once per unique
  # book here (df is still deduped, pre-join), not per genre row.
  contaminated_ids <- getLibrivoxContaminatedIds(df)

  known_librivox_ids <- c(19513, 19640, 19678, 19782, 19836)
  tar_assert_true(
    setequal(contaminated_ids, known_librivox_ids),
    "Set of librivox-contaminated ids differs from the known baseline (see known_contamination.csv) -- corpus may have changed"
  )

  df <- patchContaminatedText(df, contaminated_ids)

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
#' @param n number of training observations
#' @param nfolds number of CV folds
#' @param seed random seed
#' @returns an integer vector of fold assignments, length n
#' @export
makeFoldid <- function(n, nfolds = 5, seed = 42) {
  set.seed(seed)
  sample(rep(1:nfolds, length.out = n))
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
  glmnet::cv.glmnet(
    dtm_train, response,
    family = "binomial",
    alpha = alpha,
    foldid = foldid,
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
#' manual grep; see known_contamination.csv for the authoritative record.
#'
#' @param df tibble with gutenberg_id and text columns, one row per book
#' @returns integer vector of contaminated gutenberg_ids
#' @export
getLibrivoxContaminatedIds <- function(df) {
  df$gutenberg_id[grepl("librivox", df$text, ignore.case = TRUE)]
}

#' Title patchContaminatedText
#'
#' Replaces text for known-contaminated ids with the real text edition,
#' fetched directly from Gutenberg via gutenbergr. The original archive
#' (fetchRawData()) is left untouched -- this correction is applied as an
#' explicit step here, so its provenance stays visible in code and git
#' history rather than baked into a binary blob.
#'
#' @param df tibble with gutenberg_id and text columns, one row per book
#' @param contaminated_ids integer vector from getLibrivoxContaminatedIds()
#' @returns df with text replaced for any contaminated_ids present
#' @export
patchContaminatedText <- function(df, contaminated_ids) {
  affected <- intersect(df$gutenberg_id, contaminated_ids)
  if (length(affected) == 0) return(df)

  replacement <- gutenbergr::gutenberg_download(affected, meta_fields = character(0))

  for (id in affected) {
    df$text[df$gutenberg_id == id] <-
      paste(replacement$text[replacement$gutenberg_id == id], collapse = "\n")
  }

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
