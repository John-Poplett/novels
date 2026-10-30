# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Local configuration (see .env.example). targets does not hash environment
# variables, so NOVELS_ARCHIVE_URL is captured as its own target below --
# visible in the graph -- while the archive's SHA-256 is pinned in code.
# A moved or re-cut archive then fails verification instead of silently
# producing a different dataset.
if (file.exists(".env")) readRenviron(".env")

# Set target options:
tar_option_set(
  tidy_eval = FALSE,
  packages = c("dplyr", "readr", "usethis", "tibble", "stringi",
  "stringr", "purrr", "tidyr", "assertthat", "gutenbergr", "archive",
  "text2vec", "glmnet", "pROC", "tm")
  # format = "qs", # Optionally set the default storage format. qs is fast.
 )

# Alpha values to sweep for the glmnet elastic net mixing parameter.
# 0 = pure ridge, 1 = pure lasso.
alpha_grid <- tibble::tibble(alpha = c(0.01, 0.03, 0.05, 0.07, 0.1, 0.3, 0.5, 0.7, 0.9, 1.0))

# Number of author-safe folds for repeated cross-validation. A single
# 80/20 split's AUC estimate has a standard error of roughly 0.038 at this
# corpus's test-set size (see README) -- repeating across several
# independent splits and looking at mean/SD/range per alpha is the
# practical fix, without needing a larger corpus first.
k_folds <- 5

# Run the R scripts in the R/ folder with your custom functions:
tar_source("R")

# --- Build per-fold and per-(fold, alpha) targets programmatically ---
#
# This uses the same base-R metaprogramming approach as
# cv_results_command/cv_summary_command below (as.symbol/as.call/bquote),
# rather than a second tar_map() dimension layered on the existing alpha
# one: referencing "this fold's train_data" from the modeling step needs a
# literal, verifiable symbol reference in each target's command, and
# that's most reliably built explicitly rather than depending on
# tar_map()'s cross-branch substitution behavior for a case this shaped.

# Per-fold data prep: train/test split, dedup, vectorizer (unigrams only
# -- see README's ngram isolation test), DTMs, TF-IDF fit, and this
# fold's own fixed inner-CV foldid (shared across every alpha *within*
# this fold, same reasoning as the single-split version's cv_foldid).
fold_prep_targets <- unlist(lapply(seq_len(k_folds), function(k) {
  list(
    tar_target_raw(
      paste0("train_data_raw_", k),
      bquote(novels[fold_assignments[[.(k)]]$train, ])
    ),
    tar_target_raw(
      paste0("test_data_raw_", k),
      bquote(novels[fold_assignments[[.(k)]]$test, ])
    ),
    # Deduped by gutenberg_id, scoped to *this fold*, *after* the split --
    # never globally, before splitting. A book belonging to more than one
    # genre otherwise contributes multiple identical-text rows to the same
    # fold (author-based splitting guarantees a book's genre-duplicates
    # always land together), double-weighting its response in the model
    # fit and skewing the vectorizer's document-frequency-based pruning
    # thresholds. Doing this per-fold, post-split, keeps a book's full
    # multi-genre membership intact in `novels`/`fold_assignments` for
    # anything else that still needs it (genre-mapping work, any future
    # per-genre reporting) -- only "how many times does this text get fed
    # into this fold's model" is affected, not the corpus's own structure.
    tar_target_raw(
      paste0("train_data_", k),
      bquote(dedupe_by_group(.(as.symbol(paste0("train_data_raw_", k))), "gutenberg_id"))
    ),
    tar_target_raw(
      paste0("test_data_", k),
      bquote(dedupe_by_group(.(as.symbol(paste0("test_data_raw_", k))), "gutenberg_id"))
    ),
    tar_target_raw(
      paste0("vectorizer_", k),
      bquote(buildVectorizer(.(as.symbol(paste0("train_data_", k))), ngram = c(1L, 1L)))
    ),
    tar_target_raw(
      paste0("dtm_train_raw_", k),
      bquote(buildDtm(.(as.symbol(paste0("train_data_", k))), .(as.symbol(paste0("vectorizer_", k)))))
    ),
    tar_target_raw(
      paste0("dtm_test_raw_", k),
      bquote(buildDtm(.(as.symbol(paste0("test_data_", k))), .(as.symbol(paste0("vectorizer_", k)))))
    ),
    tar_target_raw(
      paste0("tfidf_fit_", k),
      bquote(fitTfidf(.(as.symbol(paste0("dtm_train_raw_", k)))))
    ),
    tar_target_raw(
      paste0("dtm_train_", k),
      bquote(.(as.symbol(paste0("tfidf_fit_", k)))$dtm)
    ),
    tar_target_raw(
      paste0("dtm_test_", k),
      bquote(transformTfidf(.(as.symbol(paste0("tfidf_fit_", k))), .(as.symbol(paste0("dtm_test_raw_", k)))))
    ),
    tar_target_raw(
      paste0("cv_foldid_", k),
      bquote(makeFoldid(.(as.symbol(paste0("train_data_", k)))$author, nfolds = 5))
    )
  )
}), recursive = FALSE)

# Per-(fold, alpha) model fit and evaluation.
cv_grid <- tidyr::expand_grid(fold = seq_len(k_folds), alpha = alpha_grid$alpha)

model_targets <- unlist(lapply(seq_len(nrow(cv_grid)), function(i) {
  k <- cv_grid$fold[i]
  a <- cv_grid$alpha[i]
  suffix <- paste0(k, "_", a)

  dtm_train_sym  <- as.symbol(paste0("dtm_train_", k))
  dtm_test_sym   <- as.symbol(paste0("dtm_test_", k))
  train_data_sym <- as.symbol(paste0("train_data_", k))
  test_data_sym  <- as.symbol(paste0("test_data_", k))
  cv_foldid_sym  <- as.symbol(paste0("cv_foldid_", k))
  model_fit_sym  <- as.symbol(paste0("model_fit_", suffix))

  list(
    tar_target_raw(
      paste0("model_fit_", suffix),
      bquote(fitGlmnetModel(.(dtm_train_sym), .(train_data_sym)$response, .(a), .(cv_foldid_sym)))
    ),
    tar_target_raw(
      paste0("eval_train_", suffix),
      bquote(evaluateModel(.(model_fit_sym), .(dtm_train_sym), .(train_data_sym)$response, .(a)))
    ),
    tar_target_raw(
      paste0("eval_test_", suffix),
      bquote(evaluateModel(.(model_fit_sym), .(dtm_test_sym), .(test_data_sym)$response, .(a)))
    )
  )
}), recursive = FALSE)

# Combine every (fold, alpha) test-evaluation into one long tibble, then
# summarize mean/SD/min/max AUC per alpha across folds -- this is the
# actual point of the whole repeated-CV exercise: an empirical estimate of
# how much AUC varies split-to-split, not just a single point estimate.
eval_test_symbols <- lapply(seq_len(nrow(cv_grid)), function(i) {
  as.symbol(paste0("eval_test_", cv_grid$fold[i], "_", cv_grid$alpha[i]))
})
cv_results_command <- as.call(c(quote(dplyr::bind_rows), eval_test_symbols))

cv_summary_command <- quote(
  cv_results %>%
    dplyr::group_by(alpha) %>%
    dplyr::summarize(
      mean_auc = mean(auc),
      sd_auc = sd(auc),
      min_auc = min(auc),
      max_auc = max(auc),
      .groups = "drop"
    ) %>%
    dplyr::arrange(alpha)
)

# Replace the target list below with your own:
c(
  list(
    # Where the "golden" archive lives (configuration), and what it must
    # hash to (pinned). Both are targets so they appear in the dependency
    # graph; only the digest is authoritative.
    tar_target(archive_url, Sys.getenv("NOVELS_ARCHIVE_URL")),
    tar_target(
      archive_sha256,
      "f775ad52849e48800c7021f6f31eebd624fbcb998c02093d7054569d0763f03c"
    ),
    tar_target(rawdataset_path, fetchRawData("data-raw", archive_url, archive_sha256)),
    tar_target(parsed_metadata, parseMetaData(rawdataset_path)),
    tar_target(novels, tidyRawData(rawdataset_path, parsed_metadata)),

    # K author-safe folds, computed once and indexed into by every
    # per-fold target below (see createKFoldSplit()'s comment).
    tar_target(fold_assignments, createKFoldSplit(novels, k_folds))
  ),
  fold_prep_targets,
  model_targets,
  list(
    tar_target_raw("cv_results", cv_results_command),
    tar_target_raw("cv_summary", cv_summary_command)
  )
)
