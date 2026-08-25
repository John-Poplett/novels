# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  tidy_eval = FALSE,
  packages = c("dplyr", "readr", "usethis", "tibble", "stringi",
  "stringr", "purrr", "tibble", "tidyr", "assertthat", "gutenbergr", "archive",
  "text2vec", "glmnet", "pROC", "tm")
  # format = "qs", # Optionally set the default storage format. qs is fast.
 )

# Alpha values to sweep for the glmnet elastic net mixing parameter.
# 0 = pure ridge, 1 = pure lasso.
alpha_grid <- tibble::tibble(alpha = c(0.01, 0.03, 0.05, 0.07, 0.1, 0.3, 0.5, 0.7, 0.9, 1.0))

# Run the R scripts in the R/ folder with your custom functions:
tar_source("R")

# Replace the target list below with your own:
list(
  tar_target(rawdataset_path, fetchRawData("data-raw")),
  tar_target(novel_metadata, parseMetaData(rawdataset_path)),
  tar_target(novels, tidyRawData(rawdataset_path, novel_metadata)),

  # Author-level train/test split (no author appears in both sets).
  tar_target(split_indices, createSplit(novels)),
  tar_target(train_data, novels[split_indices$train, ]),
  tar_target(test_data, novels[split_indices$test, ]),

  # Vectorizer is fit on training text only, then reused (never refit) to
  # transform the test text, so no test-set vocabulary leaks into training.
  tar_target(vectorizer, buildVectorizer(train_data, ngram = c(1, 1))),
  tar_target(dtm_train_raw, buildDtm(train_data, vectorizer)),
  tar_target(dtm_test_raw, buildDtm(test_data, vectorizer)),

  # TF-IDF weighting: fitting is its own target, so the fitted model itself
  # -- not just a transformed matrix -- is what gets stored. dtm_test depends
  # explicitly on tfidf_fit, so it's guaranteed to transform against the
  # actual fitted weights. A validation set or new inference batch could
  # later reuse tfidf_fit the same way, via its own transformTfidf() target.
  tar_target(tfidf_fit, fitTfidf(dtm_train_raw)),
  tar_target(dtm_train, tfidf_fit$dtm),
  tar_target(dtm_test, transformTfidf(tfidf_fit, dtm_test_raw)),

  # Fixed CV fold assignment, shared across every alpha branch -- see
  # makeFoldid()'s comment for why this can't just be left to cv.glmnet's
  # own default random fold draw.
  tar_target(cv_foldid, makeFoldid(nrow(dtm_train), nfolds = 5)),

  # Branch over alpha: each value gets its own fully independent fit,
  # predictions, and evaluation, so there is no possibility of an alpha=0.1
  # model's predictions being compared against an alpha=0.5 model's labels
  # or vice versa.
  tar_map(
    values = alpha_grid,
    names = alpha,

    tar_target(model_fit, fitGlmnetModel(dtm_train, train_data$response, alpha, cv_foldid)),

    tar_target(
      eval_train,
      evaluateModel(model_fit, dtm_train, train_data$response, alpha)
    ),
    tar_target(
      eval_test,
      evaluateModel(model_fit, dtm_test, test_data$response, alpha)
    )
  ),

  # Combine every branch's evaluation into one summary tibble.
  #
  # IMPORTANT: run `tar_manifest()` (or `tar_visnetwork()`) first, after
  # adding the tar_map() block above, to see the actual generated target
  # names -- tar_map() derives them from the alpha values (e.g. eval_test_0.1,
  # eval_test_0.3, ...), and the exact formatting of names like 1.0 (may come
  # out as "eval_test_1" rather than "eval_test_1.0") needs to be confirmed
  # before writing the line below. Referencing each branch by its bare name
  # here is enough for targets' static code analysis to wire up the
  # dependency correctly -- no tar_combine() needed. Adjust the names to
  # match whatever tar_manifest() actually shows.
  tar_target(
    alpha_sweep_summary,
    dplyr::bind_rows(
      eval_test_0.1, eval_test_0.3, eval_test_0.5,
      eval_test_0.7, eval_test_0.9, eval_test_1
    )
  )
 )
