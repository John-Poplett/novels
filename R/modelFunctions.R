# Feature matrices, fold ids, model fitting and evaluation.


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
  author_fold <- assignAuthorFolds(unique(author), nfolds, seed)
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
