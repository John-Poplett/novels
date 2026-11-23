# The H2 comparison on the 2013 corpus, registered and nested
# (PREREGISTRATION.md section 11, amendment of 2026-11-23).
#
# Four models on each of the five author-grouped folds, on the same rows:
#
#   bibliographic  "the record": glm on genre + log10(text length) +
#                  log10(date_added) (+ log10(gutenberg_id) with --with-id),
#                  without publication year (absent, as section 6 allows)
#   text           TF-IDF unigrams + glmnet: the pipeline's pure-text fits
#   stacked        the record plus ONE extra column, the pure-text
#                  classifier's score. The classifier never sees the record:
#                  the record is only the yardstick it is judged against.
#                  Training books' scores are pre-validated (cv.glmnet with
#                  keep = TRUE: each book scored by a model that did not see
#                  it), so the stacking model is not fooled by over-confident
#                  in-sample scores; test books are scored by the classifier
#                  fitted on the whole training fold.
#   joint          the TF-IDF matrix and the record in one glmnet fit, record
#                  unpenalised: the draft's first design, kept for comparison
#                  (and as the vignette's "classifier given the catalogue")
#
# For text and combined, alpha is chosen per fold by inner cross-validation
# (the best cross-validated AUC at lambda.min over the pipeline's alpha
# grid), never by the outer test fold. The paired fold-level differences
# text - bibliographic (registered), stacked - bibliographic (the amended
# endpoint) and joint - bibliographic give the SDs from which section 7's
# minimum detectable effect is computed.
#
# Run from the package root after `tar_make()`:
#
#   Rscript data-raw/nested-comparison.R [--with-id] [--headings=CATALOGUE] [--out=FILE]
#
# --with-id adds log10(gutenberg_id) to the baseline, as the draft amendment
# of 2026-11-23 does to section 6. Without it, the baseline is section 6 as
# registered.
#
# --headings=CATALOGUE (a pg_catalog.csv.gz; implies --with-id) adds the
# subject headings to the record: each heading is split at " -- " into its
# parts ("England", "19th century", "Fiction"), and each part found in at
# least 3 of the fold's training books becomes a 0/1 column. There are
# hundreds, so the record itself is then a penalised glmnet fit (core
# covariates unpenalised, heading columns penalised, alpha by inner CV); the
# stacked model adds the classifier's score to it, unpenalised, and the
# joint model adds the TF-IDF columns.
#
# Reads the per-fold targets (train_data_k, test_data_k, dtm_train_k,
# dtm_test_k, cv_foldid_k, model_fit_k_alpha, eval_test_k_alpha) and
# date_added from the trial snapshot, data-raw/snapshots/2026-11-07/.

suppressMessages({
  library(dplyr)
  library(readr)
  library(targets)
})
pkgload::load_all(quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
out_path <- sub("^--out=", "", grep("^--out=", args, value = TRUE))
if (!length(out_path)) out_path <- NULL
headings_path <- sub("^--headings=", "", grep("^--headings=", args, value = TRUE))
with_headings <- length(headings_path) == 1
with_id <- "--with-id" %in% args || with_headings
if (with_headings) {
  catalogue <- readCatalogue(headings_path)
  subjects_of <- setNames(catalogue$Subjects, catalogue$`Text#`)
  catalogue_sha256 <- digest::digest(headings_path, algo = "sha256", file = TRUE)
}

# The parts of every heading of each book, as a list of character vectors.
headingParts <- function(ids) {
  raw <- subjects_of[as.character(ids)]
  if (anyNA(raw)) {
    stop(sum(is.na(raw)), " book(s) are not in the catalogue.", call. = FALSE)
  }
  lapply(raw, function(x) {
    if (!nzchar(x)) return(character())
    unique(trimws(unlist(strsplit(unlist(strsplit(x, "; ", fixed = TRUE)),
                                  " -- ", fixed = TRUE))))
  })
}

# 0/1 columns for the parts in at least `min_books` training books.
headingMatrix <- function(parts, vocabulary) {
  i <- rep(seq_along(parts), lengths(parts))
  j <- match(unlist(parts), vocabulary)
  keep <- !is.na(j)
  Matrix::sparseMatrix(i = i[keep], j = j[keep], x = 1,
                       dims = c(length(parts), length(vocabulary)),
                       dimnames = list(NULL, vocabulary))
}

alphas <- c(0.01, 0.03, 0.05, 0.07, 0.1, 0.3, 0.5, 0.7, 0.9, 1.0)  # _targets.R
k_folds <- 5L

# Rank AUC (Mann-Whitney), as data-raw/label-audit.R: ties score 0.5.
aucRank <- function(truth, score) {
  n1 <- sum(truth == 1)
  n0 <- sum(truth == 0)
  r <- rank(score)
  (sum(r[truth == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

date_added <- readr::read_csv("data-raw/snapshots/2026-11-07/date_added.csv.gz",
                              show_col_types = FALSE)
genres <- sort(unique(targets::tar_read(novels)$genre))

# The baseline's covariates, as a numeric matrix without intercept, with the
# genre levels fixed across folds so train and test columns line up.
covariates <- function(d) {
  added <- date_added$date_added[match(d$gutenberg_id, date_added$gutenberg_id)]
  if (anyNA(added)) {
    stop(sum(is.na(added)), " book(s) have no date_added in the 2026-11-07 ",
         "snapshot; the baseline cannot be built for them.", call. = FALSE)
  }
  frame <- data.frame(
    genre = factor(d$genre, levels = genres),
    log_len = log10(nchar(d$text)),
    log_added = log10(as.numeric(as.Date(added))),
    log_id = log10(d$gutenberg_id)
  )
  formula <- if (with_id) ~ genre + log_len + log_added + log_id else
    ~ genre + log_len + log_added
  stats::model.matrix(formula, frame)[, -1, drop = FALSE]
}

# The best alpha by inner-CV AUC, and that fit.
bestByInnerCv <- function(fits) {
  inner <- vapply(fits, function(f) max(f$cvm), numeric(1))
  i <- which.max(inner)
  list(alpha = alphas[i], fit = fits[[i]], inner_auc = inner[i])
}

rows <- lapply(seq_len(k_folds), function(k) {
  message("fold ", k)
  train <- targets::tar_read_raw(paste0("train_data_", k))
  test <- targets::tar_read_raw(paste0("test_data_", k))
  dtm_train <- targets::tar_read_raw(paste0("dtm_train_", k))
  dtm_test <- targets::tar_read_raw(paste0("dtm_test_", k))
  foldid <- targets::tar_read_raw(paste0("cv_foldid_", k))
  x_train <- covariates(train)
  x_test <- covariates(test)

  n_heading_columns <- NA_integer_
  if (!with_headings) {
    # Bibliographic: unpenalised logistic regression.
    bib <- suppressWarnings(stats::glm.fit(cbind(1, x_train), train$response,
                                           family = stats::binomial()))
    bib_auc <- aucRank(test$response, as.vector(cbind(1, x_test) %*% bib$coefficients))
  } else {
    # The record with headings: core covariates unpenalised, heading parts
    # penalised, the vocabulary taken from this fold's training books only.
    parts_train <- headingParts(train$gutenberg_id)
    counts <- table(unlist(parts_train))
    vocabulary <- sort(names(counts)[counts >= 3])
    n_heading_columns <- length(vocabulary)
    x_train <- cbind(Matrix::Matrix(x_train, sparse = TRUE),
                     headingMatrix(parts_train, vocabulary))
    x_test <- cbind(Matrix::Matrix(x_test, sparse = TRUE),
                    headingMatrix(headingParts(test$gutenberg_id), vocabulary))
    n_core <- ncol(x_train) - n_heading_columns
    pf_bib <- c(rep(0, n_core), rep(1, n_heading_columns))
    bib_fits <- lapply(alphas, function(a) {
      glmnet::cv.glmnet(x_train, train$response, family = "binomial", alpha = a,
                        foldid = foldid, type.measure = "auc", penalty.factor = pf_bib,
                        control = list(thresh = 1e-5, maxit = 1e4))
    })
    bib_best <- bestByInnerCv(bib_fits)
    bib_auc <- aucRank(test$response, as.numeric(stats::predict(
      bib_best$fit, newx = x_test, s = "lambda.min", type = "link")))
  }

  # Text: the pipeline's fits, alpha by inner CV.
  text_fits <- lapply(alphas, function(a) targets::tar_read_raw(paste0("model_fit_", k, "_", a)))
  text_best <- bestByInnerCv(text_fits)
  text_auc <- aucRank(test$response, as.numeric(stats::predict(
    text_best$fit, newx = dtm_test, s = "lambda.min", type = "link")))

  # Stacked: the pure-text classifier's score as one column beside the
  # record. Refit at the chosen alpha with keep = TRUE for pre-validated
  # training scores (same data, alpha and foldid as the pipeline's fit).
  pv <- glmnet::cv.glmnet(dtm_train, train$response, family = "binomial",
                          alpha = text_best$alpha, foldid = foldid,
                          type.measure = "auc", keep = TRUE,
                          control = list(thresh = 1e-5, maxit = 1e4))
  # glmnet 5.x keeps binomial pre-validated fits on the link (log-odds)
  # scale; scores all inside [0, 1] would mean probabilities instead.
  s_train <- pv$fit.preval[, match(pv$lambda.min, pv$lambda)]
  if (min(s_train) >= 0 && max(s_train) <= 1) {
    stop("fit.preval looks like probabilities, not log-odds; check the glmnet version")
  }
  s_test <- as.numeric(stats::predict(pv, newx = dtm_test, s = "lambda.min", type = "link"))
  if (!with_headings) {
    st <- suppressWarnings(stats::glm.fit(cbind(1, x_train, s_train), train$response,
                                          family = stats::binomial()))
    stacked_auc <- aucRank(test$response,
                           as.vector(cbind(1, x_test, s_test) %*% st$coefficients))
  } else {
    st_fits <- lapply(alphas, function(a) {
      glmnet::cv.glmnet(cbind(x_train, s_train), train$response, family = "binomial",
                        alpha = a, foldid = foldid, type.measure = "auc",
                        penalty.factor = c(pf_bib, 0),
                        control = list(thresh = 1e-5, maxit = 1e4))
    })
    stacked_auc <- aucRank(test$response, as.numeric(stats::predict(
      bestByInnerCv(st_fits)$fit, newx = cbind(x_test, s_test), s = "lambda.min",
      type = "link")))
  }

  # Joint: covariates first, unpenalised, then the TF-IDF columns.
  z_train <- cbind(Matrix::Matrix(x_train, sparse = TRUE), dtm_train)
  z_test <- cbind(Matrix::Matrix(x_test, sparse = TRUE), dtm_test)
  pf <- c(if (with_headings) pf_bib else rep(0, ncol(x_train)), rep(1, ncol(dtm_train)))
  comb_fits <- lapply(alphas, function(a) {
    glmnet::cv.glmnet(z_train, train$response, family = "binomial", alpha = a,
                      foldid = foldid, type.measure = "auc", penalty.factor = pf,
                      control = list(thresh = 1e-5, maxit = 1e4))
  })
  comb_best <- bestByInnerCv(comb_fits)
  comb_auc <- aucRank(test$response, as.numeric(stats::predict(
    comb_best$fit, newx = z_test, s = "lambda.min", type = "link")))

  tibble::tibble(
    fold = k, n_test = nrow(test), heading_columns = n_heading_columns,
    bibliographic = bib_auc,
    text = text_auc, text_alpha = text_best$alpha,
    stacked = stacked_auc,
    joint = comb_auc, joint_alpha = comb_best$alpha,
    text_minus_bib = text_auc - bib_auc,
    stacked_minus_bib = stacked_auc - bib_auc,
    joint_minus_bib = comb_auc - bib_auc
  )
})
res <- dplyr::bind_rows(rows)

# Section 7's arithmetic: SD scales with 1/sqrt(test books per fold); the
# wave's expected ~400 per fold; one-sided alpha 0.05 and power 0.8 at 5
# folds use t(0.95, 4) = 2.132 and z(0.8) = 0.842.
mde <- function(sd_2013, n_2013 = mean(res$n_test), n_wave = 400) {
  sd_wave <- sd_2013 * sqrt(n_2013 / n_wave)
  c(sd_wave = sd_wave, mde = (2.132 + 0.842) * sd_wave / sqrt(k_folds))
}

print(as.data.frame(res), digits = 3)
diffs <- list("text - bibliographic (registered)" = res$text_minus_bib,
              "stacked - bibliographic (amended)" = res$stacked_minus_bib,
              "joint - bibliographic (first draft)" = res$joint_minus_bib)
summ <- tibble::tibble(
  comparison = names(diffs),
  mean = vapply(diffs, mean, numeric(1)),
  positive_folds = vapply(diffs, function(d) sum(d > 0), integer(1)),
  sd_2013 = vapply(diffs, stats::sd, numeric(1)),
  sd_wave = vapply(diffs, function(d) mde(stats::sd(d))[["sd_wave"]], numeric(1)),
  mde_wave = vapply(diffs, function(d) mde(stats::sd(d))[["mde"]], numeric(1))
)
cat("\nBaseline:", if (with_headings) paste0(
  "genre + log_len + log_added + log_id + heading parts (catalogue sha256 ",
  catalogue_sha256, ")") else if (with_id) "genre + log_len + log_added + log_id" else
  "genre + log_len + log_added (section 6)", "\n")
cat("Mean AUC: bibliographic", format(mean(res$bibliographic), digits = 3),
    " text", format(mean(res$text), digits = 3),
    " stacked", format(mean(res$stacked), digits = 3),
    " joint", format(mean(res$joint), digits = 3), "\n")
cat("Mean test books per fold:", format(mean(res$n_test), digits = 4), "\n\n")
print(as.data.frame(summ), digits = 3)

if (!is.null(out_path)) {
  readr::write_csv(res, out_path)
  message("wrote ", out_path)
}
