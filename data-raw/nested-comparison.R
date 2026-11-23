# The H2 comparison on the 2013 corpus, registered and nested
# (PREREGISTRATION.md section 11, draft amendment of 2026-11-22).
#
# Three models on each of the five author-grouped folds, on the same rows:
#
#   bibliographic  glm on genre + log10(text length) + log10(date_added),
#                  section 6's baseline without publication year (absent, as
#                  section 6 allows; the omission is reported)
#   text           TF-IDF unigrams + glmnet: the pipeline's own fits
#   combined       the same TF-IDF matrix plus the bibliographic covariates,
#                  unpenalised (penalty.factor = 0), one glmnet fit
#
# For text and combined, alpha is chosen per fold by inner cross-validation
# (the best cross-validated AUC at lambda.min over the pipeline's alpha
# grid), never by the outer test fold. The paired fold-level differences
# text - bibliographic (registered) and combined - bibliographic (nested)
# give the SDs from which section 7's minimum detectable effect is computed.
#
# Run from the package root after `tar_make()`:
#
#   Rscript data-raw/nested-comparison.R [--with-id] [--out=FILE]
#
# --with-id adds log10(gutenberg_id) to the baseline. It is not in section 6,
# but on this corpus it predicts the label far better than date_added
# (AUC 0.810 against 0.748 alone), so it shows whether the nested gain
# survives a stronger record.
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
with_id <- "--with-id" %in% args

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

  # Bibliographic: unpenalised logistic regression.
  bib <- suppressWarnings(stats::glm.fit(cbind(1, x_train), train$response,
                                         family = stats::binomial()))
  bib_auc <- aucRank(test$response, as.vector(cbind(1, x_test) %*% bib$coefficients))

  # Text: the pipeline's fits, alpha by inner CV.
  text_fits <- lapply(alphas, function(a) targets::tar_read_raw(paste0("model_fit_", k, "_", a)))
  text_best <- bestByInnerCv(text_fits)
  text_auc <- aucRank(test$response, as.numeric(stats::predict(
    text_best$fit, newx = dtm_test, s = "lambda.min", type = "link")))

  # Combined: covariates first, unpenalised, then the TF-IDF columns.
  z_train <- cbind(Matrix::Matrix(x_train, sparse = TRUE), dtm_train)
  z_test <- cbind(Matrix::Matrix(x_test, sparse = TRUE), dtm_test)
  pf <- c(rep(0, ncol(x_train)), rep(1, ncol(dtm_train)))
  comb_fits <- lapply(alphas, function(a) {
    glmnet::cv.glmnet(z_train, train$response, family = "binomial", alpha = a,
                      foldid = foldid, type.measure = "auc", penalty.factor = pf,
                      control = list(thresh = 1e-5, maxit = 1e4))
  })
  comb_best <- bestByInnerCv(comb_fits)
  comb_auc <- aucRank(test$response, as.numeric(stats::predict(
    comb_best$fit, newx = z_test, s = "lambda.min", type = "link")))

  tibble::tibble(
    fold = k, n_test = nrow(test),
    bibliographic = bib_auc,
    text = text_auc, text_alpha = text_best$alpha,
    combined = comb_auc, combined_alpha = comb_best$alpha,
    text_minus_bib = text_auc - bib_auc,
    combined_minus_bib = comb_auc - bib_auc
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
summ <- tibble::tibble(
  comparison = c("text - bibliographic (registered)", "combined - bibliographic (nested)"),
  mean = c(mean(res$text_minus_bib), mean(res$combined_minus_bib)),
  sd_2013 = c(stats::sd(res$text_minus_bib), stats::sd(res$combined_minus_bib)),
  sd_wave = c(mde(stats::sd(res$text_minus_bib))[["sd_wave"]],
              mde(stats::sd(res$combined_minus_bib))[["sd_wave"]]),
  mde_wave = c(mde(stats::sd(res$text_minus_bib))[["mde"]],
               mde(stats::sd(res$combined_minus_bib))[["mde"]])
)
cat("\nBaseline:", if (with_id) "genre + log_len + log_added + log_id" else
  "genre + log_len + log_added (section 6)", "\n")
cat("Mean AUC: bibliographic", format(mean(res$bibliographic), digits = 3),
    " text", format(mean(res$text), digits = 3),
    " combined", format(mean(res$combined), digits = 3), "\n")
cat("Mean test books per fold:", format(mean(res$n_test), digits = 4), "\n\n")
print(as.data.frame(summ), digits = 3)

if (!is.null(out_path)) {
  readr::write_csv(res, out_path)
  message("wrote ", out_path)
}
