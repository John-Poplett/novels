# A continuous response on the 2013 corpus (TODO: "Decide when and how to
# replace binomial classification with a continuous response"; README
# Roadmap, "model the download count instead"). Exploratory.
#
# The question: does a model fitted to the download count itself order books
# better than the binary classifier's score does, and above all *within* the
# success band, the ordering the 0/1 label throws away? The 2013 corpus is
# case-control: per genre the middle band of counts was discarded (TODO.md,
# "Selection rule"), so a regression here can only speak to ordering inside
# each band, never to Gutenberg as a whole. The 2013 count is cumulative
# downloads at the scrape (id 215 holds 3,433).
#
# Outcome y = log1p(download_count). Four models on each of the five
# author-grouped folds, on the pipeline's rows and TF-IDF matrices:
#
#   text_bin     the pipeline's binomial glmnet (alpha by inner-CV AUC), its
#                link score: the existing classifier, as the comparison
#   text_reg     gaussian glmnet on the same TF-IDF matrix and inner folds,
#                alpha by inner-CV mean squared error
#   bib_reg      lm of y on the bibliographic baseline of section 6 as
#                amended (genre + log10 length + log10 date_added +
#                log10 gutenberg_id): the yardstick, never an input to text
#   stacked_reg  lm of y on the baseline plus ONE column, text_reg's score
#                (pre-validated on training books, cv.glmnet keep = TRUE)
#
# Every book is scored once, by the fold that did not train on it, and the
# metrics are computed on those pooled out-of-fold scores:
#
#   spearman          overall, against y
#   spearman_success  among successes only (response == 1)
#   spearman_failure  among failures only
#   success_by_genre  the success-band Spearman within each genre, averaged
#                     over the 8 genres (genres' thresholds differ, so the
#                     pooled band mixes genre levels)
#   auc               against the 0/1 label: the binomial number, comparable
#
# 95% intervals resample whole authors (2,000 draws), as section 7 does.
# The three patched LibriVox rows (19513, 19640, 19836) still carry the
# audio entries' counts, so they are scored but left out of every metric.
#
# Run from the package root after `tar_make()`:
#
#   Rscript data-raw/continuous-response.R [--out=FILE] [--draws=N]
#
# --out writes the pooled out-of-fold scores, one row per test book.

suppressMessages({
  library(dplyr)
  library(targets)
})
pkgload::load_all(quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
out_path <- sub("^--out=", "", grep("^--out=", args, value = TRUE))
if (!length(out_path)) out_path <- NULL
draws <- sub("^--draws=", "", grep("^--draws=", args, value = TRUE))
draws <- if (length(draws)) as.integer(draws) else 2000L
if (is.na(draws) || draws < 1) stop("--draws must be a positive integer.", call. = FALSE)

alphas <- c(0.01, 0.03, 0.05, 0.07, 0.1, 0.3, 0.5, 0.7, 0.9, 1.0)  # _targets.R
k_folds <- 5L
unreliable_counts <- c(19513L, 19640L, 19836L)  # README, "Not yet fixed"
glmnet_control <- list(thresh = 1e-5, maxit = 1e4)

# aucRank() is the package's (R/aucRank.R), loaded above.

spearman <- function(x, y) stats::cor(x, y, method = "spearman")

date_added <- readr::read_csv("data-raw/snapshots/2026-11-07/date_added.csv.gz",
                              show_col_types = FALSE)
genres <- sort(unique(targets::tar_read(novels)$genre))

# The amended baseline's covariates, as data-raw/nested-comparison.R --with-id.
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
  stats::model.matrix(~ genre + log_len + log_added + log_id, frame)[, -1, drop = FALSE]
}

outcome <- function(d) log1p(d$download_count)

scored <- lapply(seq_len(k_folds), function(k) {
  message("fold ", k)
  train <- targets::tar_read_raw(paste0("train_data_", k))
  test <- targets::tar_read_raw(paste0("test_data_", k))
  dtm_train <- targets::tar_read_raw(paste0("dtm_train_", k))
  dtm_test <- targets::tar_read_raw(paste0("dtm_test_", k))
  foldid <- targets::tar_read_raw(paste0("cv_foldid_", k))
  y_train <- outcome(train)

  # text_bin: the pipeline's fits, alpha by best inner-CV AUC.
  bin_fits <- lapply(alphas, function(a) targets::tar_read_raw(paste0("model_fit_", k, "_", a)))
  bin_best <- bin_fits[[which.max(vapply(bin_fits, function(f) max(f$cvm), numeric(1)))]]
  text_bin <- as.numeric(stats::predict(bin_best, newx = dtm_test, s = "lambda.min",
                                        type = "link"))

  # text_reg: gaussian, alpha by lowest inner-CV MSE, with keep = TRUE so the
  # chosen fit also carries pre-validated training scores for the stack.
  reg_fits <- lapply(alphas, function(a) {
    glmnet::cv.glmnet(dtm_train, y_train, family = "gaussian", alpha = a,
                      foldid = foldid, type.measure = "mse", keep = TRUE,
                      control = glmnet_control)
  })
  i <- which.min(vapply(reg_fits, function(f) min(f$cvm), numeric(1)))
  reg_best <- reg_fits[[i]]
  text_reg <- as.numeric(stats::predict(reg_best, newx = dtm_test, s = "lambda.min"))
  s_train <- reg_best$fit.preval[, match(reg_best$lambda.min, reg_best$lambda)]

  # bib_reg and stacked_reg: ordinary least squares.
  x_train <- covariates(train)
  x_test <- covariates(test)
  bib <- stats::lm.fit(cbind(1, x_train), y_train)
  bib_reg <- as.vector(cbind(1, x_test) %*% bib$coefficients)
  st <- stats::lm.fit(cbind(1, x_train, s_train), y_train)
  stacked_reg <- as.vector(cbind(1, x_test, text_reg) %*% st$coefficients)

  tibble::tibble(
    fold = k, gutenberg_id = test$gutenberg_id, author = test$author,
    genre = as.character(test$genre), response = test$response,
    download_count = test$download_count, y = outcome(test),
    text_bin = text_bin, text_reg = text_reg, bib_reg = bib_reg,
    stacked_reg = stacked_reg,
    text_bin_alpha = alphas[which.max(vapply(bin_fits, function(f) max(f$cvm), numeric(1)))],
    text_reg_alpha = alphas[i],
    text_reg_nonzero = reg_best$nzero[match(reg_best$lambda.min, reg_best$lambda)]
  )
})
oof <- dplyr::bind_rows(scored)
eval_rows <- oof[!oof$gutenberg_id %in% unreliable_counts, ]

models <- c("text_bin", "text_reg", "bib_reg", "stacked_reg")

# All metrics for one set of rows, one row per model.
metrics <- function(d) {
  succ <- d$response == 1
  fail <- !succ
  by_genre <- function(score) {
    mean(vapply(split(seq_len(nrow(d))[succ], d$genre[succ]), function(j) {
      spearman(score[j], d$y[j])
    }, numeric(1)), na.rm = TRUE)
  }
  do.call(rbind, lapply(models, function(m) {
    s <- d[[m]]
    data.frame(model = m,
               spearman = spearman(s, d$y),
               spearman_success = spearman(s[succ], d$y[succ]),
               spearman_failure = spearman(s[fail], d$y[fail]),
               success_by_genre = by_genre(s),
               auc = aucRank(d$response, s))
  }))
}

point <- metrics(eval_rows)

# Whole-author bootstrap of every metric, and of the two differences that
# answer the question: text_reg - text_bin, stacked_reg - bib_reg.
set.seed(20261123)
authors <- unique(eval_rows$author)
rows_of <- split(seq_len(nrow(eval_rows)), eval_rows$author)
boot <- replicate(draws, {
  pick <- sample(authors, length(authors), replace = TRUE)
  m <- metrics(eval_rows[unlist(rows_of[pick], use.names = FALSE), ])
  v <- as.matrix(m[, -1])
  rownames(v) <- m$model
  c(v, v["text_reg", ] - v["text_bin", ], v["stacked_reg", ] - v["bib_reg", ])
})
metric_names <- names(point)[-1]
labels <- c(outer(models, metric_names, paste, sep = ":"),
            paste0("text_reg - text_bin:", metric_names),
            paste0("stacked_reg - bib_reg:", metric_names))
pv <- as.matrix(point[, -1])
rownames(pv) <- point$model
estimate <- c(pv, pv["text_reg", ] - pv["text_bin", ], pv["stacked_reg", ] - pv["bib_reg", ])
ci <- apply(boot, 1, stats::quantile, probs = c(0.025, 0.975), na.rm = TRUE)
summary_table <- data.frame(
  comparison = sub(":.*", "", labels), metric = sub(".*:", "", labels),
  estimate = estimate, lo = ci[1, ], hi = ci[2, ], row.names = NULL
)

cat("Books scored:", nrow(oof), " in the metrics:", nrow(eval_rows),
    " authors:", length(authors), " bootstrap draws:", draws, "\n")
cat("Alphas chosen (text_bin, text_reg) per fold:\n")
print(as.data.frame(dplyr::distinct(oof, fold, text_bin_alpha, text_reg_alpha,
                                    text_reg_nonzero)))
cat("\n")
print(summary_table, digits = 3)

if (!is.null(out_path)) {
  readr::write_csv(oof, out_path)
  message("wrote ", out_path)
}
