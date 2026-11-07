# Learning curve for the elastic-net sweep: does held-out AUC still rise as the
# training set grows? If it does, more data (especially more *authors*) would
# help; if it is flat, the labels' noise ceiling is the limit.
#
# For each fold, train on a random fraction of that fold's training authors
# (whole authors, so folds stay author-disjoint) and score on the fold's
# unchanged test set. Everything else is the pipeline's own code, applied to
# the stored per-fold data. At fraction 1 the result must equal the pipeline's
# eval_test_* targets; the script checks that and stops if it does not.
#
# Run from the package root after tar_make():
#   Rscript data-raw/learning-curve.R            # full curve
#   Rscript data-raw/learning-curve.R smoke      # one fold, one fraction, one alpha

suppressMessages({
  library(dplyr)
  library(targets)
})
pkgload::load_all(quiet = TRUE)

smoke <- "smoke" %in% commandArgs(trailingOnly = TRUE)

alphas    <- if (smoke) 0.5 else c(0.05, 0.5, 1)
fractions <- if (smoke) c(0.25, 1) else c(0.25, 0.5, 0.75, 1)
repeats   <- if (smoke) 1L else 3L
folds     <- if (smoke) 1L else seq_len(5L)

# Keep a random fraction of the authors, with all of their books.
subsample_authors <- function(train, fraction, seed) {
  authors <- unique(train$author)
  set.seed(seed)
  keep <- sample(authors, size = ceiling(fraction * length(authors)))
  train[train$author %in% keep, ]
}

# Everything that does not depend on alpha, as in the pipeline's per-fold prep.
prepare_fold <- function(train, test) {
  vectorizer <- buildVectorizer(train, ngram = c(1L, 1L))
  tfidf <- fitTfidf(buildDtm(train, vectorizer))
  list(
    dtm_train = tfidf$dtm,
    dtm_test = transformTfidf(tfidf, buildDtm(test, vectorizer)),
    foldid = makeFoldid(train$author, nfolds = 5)
  )
}

score_run <- function(fold, fraction, rep) {
  train <- tar_read_raw(paste0("train_data_", fold))
  test <- tar_read_raw(paste0("test_data_", fold))
  seed <- fold * 1000 + rep * 100 + round(fraction * 100)
  train_sub <- if (fraction < 1) subsample_authors(train, fraction, seed) else train

  prep <- prepare_fold(train_sub, test)
  alphas |>
    lapply(function(a) {
      fit <- fitGlmnetModel(prep$dtm_train, train_sub$response, a, prep$foldid)
      evaluateModel(fit, prep$dtm_test, test$response, a)
    }) |>
    bind_rows() |>
    mutate(fold = fold, fraction = fraction, rep = rep,
           n_train_books = nrow(train_sub), .before = 1)
}

grid <- tidyr::expand_grid(fold = folds, fraction = fractions, rep = seq_len(repeats)) |>
  filter(fraction < 1 | rep == 1L)

message(sprintf("learning curve: %d runs x %d alphas", nrow(grid), length(alphas)))
results <- vector("list", nrow(grid))
for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  message(sprintf("[%s] run %d/%d: fold %d, fraction %.2f, rep %d",
                  format(Sys.time(), "%H:%M:%S"), i, nrow(grid), g$fold, g$fraction, g$rep))
  results[[i]] <- score_run(g$fold, g$fraction, g$rep)
}
res <- bind_rows(results)

# The check that makes the rest trustworthy: fraction 1 must reproduce the
# pipeline's own per-fold test AUCs.
full <- res |> filter(fraction == 1)
pipeline_auc <- mapply(
  function(k, a) tar_read_raw(paste0("eval_test_", k, "_", a))$auc,
  full$fold, full$alpha
)
max_diff <- max(abs(full$auc - pipeline_auc))
cat(sprintf("\nfraction 1 vs the pipeline's eval_test_*: max |difference| = %.2e\n", max_diff))
if (max_diff > 1e-8) stop("fraction 1 does not reproduce the pipeline; do not trust this curve")

out <- if (smoke) "data-raw/learning-curve-smoke.csv" else "data-raw/learning-curve.csv"
write.csv(res, out, row.names = FALSE)

per_fold <- res |>
  group_by(alpha, fraction, fold) |>
  summarise(auc = mean(auc), n_train_books = mean(n_train_books), .groups = "drop")

curve <- per_fold |>
  group_by(alpha, fraction) |>
  summarise(train_books = round(mean(n_train_books)), mean_auc = mean(auc),
            sd_across_folds = sd(auc), .groups = "drop")
cat("\nMean test AUC by training fraction (averaged over repeats, then folds):\n")
print(as.data.frame(curve |> mutate(across(c(mean_auc, sd_across_folds), ~ round(., 4)))))

# Paired by fold, so fold difficulty cancels out: how much does the full
# training set gain over smaller ones?
if (!smoke) {
  wide <- per_fold |> select(alpha, fraction, fold, auc) |>
    tidyr::pivot_wider(names_from = fraction, values_from = auc, names_prefix = "f")
  gains <- bind_rows(
    wide |> transmute(alpha, fold, comparison = "100% vs 50%", gain = f1 - f0.5),
    wide |> transmute(alpha, fold, comparison = "100% vs 75%", gain = f1 - f0.75)
  ) |>
    group_by(alpha, comparison) |>
    summarise(mean_gain = mean(gain), se = sd(gain) / sqrt(n()), z = mean_gain / se, .groups = "drop")
  cat("\nGain from the full training set (paired across the 5 folds):\n")
  print(as.data.frame(gains |> mutate(across(c(mean_gain, se, z), ~ round(., 4)))))
}
cat("\nresults written to", out, "\n")
