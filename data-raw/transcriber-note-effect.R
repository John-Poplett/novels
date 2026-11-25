# The effect of stripping the pulp-magazine transcriber's note (2026-11-24;
# stripTranscriberNote() in R/corpusRepair.R) on the text model, overall and
# within Short Stories, where the note marked all 11 of its books as
# successes. The pipeline's sweep reports one pooled AUC per fold and keeps
# no predictions, so it cannot show a within-genre effect; this script runs
# the pipeline's own per-fold chain (the stored author folds, per-fold
# vocabulary and TF-IDF, cv.glmnet with author-grouped inner folds) on the
# corpus before and after, at one alpha, and scores the out-of-fold
# predictions both pooled and for the Short Stories rows alone.
#
#   Rscript data-raw/transcriber-note-effect.R --before=FILE.rds --after=FILE.rds
#       [--alpha=0.05]
#
# FILE.rds: a saved tar_read(novels). --alpha defaults to the sweep's best
# mean AUC before the change (baselines/cv_summary.csv, 2026-11-07).

pkgload::load_all(quiet = TRUE)

arg <- function(name, default = NA) {
  v <- sub(paste0("^--", name, "="), "", grep(paste0("^--", name, "="),
                                               commandArgs(trailingOnly = TRUE), value = TRUE))
  if (length(v)) v else default
}
before_path <- arg("before")
after_path <- arg("after")
alpha <- as.numeric(arg("alpha", "0.05"))
if (is.na(before_path) || is.na(after_path)) {
  stop("--before=FILE.rds and --after=FILE.rds are required.", call. = FALSE)
}

outOfFold <- function(novels) {
  folds <- createKFoldSplit(novels, 5, use_stored_folds = TRUE)
  do.call(rbind, lapply(seq_along(folds), function(k) {
    train <- dedupe_by_group(novels[folds[[k]]$train, ], "gutenberg_id")
    test <- dedupe_by_group(novels[folds[[k]]$test, ], "gutenberg_id")
    vectorizer <- buildVectorizer(train, ngram = c(1L, 1L))
    tfidf <- fitTfidf(buildDtm(train, vectorizer))
    dtm_test <- transformTfidf(tfidf, buildDtm(test, vectorizer))
    fit <- fitGlmnetModel(tfidf$dtm, train$response, alpha,
                          makeFoldid(train$author, nfolds = 5))
    data.frame(fold = k, gutenberg_id = test$gutenberg_id, response = test$response,
               score = as.numeric(predict(fit, newx = dtm_test, s = "lambda.min")))
  }))
}

summarise <- function(novels, oof, label) {
  pooled_by_fold <- vapply(split(oof, oof$fold), function(d) aucRank(d$response, d$score), 0)
  # Short Stories: its own rows and labels, scored by the out-of-fold prediction
  ss <- novels[novels$genre == "Short Stories", c("gutenberg_id", "response")]
  ss$score <- oof$score[match(ss$gutenberg_id, oof$gutenberg_id)]
  note <- grepl("copyright[[:space:]]+on[[:space:]]+this[[:space:]]+publication[[:space:]]+was[[:space:]]+renewed",
                novels$text[match(ss$gutenberg_id, novels$gutenberg_id)])
  data.frame(corpus = label, alpha = alpha, mean_fold_auc = mean(pooled_by_fold),
             short_stories_auc = aucRank(ss$response, ss$score),
             short_stories_n = nrow(ss), books_with_note = sum(note))
}

before <- readRDS(before_path)
after <- readRDS(after_path)
res <- rbind(summarise(before, outOfFold(before), "before"),
             summarise(after, outOfFold(after), "after"))
print(res, digits = 4, row.names = FALSE)
cat(sprintf("change: mean fold AUC %+.4f, Short Stories AUC %+.4f\n",
            res$mean_fold_auc[2] - res$mean_fold_auc[1],
            res$short_stories_auc[2] - res$short_stories_auc[1]))
