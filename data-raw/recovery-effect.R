# How much of the sweep's movement when 23177 and 23178 were recovered
# (CHANGELOG, 2013+) is the two books, and how much the inner folds that
# moved with them (TODO.md, "Inner folds that do not move when authors are
# added"). makeFoldid() shuffles the training set's authors positionally, so
# adding two authors reassigns most of the others' inner folds as well.
#
# Three runs of the pipeline's per-fold chain (stored outer author folds,
# per-fold dedupe, vocabulary and TF-IDF, cv.glmnet at lambda.min), over the
# pipeline's alpha grid:
#
#   A  before        the corpus without 23177 and 23178; inner folds as the
#                    pipeline builds them. Must reproduce the stored baseline.
#   B  books only    the corpus with them; every author already in a training
#                    set keeps the inner fold it had in A, and the two new
#                    authors take the one C gives them.
#   C  after         the corpus with them; inner folds as the pipeline builds
#                    them (the reshuffle).
#
# B - A is the books' share of the movement, C - B the reshuffle's. The
# patched LibriVox rows are set back to their labels before
# applyLibrivoxCounts() (only 19836's changes: success back to failure), so
# that change does not enter any run.
#
#   Rscript data-raw/recovery-effect.R [--cores=5] [--out=FILE.csv]
#
# Run from the package root after `tar_make()`.

pkgload::load_all(quiet = TRUE)

arg <- function(name, default = NA) {
  v <- sub(paste0("^--", name, "="), "", grep(paste0("^--", name, "="),
                                               commandArgs(trailingOnly = TRUE), value = TRUE))
  if (length(v)) v else default
}
cores <- as.integer(arg("cores", "5"))
out_path <- arg("out", NA)
alphas <- c(0.01, 0.03, 0.05, 0.07, 0.1, 0.3, 0.5, 0.7, 0.9, 1.0)  # _targets.R's alpha_grid
recovered <- c(23177L, 23178L)

after <- targets::tar_read(novels)
stopifnot("the pipeline's corpus does not hold the recovered rows; run tar_make()" =
            all(recovered %in% after$gutenberg_id))
# Labels as they were before applyLibrivoxCounts(): 19836 read 9, a Poetry failure.
after$response[after$gutenberg_id == 19836L] <- 0L
before <- after[!after$gutenberg_id %in% recovered, ]

# One outer fold's prepared data, and its pipeline foldid.
prepare <- function(novels, k) {
  folds <- createKFoldSplit(novels, 5, use_stored_folds = TRUE)
  train <- dedupe_by_group(novels[folds[[k]]$train, ], "gutenberg_id")
  test <- dedupe_by_group(novels[folds[[k]]$test, ], "gutenberg_id")
  vectorizer <- buildVectorizer(train, ngram = c(1L, 1L))
  tfidf <- fitTfidf(buildDtm(train, vectorizer))
  list(train = train, test = test, dtm_train = tfidf$dtm,
       dtm_test = transformTfidf(tfidf, buildDtm(test, vectorizer)),
       foldid = makeFoldid(train$author, nfolds = 5))
}

sweep <- function(p, foldid) {
  do.call(rbind, lapply(alphas, function(a) {
    fit <- fitGlmnetModel(p$dtm_train, p$train$response, a, foldid)
    evaluateModel(fit, p$dtm_test, p$test$response, a)[, c("alpha", "auc")]
  }))
}

runFold <- function(k) {
  pa <- prepare(before, k)
  pc <- prepare(after, k)
  # B: C's data, A's inner fold for every author A's training set had.
  frozen <- pa$foldid[match(pc$train$author, pa$train$author)]
  frozen[is.na(frozen)] <- pc$foldid[is.na(frozen)]
  rbind(cbind(condition = "A", fold = k, sweep(pa, pa$foldid)),
        cbind(condition = "B", fold = k, sweep(pc, frozen)),
        cbind(condition = "C", fold = k, sweep(pc, pc$foldid)),
        data.frame(condition = "moved", fold = k, alpha = NA,
                   auc = mean(frozen != pc$foldid)))
}

res <- parallel::mclapply(1:5, runFold, mc.cores = cores)
bad <- vapply(res, function(r) is.null(r) || inherits(r, "try-error"), logical(1))
if (any(bad)) stop("fold(s) ", paste(which(bad), collapse = ", "), " failed: ",
                   paste(vapply(res[bad], as.character, ""), collapse = "; "), call. = FALSE)
res <- do.call(rbind, res)

moved <- res[res$condition == "moved", ]
cat("share of training rows whose inner fold moved, by outer fold:",
    paste(sprintf("%.2f", moved$auc), collapse = " "), "\n")
res <- res[res$condition != "moved", ]
if (!is.na(out_path)) readr::write_csv(res, out_path)

means <- stats::aggregate(auc ~ condition + alpha, data = res, FUN = mean)
wide <- stats::reshape(means, idvar = "alpha", timevar = "condition", direction = "wide")
names(wide) <- sub("^auc\\.", "", names(wide))
baseline <- readr::read_csv("baselines/cv_summary.csv", show_col_types = FALSE)
wide$baseline <- baseline$mean_auc[match(wide$alpha, baseline$alpha)]
wide$books <- wide$B - wide$A
wide$reshuffle <- wide$C - wide$B
wide$total <- wide$C - wide$A
print(wide[order(wide$alpha), c("alpha", "baseline", "A", "B", "C", "books", "reshuffle", "total")],
      digits = 4, row.names = FALSE)
if (max(abs(wide$A - wide$baseline)) > 1e-8) {
  warning("Run A does not reproduce baselines/cv_summary.csv (largest gap ",
          signif(max(abs(wide$A - wide$baseline)), 3), "); the split is not trustworthy.",
          call. = FALSE)
}
