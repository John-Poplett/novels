# Pilot for the genre-classifier qualifier (PREREGISTRATION.md, draft
# amendment of 2026-11-23, "a qualifier for the text genre classifier",
# point 3). It uses only the 2013 genre labels, never a genre classifier.
#
# The question: how noisy is one contaminated draw, and so how many draws
# per rate keep the induced bias's standard error at or below 0.003?
#
# One host genre at a time is the stratum. Books from outside it (other
# genres, with their own labels and counts) are added at rate c, so that c
# is their share of the contaminated stratum. On each of the five stored
# author folds the stacked comparison of section 7 is run on the host
# alone:
#
#   bibliographic  glm on log10(text length) + log10(date_added) +
#                  log10(gutenberg_id). Genre is left out: inside one
#                  stratum it is constant, and a contaminant is not known
#                  to be one.
#   stacked        the same plus one pre-validated TF-IDF glmnet score
#                  (alpha by inner-CV AUC over the pipeline's grid,
#                  cv.glmnet keep = TRUE, inner folds by author)
#
# d = mean over folds of AUC(stacked) - AUC(bibliographic). The clean host
# gives d_0; a draw gives d_c; its bias is d_c - d_0 (paired: same folds,
# same host books, only the contaminants differ). Two labellings:
#
#   fixed   every book keeps its 2013 label
#   recut   labels re-cut on the contaminated stratum: 1 above its median
#           download count, else 0 (the 2013 bands have no middle, so the
#           median is the only quantile cut both bands share)
#
# Tokenisation is done once per book: one raw count matrix over the whole
# corpus, unpruned. Each fold then prunes the vocabulary on its own training
# books with the pipeline's rules (R/text2vocab.R: stopwords removed, term
# count >= 10, document proportion between 10 / n and 0.5) and fits TF-IDF
# on them alone, so nothing about a test book reaches its fold's features.
#
# Run from the package root after `tar_make()`:
#
#   Rscript data-raw/genre-contamination-pilot.R [--hosts=A,B] [--rates=0.05,0.2]
#       [--draws=10] [--cores=16] [--out=FILE]
#
# Hosts are named by genre; the default is every genre.

suppressMessages({
  library(dplyr)
  library(targets)
  library(text2vec)
})
pkgload::load_all(quiet = TRUE)

arg <- function(name, default) {
  v <- sub(paste0("^--", name, "="), "", grep(paste0("^--", name, "="),
                                               commandArgs(trailingOnly = TRUE), value = TRUE))
  if (length(v)) v else default
}
rates <- as.numeric(strsplit(arg("rates", "0.05,0.2"), ",")[[1]])
draws <- as.integer(arg("draws", "10"))
cores <- as.integer(arg("cores", "16"))
out_path <- arg("out", NA_character_)
if (anyNA(rates) || any(rates <= 0 | rates >= 1)) {
  stop("--rates must be numbers strictly between 0 and 1.", call. = FALSE)
}
if (is.na(draws) || draws < 2) stop("--draws must be at least 2.", call. = FALSE)

alphas <- c(0.01, 0.03, 0.05, 0.07, 0.1, 0.3, 0.5, 0.7, 0.9, 1.0)  # _targets.R
unreliable_counts <- c(19513L, 19640L, 19836L)  # README, "Not yet fixed"
glmnet_control <- list(thresh = 1e-5, maxit = 1e4)

aucRank <- function(truth, score) {
  n1 <- sum(truth == 1)
  n0 <- sum(truth == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(score)
  (sum(r[truth == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

novels <- targets::tar_read(novels) |>
  dplyr::filter(!gutenberg_id %in% unreliable_counts)
hosts <- strsplit(arg("hosts", paste(sort(unique(as.character(novels$genre))),
                                     collapse = ",")), ",")[[1]]
if (!all(hosts %in% novels$genre)) {
  stop("Unknown host genre(s): ", paste(setdiff(hosts, novels$genre), collapse = ", "),
       call. = FALSE)
}

author_fold <- readr::read_csv("inst/extdata/author_folds.csv", show_col_types = FALSE)
author_fold <- setNames(author_fold$fold, author_fold$author)
date_added <- readr::read_csv("data-raw/snapshots/2026-11-07/date_added.csv.gz",
                              show_col_types = FALSE)

# One row per book, its genres collapsed; texts are identical across genres.
books <- novels |>
  dplyr::group_by(gutenberg_id) |>
  dplyr::summarise(author = dplyr::first(author), title = dplyr::first(title),
                   text = dplyr::first(text), download_count = dplyr::first(download_count),
                   genres = list(as.character(genre)),
                   response = list(setNames(response, as.character(genre))),
                   .groups = "drop")
books$fold <- unname(author_fold[books$author])
if (anyNA(books$fold)) stop("Authors missing from author_folds.csv.", call. = FALSE)
books$log_len <- log10(nchar(books$text))
books$log_added <- log10(as.numeric(as.Date(
  date_added$date_added[match(books$gutenberg_id, date_added$gutenberg_id)])))
books$log_id <- log10(books$gutenberg_id)
if (anyNA(books$log_added)) stop("Books without date_added.", call. = FALSE)

message("tokenising ", nrow(books), " books once")
it <- getIterator(books$text, as.character(books$gutenberg_id))
vocab_all <- text2vec::create_vocabulary(it, stopwords = stopwords())
counts <- text2vec::create_dtm(it, text2vec::vocab_vectorizer(vocab_all))
books$text <- NULL

# The pipeline's pruning, decided on the training rows only.
foldColumns <- function(train_rows) {
  m <- counts[train_rows, , drop = FALSE]
  n <- length(train_rows)
  term_count <- Matrix::colSums(m)
  doc_prop <- Matrix::colSums(m > 0) / n
  which(term_count >= 10 & doc_prop <= 0.5 & doc_prop >= max(10 / n, 0.001))
}

# d = mean over folds of AUC(stacked) - AUC(bibliographic), for a stratum
# given as book rows and their labels.
stackedGap <- function(rows, y) {
  gaps <- vapply(1:5, function(k) {
    tr <- rows[books$fold[rows] != k]
    te <- rows[books$fold[rows] == k]
    y_tr <- y[books$fold[rows] != k]
    y_te <- y[books$fold[rows] == k]
    if (length(unique(y_te)) < 2 || length(unique(y_tr)) < 2) return(NA_real_)
    cols <- foldColumns(tr)
    tfidf <- text2vec::TfIdf$new()
    x_tr <- tfidf$fit_transform(counts[tr, cols, drop = FALSE])
    x_te <- tfidf$transform(counts[te, cols, drop = FALSE])
    foldid <- makeFoldid(books$author[tr], nfolds = 5)
    fits <- lapply(alphas, function(a) {
      glmnet::cv.glmnet(x_tr, y_tr, family = "binomial", alpha = a, foldid = foldid,
                        type.measure = "auc", keep = TRUE, control = glmnet_control)
    })
    if (!all(vapply(fits, function(f) identical(f$name[[1]], "AUC"), logical(1)))) {
      stop("cv.glmnet fell back from AUC (too few books per inner fold).", call. = FALSE)
    }
    best <- fits[[which.max(vapply(fits, function(f) max(f$cvm), numeric(1)))]]
    s_tr <- best$fit.preval[, match(best$lambda.min, best$lambda)]
    s_te <- as.numeric(stats::predict(best, newx = x_te, s = "lambda.min", type = "link"))
    z <- function(r) cbind(1, books$log_len[r], books$log_added[r], books$log_id[r])
    bib <- suppressWarnings(stats::glm.fit(z(tr), y_tr, family = stats::binomial()))
    st <- suppressWarnings(stats::glm.fit(cbind(z(tr), s_tr), y_tr,
                                          family = stats::binomial()))
    aucRank(y_te, as.vector(cbind(z(te), s_te) %*% st$coefficients)) -
      aucRank(y_te, as.vector(z(te) %*% bib$coefficients))
  }, numeric(1))
  c(gap = mean(gaps, na.rm = TRUE), folds = sum(!is.na(gaps)))
}

labelsFor <- function(rows, host, recut) {
  if (recut) {
    dc <- books$download_count[rows]
    return(as.integer(rank(dc, ties.method = "first") > length(dc) / 2))
  }
  # A contaminant keeps the label of its own (first) genre.
  vapply(rows, function(r) {
    lab <- books$response[[r]]
    if (host %in% names(lab)) lab[[host]] else lab[[1]]
  }, integer(1))
}

jobs <- expand.grid(host = hosts, rate = c(0, rates), draw = seq_len(draws),
                    stringsAsFactors = FALSE)
jobs <- jobs[jobs$rate > 0 | jobs$draw == 1, ]

message(nrow(jobs), " runs (clean hosts and draws) on ", cores, " cores")
started <- Sys.time()
results <- parallel::mclapply(seq_len(nrow(jobs)), function(j) {
  host <- jobs$host[j]
  rate <- jobs$rate[j]
  host_rows <- which(vapply(books$genres, function(g) host %in% g, logical(1)))
  rows <- host_rows
  if (rate > 0) {
    set.seed(20261123 + j)
    outside <- setdiff(seq_len(nrow(books)), host_rows)
    n_c <- round(rate / (1 - rate) * length(host_rows))
    rows <- c(host_rows, sample(outside, n_c))
  }
  do.call(rbind, lapply(c(FALSE, TRUE), function(recut) {
    g <- stackedGap(rows, labelsFor(rows, host, recut))
    data.frame(host = host, rate = rate, draw = jobs$draw[j],
               labels = if (recut) "recut" else "fixed", n = length(rows),
               contaminants = length(rows) - length(host_rows),
               gap = g[["gap"]], folds = g[["folds"]])
  }))
}, mc.cores = cores, mc.preschedule = FALSE)
failed <- vapply(results, inherits, logical(1), "try-error")
if (any(failed)) stop("Runs failed: ", conditionMessage(attr(results[[which(failed)[1]]], "condition")))
res <- dplyr::bind_rows(results)
message("done in ", format(round(difftime(Sys.time(), started, units = "mins"), 1)))

clean <- res |> dplyr::filter(rate == 0) |> dplyr::select(host, labels, gap_0 = gap)
summ <- res |>
  dplyr::filter(rate > 0) |>
  dplyr::left_join(clean, by = c("host", "labels")) |>
  dplyr::mutate(bias = gap - gap_0) |>
  dplyr::group_by(host, labels, rate) |>
  dplyr::summarise(draws = dplyr::n(), contaminants = dplyr::first(contaminants),
                   mean_bias = mean(bias), sd_bias = stats::sd(bias),
                   se = sd_bias / sqrt(draws),
                   draws_for_se_0.003 = ceiling((sd_bias / 0.003)^2),
                   min_folds = min(folds), .groups = "drop")

print(as.data.frame(clean), digits = 3)
cat("\n")
print(as.data.frame(summ), digits = 3)

if (!is.na(out_path)) {
  readr::write_csv(res, out_path)
  message("wrote ", out_path)
}
