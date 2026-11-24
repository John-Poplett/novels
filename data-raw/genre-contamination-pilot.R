# Pilot for the genre-classifier qualifier (PREREGISTRATION.md, draft
# amendment of 2026-11-23, "a qualifier for the text genre classifier",
# point 3). It uses only the 2013 genre labels, never a genre classifier.
#
# The question: how noisy is one contaminated draw, and so how many draws
# per rate keep the induced bias's standard error at or below 0.003?
#
# Design 2 (2026-11-23). The 2013 analogue of the wave's Novels stratum is
# the host: books in any of the six novel-form genres (Adventure,
# Detective/Mystery, Fiction, Historical Fiction, Romance, Science Fiction)
# and in neither Poetry nor Short Stories, 561 books. The contaminants are
# what a novel classifier would wrongly admit: books in Poetry or Short
# Stories and in no novel genre, 175. The 18 books in both are left out.
#
# Each draw takes a stratum of --size host books, then replaces the same
# random fraction c of it twice: by other host books (the placebo) and by
# contaminants (contaminated). Both runs have the same size and the same
# books removed, so the disturbance cancels, and
#
#   bias = d(contaminated) - d(placebo)
#
# where d is the stacked comparison of section 7 on the five stored author
# folds: mean over folds of AUC(stacked) - AUC(bibliographic), with
#
#   bibliographic  glm on log10(text length) + log10(date_added) +
#                  log10(gutenberg_id). Genre is left out: the stratum does
#                  not know a contaminant is one.
#   stacked        the same plus one pre-validated TF-IDF glmnet score
#                  (alpha by inner-CV AUC over the pipeline's grid,
#                  cv.glmnet keep = TRUE, inner folds by author)
#
# Two labellings: fixed (every book keeps its 2013 label, a book in two
# genres that of its first) and recut (1 above the run's median download
# count: the 2013 bands have no middle, so the median is the only quantile
# cut both bands share).
#
# Design 1, one genre of about 100 books against a single clean run, was
# abandoned: its "bias" tracked the clean run's own noise (regression to
# the mean), and 100 books is not the scale of the wave. Its results are
# kept in data-raw/pilot/contamination-pilot-v1.csv.
#
# Tokenisation is done once per book: one raw count matrix over the whole
# corpus, unpruned. Each fold then prunes the vocabulary on its own training
# books with the pipeline's rules (R/text2vocab.R: stopwords removed, term
# count >= 10, document proportion between 10 / n and 0.5) and fits TF-IDF
# on them alone, so nothing about a test book reaches its fold's features.
#
# Run from the package root after `tar_make()`:
#
#   Rscript data-raw/genre-contamination-pilot.R [--rates=0.05,0.2]
#       [--draws=10] [--size=400] [--match=none|quintile] [--balance-only]
#       [--cores=12] [--out=FILE]
#
# --match=quintile matches contaminants to the host's length distribution
# (by host length quintile), so that length cannot identify them: a real
# false positive reads like a novel, and is probably novel-length, where a
# random poetry collection is short (median 85 thousand characters against
# the novels' 372; length alone separates the pools at AUC 0.72).
# --balance-only draws 500 strata per rate and reports the length balance
# without fitting anything.

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
cores <- as.integer(arg("cores", "12"))
size <- as.integer(arg("size", "400"))
match <- arg("match", "none")
balance_only <- "--balance-only" %in% commandArgs(trailingOnly = TRUE)
if (!match %in% c("none", "quintile")) stop("--match must be none or quintile.", call. = FALSE)
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
books$log_len_chars <- books$log_len
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

novel_genres <- c("Adventure", "Detective/Mystery", "Fiction", "Historical Fiction",
                  "Romance", "Science Fiction")
is_novel <- vapply(books$genres, function(g) any(g %in% novel_genres), logical(1))
is_other <- vapply(books$genres, function(g) any(g %in% c("Poetry", "Short Stories")),
                   logical(1))
host_pool <- which(is_novel & !is_other)
contaminant_pool <- which(is_other & !is_novel)
message("host pool ", length(host_pool), ", contaminant pool ", length(contaminant_pool),
        ", left out ", sum(is_novel & is_other))
# Length quintiles of the host pool. With --match=quintile a draw removes the
# same number of books from each quintile of its stratum and replaces them
# from the same quintile: by contaminants, or (placebo) by other host books.
# The contaminants then have the host's length distribution, and length
# cannot tell them apart.
breaks <- stats::quantile(books$log_len[host_pool], seq(0, 1, 0.2))
breaks[1] <- -Inf
breaks[6] <- Inf
books$quintile <- as.integer(cut(books$log_len, breaks))
capacity <- table(factor(books$quintile[contaminant_pool], levels = 1:5))
message("contaminants per host length quintile: ", paste(capacity, collapse = " "))
per_q <- function(rate) ceiling(round(rate * size) / 5)
if (match == "quintile" && per_q(max(rates)) > min(capacity)) {
  stop("Rate ", max(rates), " needs ", per_q(max(rates)), " contaminants per length ",
       "quintile at --size ", size, "; the thinnest quintile has ", min(capacity),
       ". Lower the rate or the size.", call. = FALSE)
}
if (size > length(host_pool) - round(max(rates) * size) ||
    round(max(rates) * size) > length(contaminant_pool)) {
  stop("--size ", size, " is too large for the pools at rate ", max(rates), ".", call. = FALSE)
}

# One draw's stratum and its two swaps.
drawSwap <- function(rate) {
  stratum <- sample(host_pool, size)
  n_swap <- round(rate * size)
  if (match == "none") {
    out <- sample(seq_len(size), n_swap)
    placebo_in <- sample(setdiff(host_pool, stratum), n_swap)
    contaminant_in <- sample(contaminant_pool, n_swap)
  } else {
    # n_swap spread over the quintiles as evenly as it divides.
    k <- rep(n_swap %/% 5, 5) + (seq_len(5) <= n_swap %% 5)[sample(5)]
    out <- placebo_in <- contaminant_in <- integer()
    rest <- setdiff(host_pool, stratum)
    for (q in 1:5) {
      if (!k[q]) next
      pick <- function(x) x[sample.int(length(x), k[q])]
      out <- c(out, pick(which(books$quintile[stratum] == q)))
      placebo_in <- c(placebo_in, pick(rest[books$quintile[rest] == q]))
      contaminant_in <- c(contaminant_in,
                          pick(contaminant_pool[books$quintile[contaminant_pool] == q]))
    }
  }
  placebo <- stratum
  placebo[out] <- placebo_in
  contaminated <- stratum
  contaminated[out] <- contaminant_in
  # Balance: how well length tells the newcomers from the books they replace
  # (0.5 = not at all), and contaminants from the rest of their stratum.
  len <- books$log_len
  list(placebo = placebo, contaminated = contaminated, n_swap = n_swap,
       auc_vs_removed = aucRank(c(rep(1, n_swap), rep(0, n_swap)),
                                c(len[contaminant_in], len[stratum[out]])),
       auc_vs_stratum = aucRank(rep(c(1, 0), c(n_swap, size - n_swap)),
                                len[c(contaminant_in, stratum[-out])]))
}

if (balance_only) {
  set.seed(20261123)
  bal <- do.call(rbind, lapply(rep(rates, each = 500), function(r) {
    d <- drawSwap(r)
    data.frame(rate = r, auc_vs_removed = d$auc_vs_removed, auc_vs_stratum = d$auc_vs_stratum)
  }))
  cat("Length balance, match =", match, ", size =", size, ", 500 draws per rate\n")
  print(as.data.frame(dplyr::summarise(dplyr::group_by(bal, rate),
    vs_removed_mean = mean(auc_vs_removed), vs_removed_q05 = stats::quantile(auc_vs_removed, 0.05),
    vs_removed_q95 = stats::quantile(auc_vs_removed, 0.95),
    vs_stratum_mean = mean(auc_vs_stratum), vs_stratum_q05 = stats::quantile(auc_vs_stratum, 0.05),
    vs_stratum_q95 = stats::quantile(auc_vs_stratum, 0.95))), digits = 3)
  quit(status = 0)
}

labelsFor <- function(rows, recut) {
  if (recut) {
    dc <- books$download_count[rows]
    return(as.integer(rank(dc, ties.method = "first") > length(dc) / 2))
  }
  vapply(rows, function(r) books$response[[r]][[1]], integer(1))
}

jobs <- expand.grid(rate = rates, draw = seq_len(draws))
message(nrow(jobs), " draws, 2 runs each, on ", cores, " cores")
started <- Sys.time()
results <- parallel::mclapply(seq_len(nrow(jobs)), function(j) {
  set.seed(20261123 + j)
  rate <- jobs$rate[j]
  d <- drawSwap(rate)
  placebo <- d$placebo
  contaminated <- d$contaminated
  n_swap <- d$n_swap
  do.call(rbind, lapply(c(FALSE, TRUE), function(recut) {
    gp <- stackedGap(placebo, labelsFor(placebo, recut))
    gc <- stackedGap(contaminated, labelsFor(contaminated, recut))
    data.frame(rate = rate, draw = jobs$draw[j], labels = if (recut) "recut" else "fixed",
               n = size, swapped = n_swap, match = match,
               auc_len_vs_removed = d$auc_vs_removed, auc_len_vs_stratum = d$auc_vs_stratum,
               gap_placebo = gp[["gap"]],
               gap_contaminated = gc[["gap"]], bias = gc[["gap"]] - gp[["gap"]],
               folds = min(gp[["folds"]], gc[["folds"]]))
  }))
}, mc.cores = cores, mc.preschedule = FALSE)
failed <- vapply(results, inherits, logical(1), "try-error")
if (any(failed)) stop("Runs failed: ", conditionMessage(attr(results[[which(failed)[1]]], "condition")))
res <- dplyr::bind_rows(results)
message("done in ", format(round(difftime(Sys.time(), started, units = "mins"), 1)))

summ <- res |>
  dplyr::group_by(labels, rate) |>
  dplyr::summarise(draws = dplyr::n(), swapped = dplyr::first(swapped),
                   mean_bias = mean(bias), sd_bias = stats::sd(bias),
                   se = sd_bias / sqrt(draws),
                   draws_for_se_0.003 = ceiling((sd_bias / 0.003)^2),
                   mean_gap_placebo = mean(gap_placebo),
                   len_auc_vs_stratum = mean(auc_len_vs_stratum), min_folds = min(folds),
                   .groups = "drop")
print(as.data.frame(summ), digits = 3)

if (!is.na(out_path)) {
  readr::write_csv(res, out_path)
  message("wrote ", out_path)
}
