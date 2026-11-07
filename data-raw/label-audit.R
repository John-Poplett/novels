# Label-quality audit for the novels corpus.
#
# "Is this label good enough to learn from?" is five separate questions, and
# this script answers all five on one set of author-disjoint folds so that a
# candidate label can be compared with the shipped one on identical footing.
#
#   1. artefacts      is the count a plausible readership measure at all?
#   2. stability      do labels survive resampling the corpus?
#   3. reliability    would a second measurement produce the same label?
#   4. predictability how much of the label is non-text (bibliography)?
#   5. agreement      does an independent source rank the same books?
#
# Tests 3 and 5 need data this repository does not yet have (repeat snapshots
# of one source, and a second source). Both accept a data frame and skip with
# a stated reason when it is absent, so adding the data later needs no code
# change. Test 4 is the one specific to this project: the paper's claim is
# that *style* predicts success, so a label that a handful of bibliographic
# columns predict just as well is a label about era and format, not writing.
#
# Run from the package root after `tar_make()`:
#
#   Rscript data-raw/label-audit.R                 # shipped 2013 label
#   Rscript data-raw/label-audit.R --text          # also fit the TF-IDF model
#   Rscript data-raw/label-audit.R --out=audit.csv # write the summary
#
# The text model adds roughly two minutes and is off by default: the headline
# number is the bibliographic baseline, and that one is cheap.

suppressMessages({
  library(dplyr)
  library(readr)
  library(targets)
})
pkgload::load_all(quiet = TRUE)

# ---------------------------------------------------------------- helpers ---

# Rank-based AUC (Mann-Whitney), so no package decides the `direction` for us
# and tied predictions score 0.5 rather than depending on input order.
aucRank <- function(truth, score) {
  stopifnot(length(truth) == length(score), all(truth %in% c(0, 1)))
  n1 <- sum(truth == 1)
  n0 <- sum(truth == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(score)
  (sum(r[truth == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# The paper's fold column is not author-disjoint (85 of 514 authors appear in
# more than one fold), so every number below uses the stored author-grouped
# table instead. Fails loudly rather than silently dropping unmatched rows.
withAuthorFolds <- function(data, path = "inst/extdata/author_folds.csv") {
  folds <- readr::read_csv(path, show_col_types = FALSE)
  fold <- folds$fold[match(data$author, folds$author)]
  missing <- unique(data$author[is.na(fold)])
  if (length(missing) > 0) {
    stop(
      "no fold for ", length(missing), " author(s): ",
      paste(utils::head(missing, 3), collapse = "; "),
      ". Rebuild inst/extdata/author_folds.csv with data-raw/author-folds.R.",
      call. = FALSE
    )
  }
  data$afold <- fold
  # The property every AUC below relies on. If it ever stops holding, the
  # numbers would silently become author-leaky rather than wrong-looking.
  spanning <- tapply(data$afold, data$author, function(z) length(unique(z)))
  if (any(spanning != 1)) {
    stop(sum(spanning != 1), " author(s) span more than one fold; ",
         "inst/extdata/author_folds.csv is not author-disjoint.", call. = FALSE)
  }
  data
}

# --------------------------------------------------------- 1. artefacts ------

# Descriptive, not pass/fail: it says whether the counter behaves like
# readership or like a scrape artefact. `text_chars` and `gutenberg_id` are
# passed separately so the same function works for a future snapshot that has
# neither.
auditArtefacts <- function(downloads, text_chars = NULL, gutenberg_id = NULL,
                           titles = NULL) {
  stopifnot(is.numeric(downloads), all(downloads >= 0))
  total <- sum(downloads)
  top_share <- if (total > 0) {
    k <- max(1L, ceiling(0.1 * length(downloads)))
    sum(sort(downloads, decreasing = TRUE)[seq_len(k)]) / total
  } else {
    NA_real_
  }
  out <- list(
    n = length(downloads),
    zero = sum(downloads == 0),
    under_5 = sum(downloads < 5),
    under_30 = sum(downloads < 30),
    median = stats::median(downloads),
    mean = mean(downloads),
    max = max(downloads),
    distinct = length(unique(downloads)),
    ties_at_median = sum(downloads == stats::median(downloads)),
    top_decile_share = top_share,
    gini = {
      x <- sort(downloads)
      n <- length(x)
      if (total > 0) (2 * sum(seq_len(n) * x) / (n * total)) - (n + 1) / n else NA_real_
    },
    duplicate_titles = if (is.null(titles)) NA_integer_ else sum(duplicated(tolower(titles))),
    cor_log_length = if (is.null(text_chars)) NA_real_ else
      stats::cor(log1p(downloads), log10(pmax(text_chars, 1))),
    cor_log_id = if (is.null(gutenberg_id)) NA_real_ else
      stats::cor(log1p(downloads), log10(gutenberg_id))
  )
  tibble::as_tibble(out)
}

# --------------------------------------------------------- 2. stability ------

# Does the label survive replacing the corpus with a resample of itself? The
# rule is re-derived on each resample, because a fixed threshold applied to a
# different corpus is not the same rule. `rule` takes the count vector and
# returns 0/1, so the median split and any quantile trim can both be audited.
auditStability <- function(downloads, rule, reps = 200L, seed = 1L) {
  base <- rule(downloads)
  set.seed(seed)
  flips <- vapply(seq_len(reps), function(i) {
    idx <- sample.int(length(downloads), replace = TRUE)
    # Compared book by book, not position by position: `idx` is the book
    # identity, so the resampled label for book `idx[j]` is compared with that
    # book's original label.
    mean(rule(downloads[idx]) != base[idx], na.rm = TRUE)
  }, numeric(1))
  list(
    base_success_rate = mean(base == 1),
    mean_flip = mean(flips),
    sd_flip = stats::sd(flips),
    p95_flip = stats::quantile(flips, 0.95, names = FALSE),
    reps = reps
  )
}

# ------------------------------------------------------- 3. reliability ------

# How far from a cut must a book sit before counting noise cannot move it?
# For Poisson counts the standard error grows with the count, so the answer is
# a curve in `lambda / cut`, not a property of the corpus. Returns the
# probability that one measurement lands on the wrong side of the cut.
noiseFlipCurve <- function(ratios = c(0.5, 0.7, 0.85, 1, 1.15, 1.3, 1.5, 2)) {
  # Lambda expressed as a multiple of the cut, evaluated at cut = 100 so the
  # numbers are counts, not probabilities of probabilities. A book flips when
  # the measurement lands on the far side of the cut, so the tail is chosen by
  # which side the true rate is on.
  cut <- 100
  lambda <- ratios * cut
  flip <- ifelse(
    lambda < cut,
    stats::pnorm(cut, mean = lambda, sd = sqrt(lambda), lower.tail = FALSE),
    stats::pnorm(cut, mean = lambda, sd = sqrt(lambda), lower.tail = TRUE)
  )
  tibble::tibble(lambda_over_cut = ratios, lambda = lambda, flip = flip)
}

# With repeated measurements of the same books, labels can be checked directly
# instead of modelled. `repeats` is a books x snapshots matrix of counts.
auditRepeatAgreement <- function(repeats, rule) {
  labels <- apply(repeats, 2, rule)
  # Unanimity is the strong claim: every snapshot gives this book the same
  # label. Mean pairwise agreement is the weaker, more forgiving version.
  unanimous <- mean(apply(labels, 1, function(z) length(unique(z)) == 1))
  pairs <- utils::combn(ncol(labels), 2)
  pairwise <- mean(apply(pairs, 2, function(p) mean(labels[, p[1]] == labels[, p[2]])))
  list(snapshots = ncol(labels), books = nrow(labels),
       unanimous = unanimous, pairwise = pairwise)
}

# ---------------------------------------------------- 4. predictability ------

# Fit a logistic model on author-disjoint folds and return per-fold AUC. Two
# models, reported separately: `bibliographic` uses only what the library
# catalogue knows (genre, length, upload order), and `author` adds author
# productivity, which is close to author identity for prolific authors and so
# cannot be the headline number -- it would not generalise to a new writer.
auditPredictability <- function(data, formula, folds = data$afold) {
  X <- stats::model.matrix(formula, data = data)
  y <- data$response
  per_fold <- vapply(sort(unique(folds)), function(k) {
    train <- folds != k
    fit <- suppressWarnings(stats::glm.fit(X[train, , drop = FALSE], y[train],
                                           family = stats::binomial()))
    aucRank(y[!train], as.vector(X[!train, , drop = FALSE] %*% fit$coefficients))
  }, numeric(1))
  list(per_fold = per_fold, mean = mean(per_fold), sd = stats::sd(per_fold),
       formula = paste(deparse(formula), collapse = " "))
}

# ---------------------------------------------------------- 5. agreement -----

# Two labels for the same books, from independent sources. Reports both a rank
# correlation (does the second source order books the same way?) and Cohen's
# kappa (do the binary decisions agree beyond chance?). AUC of one binary
# label against the other's count is the same statistic as `aucRank` with the
# roles swapped, which is why it is not repeated here.
auditAgreement <- function(count_a, count_b, rule = medianRule) {
  stopifnot(length(count_a) == length(count_b), !anyNA(count_a), !anyNA(count_b))
  a <- rule(count_a)
  b <- rule(count_b)
  tab <- table(factor(a, 0:1), factor(b, 0:1))
  po <- sum(diag(tab)) / sum(tab)
  pe <- sum(rowSums(tab) * colSums(tab)) / sum(tab)^2
  list(n = length(a), spearman = stats::cor(count_a, count_b, method = "spearman"),
       kappa = if (pe < 1) (po - pe) / (1 - pe) else NA_real_,
       agreement = po)
}

# --------------------------------------------------------------- labels -----

medianRule <- function(x) as.integer(x > stats::median(x))

# Trim the middle and label only the extremes, which is what the original
# dataset did per genre (threshold bands in the paper's Table 1).
quantileRule <- function(x, lower = 0.25, upper = 0.75) {
  q <- stats::quantile(x, c(lower, upper), names = FALSE)
  ifelse(x <= q[1], 0L, ifelse(x >= q[2], 1L, NA_integer_))
}

# For a cut at level c the ambiguous band is c +/- 2 standard errors. How many
# books fall in it is the practical cost of putting a cut at that level, and it
# is computable from the corpus alone, without knowing the future rate.
bandAround <- function(downloads, level) {
  half_width <- 2 * sqrt(level)
  sum(abs(downloads - level) <= half_width)
}

# --------------------------------------------------------------- report -----

section <- function(title) cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")

fmt <- function(x, digits = 3) formatC(x, format = "f", digits = digits)

main <- function(use_text = FALSE, out_path = NULL) {
  n <- withAuthorFolds(targets::tar_read(novels))

  # Book-level view for the count-based tests: `novels` has one row per
  # (book, genre), so 42 books appear twice and counting rows would weight
  # them twice.
  books <- n[!duplicated(n$gutenberg_id), ]

  section("0. Corpus")
  cat(sprintf("rows %d over %d books, %d authors, %d genres\n",
              nrow(n), nrow(books), length(unique(n$author)), length(unique(n$genre))))
  cat(sprintf("label balance %.3f (SUCCESS share), by construction 0.5 per genre\n",
              mean(books$response)))

  section("1. Artefacts")
  art <- auditArtefacts(
    downloads = books$download_count,
    text_chars = nchar(books$text),
    gutenberg_id = books$gutenberg_id,
    titles = books$title
  )
  cat(sprintf("zero: %d   <5: %d   <30: %d   of %d\n",
              art$zero, art$under_5, art$under_30, art$n))
  cat(sprintf("median %.1f   mean %.1f   max %.0f   distinct values %d\n",
              art$median, art$mean, art$max, art$distinct))
  cat(sprintf("top decile holds %.1f%% of all downloads   Gini %.3f\n",
              100 * art$top_decile_share, art$gini))
  cat(sprintf("cor(log downloads, log length) %s   cor(log downloads, log id) %s\n",
              fmt(art$cor_log_length), fmt(art$cor_log_id)))
  cat(sprintf("duplicate titles: %d\n", art$duplicate_titles))
  cat("Reading: a long right tail plus a -0.5 id correlation means the count\n")
  cat("mixes readership with how long the book has been downloadable.\n")

  section("2. Stability under resampling (median split)")
  stab <- auditStability(books$download_count, medianRule)
  cat(sprintf("books relabelled: mean %.2f%%  sd %.2f  p95 %.2f  (%d resamples)\n",
              100 * stab$mean_flip, 100 * stab$sd_flip, 100 * stab$p95_flip,
              stab$reps))
  cat(sprintf("median is %.1f with %d tie(s), so the rule itself is unambiguous\n",
              art$median, art$ties_at_median))

  section("3. Reliability: can counting noise move the label?")
  cat("Probability that one measurement falls on the wrong side of a cut,\n")
  cat("for a book whose true rate is that multiple of the cut:\n")
  noise <- noiseFlipCurve()
  for (i in seq_len(nrow(noise))) {
    cat(sprintf("  lambda = %4.2f x cut  ->  flip probability %.3f\n",
                noise$lambda_over_cut[i], noise$flip[i]))
  }
  cat("So a book whose true rate equals the cut is a coin flip, one 15% away\n")
  cat("from it is wrong about 5-8% of the time, and one 50% away is safe. The\n")
  cat("band is scale-free: what matters is the book's rate relative to the cut,\n")
  cat("not the absolute size of the cut. The archived counts are cumulative and\n")
  cat("cannot be used directly to estimate today's 30-day rate, which is why this\n")
  cat("is a curve rather than a corpus statistic.\n")

  section("4. Non-text predictability (author-disjoint folds)")
  n$log_len <- log10(nchar(n$text))
  n$log_id <- log10(n$gutenberg_id)
  books_per_author <- table(books$author)
  n$author_books <- log10(as.integer(books_per_author[n$author]))

  biblio <- auditPredictability(n, ~ genre + log_len + log_id)
  author <- auditPredictability(n, ~ genre + log_len + log_id + author_books)
  cat(sprintf("bibliographic only           AUC %s   per fold %s\n",
              fmt(biblio$mean), paste(fmt(biblio$per_fold, 2), collapse = " ")))
  cat(sprintf("+ author productivity        AUC %s   per fold %s\n",
              fmt(author$mean), paste(fmt(author$per_fold, 2), collapse = " ")))
  cat(sprintf("increment from knowing who wrote it: %+.3f\n", author$mean - biblio$mean))

  if (use_text) {
    cr <- readr::read_csv("baselines/cv_results.csv", show_col_types = FALSE)
    by_alpha <- tapply(cr$auc, cr$alpha, mean)
    best_alpha <- as.numeric(names(by_alpha)[which.max(by_alpha)])
    text_auc <- cr$auc[cr$alpha == best_alpha]
    d <- biblio$per_fold - text_auc
    cat(sprintf("TF-IDF + elastic net        AUC %s   (best alpha %.2f, frozen fold column)\n",
                fmt(mean(text_auc)), best_alpha))
    cat(sprintf("bibliographic minus text, paired by fold: %+.3f (sd %.3f)\n",
                mean(d), stats::sd(d)))
    cat("Note: the stored text baselines use the frozen `fold` column, which is\n")
    cat("not author-disjoint, so that comparison is indicative only. Re-run the\n")
    cat("sweep on `afold` before quoting it.\n")
  }

  section("5. Agreement with an independent source")
  cat("Skipped: no second source is loaded. To run it, supply a table of\n")
  cat("gutenberg_id -> count from Open Library ratings and call\n")
  cat("auditAgreement(corpus_counts, other_counts).\n")
  cat("Goodreads is not an option for a redistributable dataset: its API closed\n")
  cat("in December 2020 and scraping is against its terms.\n")

  section("6. Where to put a cut")
  cat("Books within +/-2 Poisson standard errors of a candidate cut, on the\n")
  cat("shipped counts. This is where a cut is decided by noise rather than by\n")
  cat("the book. The levels are cumulative counts, so the absolute percentages\n")
  cat("are indicative; the ranking of positions is what the table is for.\n")
  candidates <- tibble::tibble(
    rule = c("median", "25th pct", "75th pct", "90th pct"),
    level = c(stats::median(books$download_count),
              stats::quantile(books$download_count, c(0.25, 0.75, 0.9), names = FALSE))
  )
  candidates$ambiguous <- vapply(candidates$level, function(l) bandAround(books$download_count, l),
                                 numeric(1))
  candidates$share <- candidates$ambiguous / nrow(books)
  for (i in seq_len(nrow(candidates))) {
    cat(sprintf("  %-9s at %6.1f  ->  %3d books (%.1f%%) within the band\n",
                candidates$rule[i], candidates$level[i],
                candidates$ambiguous[i], 100 * candidates$share[i]))
  }
  cat("The median sits where the corpus is densest, which is the worst place\n")
  cat("for a cut. Trimming the middle and labelling only the extremes moves at\n")
  cat("least one cut into a sparser region -- that is the case for reproducing\n")
  cat("the paper's per-genre threshold bands in an expanded dataset.\n")

  if (!is.null(out_path)) {
    summary <- dplyr::bind_rows(
      tibble::tibble(test = "artefacts", metric = names(art),
                     value = as.numeric(unlist(art))),
      tibble::tibble(test = "stability", metric = c("mean_flip", "sd_flip", "p95_flip"),
                     value = c(stab$mean_flip, stab$sd_flip, stab$p95_flip)),
      tibble::tibble(test = "predictability",
                     metric = c("bibliographic_auc", "author_auc", "increment"),
                     value = c(biblio$mean, author$mean, author$mean - biblio$mean))
    )
    readr::write_csv(summary, out_path)
    cat("\nsummary written to", out_path, "\n")
  }

  invisible(list(artefacts = art, stability = stab, biblio = biblio, author = author))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  out <- sub("^--out=", "", grep("^--out=", args, value = TRUE))
  main(
    use_text = "--text" %in% args,
    out_path = if (length(out) == 1 && nzchar(out)) out else NULL
  )
}
