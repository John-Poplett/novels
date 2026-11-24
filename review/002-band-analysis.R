# Feasibility of a 10x expansion: labelled books per class under different
# band and author-cap rules, with a Poisson re-measurement flip rate.
#
# Read-only. Inputs: the 2026-11-07 trial snapshot (30-day counts) and the
# gutenbergr catalogue (LCSH subjects). Genre membership here is an LCSH
# pattern match, a rough stand-in for the allow-list mechanism, so the counts
# are an order-of-magnitude feasibility estimate, not the wave's pool.
#
# Run from the repo root: Rscript review/002-band-analysis.R

suppressMessages(library(dplyr))

downloads <- readr::read_csv("data-raw/snapshots/2026-11-07/downloads.csv.gz",
                             show_col_types = FALSE) |>
  dplyr::select(gutenberg_id, downloads)

meta <- gutenbergr::gutenberg_metadata |>
  dplyr::filter(language == "en", has_text) |>
  dplyr::distinct(gutenberg_id, .keep_all = TRUE) |>
  dplyr::select(gutenberg_id, author_id = gutenberg_author_id)

subjects <- gutenbergr::gutenberg_subjects |>
  dplyr::filter(subject_type == "lcsh", gutenberg_id %in% meta$gutenberg_id)

# Two genre sources: LCSH subject headings (pattern match) or the catalogue's
# "Category:" bookshelves. Choose with GENRE_SOURCE=lcsh|shelf (default both).
lcsh_patterns <- c(
  "Adventure"          = "adventure stories",
  "Detective/Mystery"  = "detective and mystery",
  "Historical Fiction" = "historical fiction",
  "Romance"            = "love stories|romance fiction",
  "Poetry"             = "poetry|poems",
  "Science Fiction"    = "science fiction",
  "Short Stories"      = "short stories"
)
shelf_names <- c(
  "Adventure"          = "Category: Adventure",
  "Detective/Mystery"  = "Category: Crime, Thrillers and Mystery",
  "Historical Fiction" = "Category: Historical Novels",
  "Romance"            = "Category: Romance",
  "Poetry"             = "Category: Poetry",
  "Science Fiction"    = "Category: Science-Fiction & Fantasy",
  "Short Stories"      = "Category: Short Stories"
)

shelves <- gutenbergr::gutenberg_metadata |>
  dplyr::filter(gutenberg_id %in% meta$gutenberg_id, !is.na(gutenberg_bookshelf)) |>
  dplyr::distinct(gutenberg_id, gutenberg_bookshelf) |>
  tidyr::separate_rows(gutenberg_bookshelf, sep = "/")

genreIds <- function(source) {
  if (source == "lcsh") {
    purrr::imap(lcsh_patterns, \(p, g) tibble::tibble(
      gutenberg_id = unique(subjects$gutenberg_id[grepl(p, subjects$subject, ignore.case = TRUE)]),
      genre = g))
  } else {
    purrr::imap(shelf_names, \(s, g) tibble::tibble(
      gutenberg_id = unique(shelves$gutenberg_id[shelves$gutenberg_bookshelf == s]),
      genre = g))
  }
}

makePool <- function(source) {
  genreIds(source) |>
    dplyr::bind_rows() |>
    dplyr::inner_join(meta, by = "gutenberg_id") |>
    dplyr::inner_join(downloads, by = "gutenberg_id") |>
    dplyr::filter(!is.na(author_id), downloads > 0)
}

# Cap an author at `cap` labelled books per genre (as in 2013), deterministically.
capAuthors <- function(df, cap) {
  if (is.infinite(cap)) return(df)
  df |>
    dplyr::arrange(genre, class, author_id, gutenberg_id) |>
    dplyr::group_by(genre, author_id) |>
    dplyr::slice_head(n = cap) |>
    dplyr::ungroup()
}

# One rule: label below the b-quantile FAILURE, above the (1-b)-quantile
# SUCCESS, discard the middle, then cap. b = 0.5 is the median split.
labelUnder <- function(pool, b, cap) {
  pool |>
    dplyr::group_by(genre) |>
    dplyr::mutate(
      c_lo = stats::quantile(downloads, b, type = 1),
      c_hi = stats::quantile(downloads, 1 - b, type = 1),
      class = dplyr::case_when(
        downloads <= c_lo & downloads < c_hi ~ "FAILURE",
        downloads >= c_hi & downloads > c_lo ~ "SUCCESS",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(class)) |>
    capAuthors(cap) |>
    dplyr::mutate(
      # chance a re-measurement of the same 30-day count lands in the other
      # class: Poisson is a lower bound, real dispersion is larger
      flip = dplyr::if_else(
        class == "FAILURE",
        stats::ppois(c_hi - 1, downloads, lower.tail = FALSE),
        stats::ppois(c_lo, downloads)
      )
    ) |>
    dplyr::group_by(genre) |>
    dplyr::summarise(
      band = sprintf("%d/%d", round(100 * b), round(100 * (1 - b))),
      cap = cap,
      c_lo = dplyr::first(c_lo), c_hi = dplyr::first(c_hi),
      noise_ok = dplyr::first(c_hi - c_lo >= 4 * sqrt(c_lo)),
      n_fail = sum(class == "FAILURE"), n_succ = sum(class == "SUCCESS"),
      per_class = min(n_fail, n_succ),
      flip_rate = mean(flip),
      .groups = "drop"
    )
}

report <- function(source) {
  pool <- makePool(source)
  cat("\n==================== genre source:", source, "====================\n")
  grid <- tidyr::expand_grid(b = c(0.2, 0.3, 0.4, 0.5), cap = c(2, 3, Inf))
  results <- purrr::pmap(grid, \(b, cap) labelUnder(pool, b, cap)) |> dplyr::bind_rows()

  cat("Pool (books with a 30-day count > 0, before any rule):\n")
  print(pool |> dplyr::count(genre, name = "books") |>
          dplyr::left_join(pool |> dplyr::group_by(genre) |>
                             dplyr::summarise(authors = dplyr::n_distinct(author_id),
                                              median_dl = stats::median(downloads)),
                           by = "genre"))

  cat("\nBooks per class (min of the two classes), by band x cap:\n")
  print(results |>
          dplyr::mutate(cap = ifelse(is.infinite(cap), "none", as.character(cap))) |>
          dplyr::select(genre, band, cap, per_class) |>
          tidyr::pivot_wider(names_from = c(band, cap), values_from = per_class,
                             names_sep = " cap "),
        width = 200)

  cat("\nCuts, noise rule and Poisson flip rate at cap 3:\n")
  print(results |> dplyr::filter(cap == 3) |>
          dplyr::select(genre, band, c_lo, c_hi, noise_ok, flip_rate) |>
          dplyr::mutate(flip_rate = signif(flip_rate, 2)),
        n = Inf)

  cat("\nGenres reaching 500 per class:\n")
  print(results |> dplyr::filter(per_class >= 500) |>
          dplyr::count(band, cap, name = "genres_at_500"))
  invisible(results)
}

sources <- Sys.getenv("GENRE_SOURCE", "lcsh,shelf")
for (src in intersect(strsplit(sources, ",")[[1]], c("lcsh", "shelf"))) report(src)

# --- Learning-curve extrapolation (data-raw/learning-curve.csv) -------------
# AUC ~ log(n_train) per alpha, projected to 5x and 10x the full corpus.
# Four points over a 4x range: an indication, not a forecast.
lc <- readr::read_csv("data-raw/learning-curve.csv", show_col_types = FALSE) |>
  dplyr::group_by(alpha, fraction) |>
  dplyr::summarise(n = mean(n_train_books), auc = mean(auc), .groups = "drop")
cat("\nLearning-curve projection (log-linear fit per alpha):\n")
print(lc |>
        dplyr::group_by(alpha) |>
        dplyr::group_modify(\(d, k) {
          fit <- stats::lm(auc ~ log(n), data = d)
          n_full <- max(d$n)
          tibble::tibble(slope_per_doubling = unname(stats::coef(fit)[2]) * log(2),
                         auc_full = d$auc[which.max(d$n)],
                         auc_5x = unname(stats::predict(fit, data.frame(n = 5 * n_full))),
                         auc_10x = unname(stats::predict(fit, data.frame(n = 10 * n_full))))
        }) |>
        dplyr::ungroup() |>
        dplyr::mutate(dplyr::across(-alpha, \(x) round(x, 3))))

# --- How the 30-day counter relates to the 2013 label ----------------------
d <- readr::read_csv("data-raw/snapshots/2026-11-07/downloads.csv.gz", show_col_types = FALSE)
cat("\n30-day counter, all records, quantiles:\n")
print(stats::quantile(d$downloads, c(.01, .05, .1, .2, .5, .8, .9, .99)))
if (file.exists("data/novels.rda")) {
  load("data/novels.rda")
  b <- novels |>
    dplyr::distinct(gutenberg_id, download_count, response) |>
    dplyr::inner_join(d, by = "gutenberg_id")
  cat("\n2013 corpus books:", nrow(b),
      " Spearman(2013 cumulative, 2026-11-07 30-day) =",
      round(stats::cor(b$download_count, b$downloads, method = "spearman"), 3),
      " AUC of the 30-day count for the 2013 label =",
      round(as.numeric(pROC::auc(b$response, b$downloads, quiet = TRUE)), 3), "\n")
}
