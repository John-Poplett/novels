# The download counter's floor (2026-11-23, exploratory). Project
# Gutenberg's trailing 30-day count now gives about 400 downloads a month
# to books that, in the Standardized Project Gutenberg Corpus's 2018-07-18
# metadata (Gerlach & Font-Clos; Zenodo 10.5281/zenodo.2422561), had none:
# almost certainly automated traffic. This script asks whether that floor is
# predictable per book or is noise.
#
# Calibration set: books with downloads == 0 in the 2018 metadata that are
# in the 2026-11-07 snapshot. Their 2026 count is taken as floor alone (a
# book nobody read in 2018 is assumed to have few readers now; any that
# have some make the floor look more predictable, not less). A second set,
# books with 1-2 downloads in 2018, checks the fitted floor out of sample.
#
# Covariates, from the catalogue only: log10(gutenberg_id), the year added,
# Type, English or not, counts of subjects, bookshelves and authors, and
# log10(title length). Model: lm of log(count) on them; reported in-sample
# R^2 and 10-fold cross-validated R^2.
#
#   Rscript data-raw/floor-calibration.R --spgc=SPGC-metadata-2018-07-18.csv \
#       --catalogue=pg_catalog.csv.gz

suppressMessages(library(dplyr))
pkgload::load_all(quiet = TRUE)

arg <- function(name) {
  v <- sub(paste0("^--", name, "="), "", grep(paste0("^--", name, "="),
                                               commandArgs(trailingOnly = TRUE), value = TRUE))
  if (length(v) != 1 || !file.exists(v)) {
    stop("--", name, "= must name an existing file.", call. = FALSE)
  }
  v
}
spgc_path <- arg("spgc")
catalogue_path <- arg("catalogue")
spgc_md5 <- digest::digest(spgc_path, algo = "md5", file = TRUE)
if (spgc_md5 != "a2d5f325f13846cbec2fd21d982b4ef4") {
  stop("The SPGC metadata's md5 is ", spgc_md5, ", not Zenodo's ",
       "a2d5f325f13846cbec2fd21d982b4ef4; re-download it.", call. = FALSE)
}

spgc <- readr::read_csv(spgc_path, show_col_types = FALSE) |>
  dplyr::transmute(gutenberg_id = suppressWarnings(as.integer(sub("^PG", "", id))),
                   d2018 = downloads) |>
  dplyr::filter(!is.na(gutenberg_id), !is.na(d2018))
snap <- readr::read_csv("data-raw/snapshots/2026-11-07/downloads.csv.gz",
                        show_col_types = FALSE) |>
  dplyr::select(gutenberg_id, d2026 = downloads)
added <- readr::read_csv("data-raw/snapshots/2026-11-07/date_added.csv.gz",
                         show_col_types = FALSE)
cat_raw <- readCatalogue(catalogue_path)
count_parts <- function(x, sep) ifelse(is.na(x) | !nzchar(x), 0L, lengths(strsplit(x, sep, fixed = TRUE)))
catalogue <- tibble::tibble(
  gutenberg_id = as.integer(cat_raw$`Text#`),
  type = cat_raw$Type,
  english = cat_raw$Language == "en",
  n_subjects = count_parts(cat_raw$Subjects, "; "),
  n_shelves = count_parts(cat_raw$Bookshelves, "; "),
  n_authors = count_parts(cat_raw$Authors, "; "),
  log_title = log10(pmax(nchar(cat_raw$Title), 1))
)

d <- snap |>
  dplyr::inner_join(spgc, by = "gutenberg_id") |>
  dplyr::inner_join(added, by = "gutenberg_id") |>
  dplyr::inner_join(catalogue, by = "gutenberg_id") |>
  dplyr::mutate(log_id = log10(gutenberg_id),
                year_added = as.numeric(format(as.Date(date_added), "%Y")),
                text = type == "Text")

describe <- function(x) {
  c(n = length(x), mean = mean(x), median = stats::median(x), sd = stats::sd(x),
    q10 = unname(stats::quantile(x, 0.1)), q90 = unname(stats::quantile(x, 0.9)),
    poisson_sd = sqrt(mean(x)), dispersion = stats::var(x) / mean(x))
}
zero <- dplyr::filter(d, d2018 == 0, d2026 > 0)
low <- dplyr::filter(d, d2018 %in% 1:2, d2026 > 0)
cat("Books in the 2026 snapshot, the 2018 metadata and the catalogue:", nrow(d), "\n")
cat("2026 count of books with 0 downloads in 2018 (zero counts in 2026 dropped: ",
    sum(d$d2018 == 0 & d$d2026 == 0), ")\n", sep = "")
print(round(describe(zero$d2026), 1))
cat("Type of those books:\n"); print(table(zero$type))

form <- log(d2026) ~ log_id + year_added + text + english + n_subjects + n_shelves +
  n_authors + log_title
fit <- stats::lm(form, data = zero)
cat("\nFloor model on the calibration set (log count):\n")
print(round(summary(fit)$coefficients, 4))
cat("In-sample R^2:", round(summary(fit)$r.squared, 3), "\n")

set.seed(20261123)
fold <- sample(rep(1:10, length.out = nrow(zero)))
pred <- numeric(nrow(zero))
for (k in 1:10) {
  f <- stats::lm(form, data = zero[fold != k, ])
  pred[fold == k] <- stats::predict(f, zero[fold == k, ])
}
y <- log(zero$d2026)
cat("10-fold cross-validated R^2:", round(1 - sum((y - pred)^2) / sum((y - mean(y))^2), 3), "\n")

y_low <- log(low$d2026)
p_low <- stats::predict(fit, low)
cat("\nOut of sample, books with 1-2 downloads in 2018 (n = ", nrow(low), "):\n", sep = "")
cat("  R^2 of the fitted floor:", round(1 - sum((y_low - p_low)^2) / sum((y_low - mean(y_low))^2), 3),
    "  Spearman:", round(stats::cor(y_low, p_low, method = "spearman"), 3),
    "  mean residual (log):", round(mean(y_low - p_low), 3), "\n")

# The wave's books are English texts (section 3): the same model on them.
zt <- dplyr::filter(zero, text, english)
form_t <- log(d2026) ~ log_id + year_added + n_subjects + n_shelves + n_authors + log_title
fit_t <- stats::lm(form_t, data = zt)
fold_t <- sample(rep(1:10, length.out = nrow(zt)))
pred_t <- numeric(nrow(zt))
for (k in 1:10) {
  pred_t[fold_t == k] <- stats::predict(stats::lm(form_t, data = zt[fold_t != k, ]),
                                        zt[fold_t == k, ])
}
y_t <- log(zt$d2026)
cat("\nEnglish texts only (n = ", nrow(zt), "):\n", sep = "")
print(round(describe(zt$d2026), 1))
print(round(summary(fit_t)$coefficients, 4))
cat("In-sample R^2:", round(summary(fit_t)$r.squared, 3),
    "  10-fold cross-validated R^2:",
    round(1 - sum((y_t - pred_t)^2) / sum((y_t - mean(y_t))^2), 3), "\n")
