# Freezing the catalogue that fixes wave eligibility (PREREGISTRATION.md
# sections 3 and 13, and the 2026-11-20 amendment in section 11).

# The catalogue must have been regenerated on or after this date...
CATALOGUE_FREEZE_DATE <- as.Date("2026-12-31")
# ...and retrieved no later than this one; after it, the wave is revisited.
CATALOGUE_FREEZE_DEADLINE <- as.Date("2027-01-31")

# pg_catalog.csv's header, as served 2026-11 (checked on every freeze).
CATALOGUE_COLUMNS <- c("Text#", "Type", "Issued", "Title", "Language",
                       "Authors", "Subjects", "LoCC", "Bookshelves")

# 79,762 RDF records on 2026-11-07; a collapse means a changed feed.
CATALOGUE_MIN_ROWS <- 70000L

#' May the catalogue be frozen from this retrieval?
#'
#' Enforces the 2026-11-20 amendment: the catalogue counts only if Project
#' Gutenberg regenerated it on or after 2026-12-31 (by `Last-Modified`, in UTC)
#' and it is retrieved by 2027-01-31. A missing `Last-Modified` is refused,
#' because the catalogue's date then cannot be shown.
#'
#' @param last_modified POSIXct, from [feedLastModified()]
#' @param run_date the retrieval date (UTC)
#' @returns `TRUE` invisibly; otherwise an error saying which rule failed
#' @noRd
checkCatalogueTiming <- function(last_modified, run_date) {
  stopifnot(
    "last_modified must be a single POSIXct" =
      inherits(last_modified, "POSIXct") && length(last_modified) == 1,
    "run_date must be a single non-missing Date" =
      inherits(run_date, "Date") && length(run_date) == 1 && !is.na(run_date)
  )
  if (run_date < CATALOGUE_FREEZE_DATE) {
    stop("It is ", format(run_date), "; the catalogue is frozen on or after ",
         format(CATALOGUE_FREEZE_DATE), ". Nothing was written. Run again then.",
         call. = FALSE)
  }
  if (run_date > CATALOGUE_FREEZE_DEADLINE) {
    stop("It is ", format(run_date), ", past the ",
         format(CATALOGUE_FREEZE_DEADLINE), " deadline for retrieving the ",
         "catalogue. Under the 2026-11-20 amendment (PREREGISTRATION.md section ",
         "11) the wave's start must be revisited and amended first. Nothing ",
         "was written.", call. = FALSE)
  }
  if (is.na(last_modified)) {
    stop("The catalogue sent no usable Last-Modified header, so it cannot be ",
         "shown to have been regenerated on or after ",
         format(CATALOGUE_FREEZE_DATE), ". Nothing was written. Re-run later.",
         call. = FALSE)
  }
  regenerated <- as.Date(format(last_modified, "%Y-%m-%d", tz = "UTC"))
  if (regenerated < CATALOGUE_FREEZE_DATE) {
    stop("The catalogue was last regenerated ",
         format(last_modified, "%Y-%m-%d %H:%M:%S", tz = "UTC"), " UTC, before ",
         format(CATALOGUE_FREEZE_DATE), ", so it is not the catalogue the ",
         "registration names. Nothing was written. Re-run after Project ",
         "Gutenberg regenerates it; do not use a mirror.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Read and validate a downloaded `pg_catalog.csv.gz`
#'
#' Refuses anything that is not a gzip file, whose header is not
#' [CATALOGUE_COLUMNS], or that has implausibly few rows. Every column is
#' read as character and nothing is treated as missing (readr's default would
#' turn a title "NA" into `NA`), so nothing is reinterpreted on the way in.
#'
#' @param path the downloaded file
#' @param min_rows the fewest rows accepted
#' @returns a tibble of the catalogue, all columns character
#' @noRd
readCatalogue <- function(path, min_rows = CATALOGUE_MIN_ROWS) {
  stopifnot(
    "path must be an existing file" =
      is.character(path) && length(path) == 1 && file.exists(path),
    "min_rows must be a single positive number" =
      is.numeric(min_rows) && length(min_rows) == 1 && min_rows > 0
  )
  magic <- readBin(path, "raw", n = 2L)
  if (!identical(magic, as.raw(c(0x1f, 0x8b)))) {
    stop("The catalogue ", path, " is not a gzip file; Project Gutenberg ",
         "probably served a page in its place. Nothing was written.",
         call. = FALSE)
  }
  catalogue <- readr::read_csv(
    path, col_types = readr::cols(.default = readr::col_character()),
    na = character(), progress = FALSE, show_col_types = FALSE
  )
  if (!identical(names(catalogue), CATALOGUE_COLUMNS)) {
    stop("The catalogue's columns are ", paste(names(catalogue), collapse = ", "),
         ", not the expected ", paste(CATALOGUE_COLUMNS, collapse = ", "),
         ". The format has changed; update the freeze before using it. ",
         "Nothing was written.", call. = FALSE)
  }
  if (nrow(catalogue) < min_rows) {
    stop("The catalogue has ", format(nrow(catalogue), big.mark = ","),
         " rows, fewer than the ", format(min_rows, big.mark = ","),
         " expected; it is probably truncated. Nothing was written.",
         call. = FALSE)
  }
  catalogue
}

#' Headline counts for the freeze's provenance
#'
#' How many books pass the two catalogue rules of section 3: English text, and
#' issued on or before 2026-12-31. Genre assignment, the third rule, happens
#' downstream against the allow-list.
#'
#' @param catalogue from [readCatalogue()]
#' @returns a named integer vector
#' @noRd
catalogueCounts <- function(catalogue) {
  issued <- as.Date(catalogue$Issued, format = "%Y-%m-%d")
  text_en <- catalogue$Type %in% "Text" & catalogue$Language %in% "en"
  c(rows = nrow(catalogue),
    text_en = sum(text_en),
    text_en_issued_by_freeze = sum(text_en & !is.na(issued) &
                                     issued <= CATALOGUE_FREEZE_DATE),
    unparsed_issued = sum(is.na(issued)))
}
