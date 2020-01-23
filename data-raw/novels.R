#!/usr/bin/R

library(dplyr, warn.conflicts = FALSE)
library(readr)
library(usethis)
library(tibble)
library(stringi)
library(stringr)
library(purrr)
library(tibble)
library(assertthat)
library(gutenbergr)
library(archive)

genres <- list(
  Adventure_Stories = "adventure",
  Fiction = "fiction",
  Historical_Fiction = "historical.fiction",
  Love_Stories = "love.stories",
  Mystery = "mystery",
  Poetry = "poetry",
  Science_Fiction = "science.fiction",
  Short_Stories = "short.stories"
)

if (!file.exists("data-raw/novels")) {
  tmp <- tempfile(fileext = ".7z")
  download.file("https://www3.cs.stonybrook.edu/~songfeng/success/data/novels.7z", tmp, quiet = TRUE)
  archive_extract(tmp, "data-raw")
  unlink(tmp)
}

build_dataset_info <- function(data_path = 'data-raw') {
  meta_data_file_path <- file.path('data-raw', 'novels', 'novel_meta.txt')
  novel_metadata <- as_tibble(data.frame(text = readLines(meta_data_file_path)))

  novel_metadata <- novel_metadata %>% filter(grepl("^(SUCCESS|FAILURE):", text)) %>% mutate(
    id = as.integer(sub("^(?:SUCCESS|FAILURE):\\s*FileName:\\s+([0-9]+)\\.txt.*", "\\1", text)),
    gutenberg_id = id,
    download.count = as.integer(sub("(?:[^:]+:){5,5}[^,]+,DownloadCount:\\s+(\\d+).*", "\\1", text)),
    date = sub("(?:[^:]+:){4,4}\\D+(\\d{4,4}(?:-\\d{4,4})?).*$", "\\1", text),
    date = ifelse(grepl("^(?:SUCCESS|FAILURE).*", date, perl=TRUE), NA, date)) %>%
    select(id, gutenberg_id, date, download.count)

  files <- list.files(path = file.path(data_path, "novels"), pattern = "[0-9]+\\.txt", recursive = TRUE)
  # Creates a nested list of vectors
  path_elements <- purrr::map(files, function(x) unlist(strsplit(x, .Platform$file.sep)))
  # Convert nested list into a tibble (data.frame)
  df <- do.call(partial(rbind.data.frame, stringsAsFactors = FALSE), path_elements)
  names(df) <- c("genre", "fold", "response", "file.name")
  df <- cbind(df, data.frame(text = files)) %>% mutate(
    genre = as.factor(as.character(genres[genre])),
    fold = as.integer(sub(".*_fold([0-9]+)", "\\1", fold)),
    response = as.integer(grepl("*success*", response)),
    gutenberg_id = as.integer(sub("([0-9]+)\\.txt", "\\1", file.name)),
    dirpart = as.factor(dirname(as.character(text))),
    text = as.character(text))

  #
  # Insert rows with placeholder values for any rows found missing. The copy
  # of novel_metadata.txt retrieved from the Sunnybrook site is corrupt
  # and missing an entire entry.
  #
  missing_download_counts <- setdiff(df$gutenberg_id, novel_metadata$gutenberg_id)
  novel_metadata <- rbind(
    novel_metadata,
    data.frame("id" = missing_download_counts) %>%
      mutate(gutenberg_id = id, date = '', download.count = NA)
  )

  #
  # Strip out any rows that refer to the same document. Perform this ahead of
  # reading in the text for a slight efficiency gain. Do this both for the
  # metadata values derived from novel_metadata.txt and metadata values derived
  # from walking the dataset file structure.
  #
  novel_metadata <- novel_metadata %>% distinct(gutenberg_id, .keep_all = TRUE)
  df <- df %>% distinct(gutenberg_id, .keep_all = TRUE)

  #
  # Now load up the relevant text
  #
  df$text <- sapply(df$text, function(x) stringi::stri_enc_toutf8(readr::read_file(file.path('data-raw', 'novels', x))))

  #
  # Merge in metadata from gutenbergr. Strip out gutenberg_bookshelf column
  # since it is incomplete and not of any obvious use.
  #
  df <- dplyr::inner_join(df, gutenberg_metadata %>% select(-gutenberg_bookshelf), by = c("gutenberg_id"))

  #
  # Merge in metadata values which we read in and parsed from novel_metadata.txt
  #
  df <- dplyr::inner_join(df, novel_metadata, by = c("gutenberg_id"))

  #
  # Compute the mean download count value for a given genre and response
  # type.
  #
  compute_mean <- function(genre_, response_) {
    download.count.vector <-
      (df %>% filter(genre == genre_ & response == response_))$download.count
    as.integer(mean(download.count.vector, na.rm = TRUE))
  }

  # Replace NAs in download count
  df <- df %>% mutate(download.count = ifelse(is.na(download.count),
    compute_mean(genre, response),
    download.count))

  assert_that(!any(is.na(df$download.count)))

  # Convert to tibble
  as_tibble(df)
}

novels <- build_dataset_info()

names(novels) <- names(novels) %>% stri_enc_toutf8

# Check for complete data in all columns but the date
# column.
assert_that(all(complete.cases(novels %>% select(-date))))

usethis::use_data(novels, overwrite = TRUE)
