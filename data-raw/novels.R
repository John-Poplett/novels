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
  download.file("https://www3.cs.stonybrook.edu/~songfeng/success/data/novels.7z",
                tmp,
                quiet = TRUE)
  setwd("data-raw")
  system(sprintf("p7zip -d %s", tmp))
  unlink(tmp)
  setwd("..")
}

build_dataset_info <- function(data_path = 'data-raw') {
  meta_data_file_path <- file.path('data-raw', 'novels', 'novel_meta.txt')
  novel_metadata <- as_tibble(data.frame(text = readLines(meta_data_file_path)))

  novel_metadata <- novel_metadata %>% filter(grepl("^(SUCCESS|FAILURE):", text)) %>% mutate(
    gutenberg_id = as.integer(sub("[^0-9]*([0-9]+)\\.txt.*", "\\1", text)),
    download.count = as.integer(sub(".*,DownloadCount: ([0-9]+)$", "\\1", text)),
    date = sub("(?:[^:]+:){4,4}\\D+(\\d{4,4}(?:-\\d{4,4})?).*$", "\\1", text)) %>%
    select(gutenberg_id, date, download.count)

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
    id = as.integer(sub("([0-9]+)\\.txt", "\\1", file.name)),
    dirpart = as.factor(dirname(as.character(text))),
    text = as.character(text))
  df$text <- sapply(df$text, function(x) stringi::stri_enc_toutf8(readr::read_file(file.path('data-raw', 'novels', x))))
  #
  # Strip out gutenberg_bookshelf column since it is incomplete and not useful.
  #
  df <- dplyr::inner_join(df %>% mutate(gutenberg_id = id), gutenberg_metadata %>% select(-gutenberg_bookshelf), by = c("gutenberg_id"))
  df <- dplyr::inner_join(df, novel_metadata, by = c("gutenberg_id"))

  #
  # The last entry in the text file "novels_meta.txt" is truncated and produces an NA in the download.count column.
  # The interpolated value for it is substituted. For now we only scan entries in the "short.stories" genre that
  # have a response value for failure (0). Alternatively, we could have just searched for the last entry with
  # gutenberg_id 23177.
  #
  suspect_genre <- "short.stories"
  suspect_response <- 0
  download_counts <- df %>% filter(genre == suspect_genre, response == suspect_response) %>% select(download.count)
  if(any(is.na(download_counts))) {
    avg <- as.integer(mean(download_counts$download.count, na.rm = TRUE))
    df <- df %>% mutate(download.count = ifelse(genre == suspect_genre && response == suspect_response && is.na(download.count), avg, download.count))
  }

  # Convert to tibble
  as_tibble(df)
}

novels <- build_dataset_info()

names(novels) <- names(novels) %>% stri_enc_toutf8

# Check for complete data
assert_that(all(complete.cases(novels)))

usethis::use_data(novels, overwrite = TRUE)
