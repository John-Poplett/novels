library(dplyr)
library(readr)
library(usethis)
library(tibble)
library(stringi)
library(stringr)

genres <- list(Adventure_Stories="adventure", Fiction="fiction", Historical_Fiction="historical.fiction",
	       Love_Stories="love.story", Mystery="mystery", Poetry="poetry",
	       Science_Fiction="science.fiction", Short_Stories="short.stories")

if (!file.exists("data-raw/novels")) {
  tmp <- tempfile(fileext = ".7z")
  download.file("https://www3.cs.stonybrook.edu/~songfeng/success/data/novels.7z", tmp, quiet = TRUE)
  setwd("data-raw")
  system(sprintf("p7zip -d %s", tmp))
  unlink(tmp)
  setwd("..")
}

root.dir <- file.path('data-raw', 'novels')
novels <- read.csv(file.path('data-raw', "novel_meta.csv"), encoding = 'UTF-8')

novels <- novels %>% mutate(
  response = as.factor(tolower(response)),
  date = gsub(".*(?:,\\s+((?:\\d{4,4}\\??)?-(?:\\d{4,4}\\??)?))?$", "\\1", author),
  author = str_squish(gsub(",\\s+(?:\\d{4,4}\\??)?-(?:\\d{4,4})?", "", author)),
  title = str_squish(title))

for(base.dir in names(genres)) {
  file.list <- list.files(file.path(root.dir, base.dir), pattern = "*.txt", recursive = TRUE)
  genre <- genres[[base.dir]]
  for(file.name in file.list) {
    base.name <- basename(file.name)
    relative.path.name <- file.path(root.dir, base.dir, file.name)
    text <- read_file(relative.path.name) %>% stri_enc_toutf8
    novels[which(novels$genre == genre & novels$file.name == base.name), "text"] <- text
  }
}

names(novels) <- names(novels) %>% stri_enc_toutf8

usethis::use_data(novels, overwrite = TRUE)
