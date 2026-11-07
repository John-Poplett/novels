# Parsing the corpus's hierarchical metadata file.



parseMetaData <- function(dataset_path) {
  meta_path <- file.path(dataset_path, "novel_meta.txt")
  lines <- readr::read_lines(meta_path)          # will warn: incomplete final line

  is_record   <- str_detect(lines, "^(SUCCESS|FAILURE):")
  is_complete <- str_detect(lines, "DownloadCount:")
  is_trunc    <- is_record & !is_complete

  # Header census -- structure, not defect
  tar_assert_identical(sum(str_detect(lines, "^\\*+DATASET:")),           8L)
  tar_assert_identical(sum(str_detect(lines, "^\\*+\\s*FOLD\\d+")),       40L)

  lines_clean <- lines[!(str_detect(lines, "^(SUCCESS|FAILURE)") & !is_complete)]

  # SUCCESS: FileName: 103.txt ,Title: around the world in 80 days ,Author: "verne, jules, 1828-1905" ,Language: en ,DownloadCount: 3260
  novel_metadata <- tibble(raw = lines_clean) |>
    mutate(
      genre = if_else(
        str_detect(raw, "^\\*+DATASET:"),
        str_trim(str_extract(raw, "(?<=DATASET:)[^*]+")),
        NA_character_
      ),
      fold = if_else(
        str_detect(raw, "^\\*+\\s*FOLD\\d+"),
        str_extract(raw, "\\d+"),
        NA_character_
      ),
      is_record = str_detect(raw, "^(SUCCESS|FAILURE)")
    ) |>
    fill(genre, fold, .direction = "down") |>
    filter(is_record) |>
    select(-is_record) |>
    separate_wider_regex(
      raw,
      patterns = c(
        response = "^(?:SUCCESS|FAILURE)",
        ":\\s*FileName:\\s+",
        id = "[0-9]+",
        "\\.txt\\s+,Title:.*?,DownloadCount:\\s+",
        download_count = "[0-9]+",
        ".*$"
      )
    ) |>
    mutate(
      response = as.integer(str_equal(response, "SUCCESS")),
      gutenberg_id = as.integer(id),
      fold = as.integer(fold),
      download_count = as.integer(download_count),
    ) |>
    select(-id)

  # Independent witness: the directory tree
  fs <- list.files(dataset_path, pattern = "[0-9]+\\.txt$",
                   recursive = TRUE, full.names = TRUE)
  ids_fs <- as.integer(sub("\\.txt$", "", basename(fs)))
  absent <- setdiff(ids_fs, novel_metadata$gutenberg_id)

  tar_assert_true(
    nrow(novel_metadata) + sum(ids_fs %in% absent) == length(ids_fs),
    sprintf(
      "file conservation failed: %d parsed rows + %d absent-file rows = %d, but %d files on disk. absent = {%s}",
      nrow(novel_metadata), sum(ids_fs %in% absent),
      nrow(novel_metadata) + sum(ids_fs %in% absent),
      length(ids_fs), paste(absent, collapse = ", ")
    )
  )

  novel_metadata$genre <- novel_metadata$genre |>
    str_replace_all("Love Story", "Romance") |>
    str_replace_all("Sci-Fi", "Science Fiction")

  novel_metadata$genre <- as.factor(novel_metadata$genre)

  list(
    data   = novel_metadata,   # tibble, unchanged content
    census = list(
      n_blocks    = sum(is_record),
      n_parseable = nrow(novel_metadata),
      n_files     = length(ids_fs),
      truncated   = lines[is_trunc],
      absent      = absent,
      ts          = Sys.time()
    )
  )
}
