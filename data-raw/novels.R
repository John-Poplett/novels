# Use the exact same functions as is used by the Targets pipeline if building
# the dataset for an R package or without targets.

packages = c("dplyr", "readr", "usethis", "tibble", "stringi",
  "stringr", "purrr", "tibble", "tidyr", "assertthat", "gutenbergr", "archive",
  "text2vec", "glmnet", "pROC", "targets")

# Explicitly force the packages listed above to load
invisible(lapply(packages, library, character.only = TRUE))

# Source the whole R/ directory, not just targetFunctions.R -- consistent
# with _targets.R's tar_source("R"). Not currently load-bearing for this
# script's specific call sequence, but avoids the same silent-collision
# risk if this script ever calls anything from the other R/ files later.
invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))

rawdataset_path <- fetchRawData("data-raw")
novel_metadata <- parseMetaData(rawdataset_path)
novels <- tidyRawData(rawdataset_path, novel_metadata)
usethis::use_data(novels, overwrite = TRUE)
