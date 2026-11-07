#' @importFrom dplyr .data across all_of arrange desc distinct filter first
#'   group_by if_else mutate select slice_head summarize
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr fill separate_wider_regex
#' @importFrom readr read_lines
#' @importFrom stringr str_detect str_equal str_extract str_replace_all str_trim
#' @importFrom stringi stri_enc_toutf8
#' @importFrom text2vec create_dtm create_vocabulary itoken prune_vocabulary
#'   vocab_vectorizer word_tokenizer
#' @importFrom tm stopwords
#' @importFrom targets tar_assert_identical tar_assert_true
#' @importFrom archive archive_extract
#' @importFrom utils download.file object.size
#' @importFrom stats coef predict
#' @keywords internal
"_PACKAGE"
