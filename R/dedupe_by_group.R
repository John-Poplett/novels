#' Title dedupe_by_group
#'
#' @param df remove duplicates from this data frame based on columns_to_group
#' @param columns_to_group columns identifying a group; only the first row within each group is kept
#'
#' @returns dataset stripped of duplicate entries within the specified group
#' @export
#'
#' @examples deduped_novels <- dedupe_by_group(novels, c("gutenberg_id", "genre", "fold"))
#'
dedupe_by_group <- function(df, columns_to_group = c("gutenberg_id", "genre", "fold")) {
  df |> group_by(across(all_of(columns_to_group))) |>
    slice_head(n = 1) |> ungroup()
}
