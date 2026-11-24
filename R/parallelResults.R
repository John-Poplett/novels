# Checking what parallel::mclapply() returned, before anything is summarised
# from it (review/003 finding 8).

#' Stop unless every parallel job delivered a result
#'
#' `parallel::mclapply()` reports a job that failed in R as a `try-error`,
#' but a forked worker that died (killed for memory, say) leaves `NULL` in
#' its slot with only a warning, and `dplyr::bind_rows()` then drops it
#' silently. A summary built from what is left rests on fewer draws, and on
#' the ones whose worker happened to survive, not a random subset.
#'
#' @param results the list `parallel::mclapply()` returned, one element per
#'   job
#' @returns `results`, unchanged, when every job delivered; otherwise an
#'   error naming the jobs
#' @noRd
parallelResults <- function(results) {
  stopifnot("results must be a list" = is.list(results))
  lost <- which(vapply(results, is.null, logical(1)))
  if (length(lost)) {
    stop(length(lost), " of ", length(results), " parallel jobs returned nothing ",
         "(jobs ", paste(utils::head(lost, 10), collapse = ", "),
         if (length(lost) > 10) ", ...", "): a worker died, most likely killed ",
         "for memory. Nothing was summarised. Re-run with fewer cores.", call. = FALSE)
  }
  failed <- which(vapply(results, inherits, logical(1), "try-error"))
  if (length(failed)) {
    condition <- attr(results[[failed[1]]], "condition")
    stop(length(failed), " of ", length(results), " parallel jobs failed (jobs ",
         paste(utils::head(failed, 10), collapse = ", "),
         if (length(failed) > 10) ", ...", "); the first said: ",
         if (is.null(condition)) trimws(as.character(results[[failed[1]]]))
         else conditionMessage(condition), call. = FALSE)
  }
  results
}
