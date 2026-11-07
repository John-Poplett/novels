#' Find a file in the package's source tree
#'
#' `system.file()` returns "" when the package is not installed, which is the
#' case when `targets` runs `tar_source("R")` from the source tree. Walking up
#' from the working directory finds package data in that context without
#' requiring an installation, so pipeline runs and tests resolve the same
#' files.
#'
#' @param relative path relative to the package root
#' @returns the first existing path, or "" when none is found
#' @noRd
findInSourceTree <- function(relative) {
  dir <- normalizePath(getwd(), mustWork = FALSE)
  repeat {
    candidate <- file.path(dir, relative)
    if (file.exists(candidate)) {
      return(candidate)
    }
    parent <- dirname(dir)
    if (identical(parent, dir)) {
      return("")
    }
    dir <- parent
  }
}
