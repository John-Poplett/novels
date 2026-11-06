# Retrieving the pinned source archive and verifying an extraction.

#' Title fetchDataSet
#'
#' Retrieves and unpacks the "golden" source corpus archive, refusing to use
#' it unless its SHA-256 matches the pinned digest. The URL is configuration
#' (the `NOVELS_ARCHIVE_URL` environment variable, see `.env.example`) so the
#' archive may move -- from Backblaze to HuggingFace, say -- without touching
#' package code. The digest is deliberately a literal in `_targets.R`, not an
#' environment variable: `targets` does not hash environment variables, so a
#' changed URL would be invisible to the dependency graph. The digest is what
#' makes switched bytes an error instead of a silent new result.
#'
#' @param data_path directory into which the source corpus is downloaded and
#'   unpacked (default "data-raw")
#' @param archive_url location of the 7z archive, defaulting to the
#'   `NOVELS_ARCHIVE_URL` environment variable
#' @param archive_sha256 expected SHA-256 of the archive, pinned in `_targets.R`
#' @returns relative path to the dataset
#' @seealso verify_sha256
#' @export
fetchRawData <- function(data_path = 'data-raw',
                         archive_url = Sys.getenv("NOVELS_ARCHIVE_URL"),
                         archive_sha256) {
  tar_assert_true(dir.exists(data_path))
  tar_assert_true(
    nzchar(archive_url),
    "NOVELS_ARCHIVE_URL is not set -- copy .env.example to .env and fill it in"
  )
  tar_assert_true(
    nzchar(archive_sha256),
    "archive_sha256 must be pinned in _targets.R, never read from the environment"
  )

  dataset_path <- file.path(data_path, 'novels')
  if (!file.exists(dataset_path)) {
    tmp <- tempfile(fileext = ".7z")
    on.exit(unlink(tmp), add = TRUE)
    download.file(archive_url, tmp, quiet = TRUE, mode = "wb")
    verify_sha256(tmp, archive_sha256, what = "source archive")
    archive_extract(tmp, data_path)
    # Record which archive produced this extraction, so a later run can tell a
    # verified corpus from one that merely happens to exist.
    writeLines(archive_sha256, file.path(dataset_path, ".archive_sha256"))
  } else {
    verifyExtraction(dataset_path, archive_sha256)
  }
  tar_assert_true(file.exists(dataset_path), "raw dataset retrieval failed.")
  dataset_path
}

#' Verify an already-extracted corpus
#'
#' A directory that exists is not evidence that it is complete or that it came
#' from the pinned archive. Newer extractions carry a `.archive_sha256` marker;
#' older ones are accepted with a warning rather than forcing a re-download,
#' but a marker that disagrees with the pin is an error.
#'
#' @param dataset_path extracted corpus directory
#' @param archive_sha256 digest pinned in `_targets.R`
#' @returns `TRUE` invisibly when the marker matches, `FALSE` when absent
#' @noRd
verifyExtraction <- function(dataset_path, archive_sha256) {
  meta <- file.path(dataset_path, "novel_meta.txt")
  if (!file.exists(meta)) {
    stop(
      "Extracted corpus at ", dataset_path, " has no novel_meta.txt. ",
      "Delete the directory and re-run to extract and verify the pinned archive.",
      call. = FALSE
    )
  }

  marker <- file.path(dataset_path, ".archive_sha256")
  if (!file.exists(marker)) {
    warning(
      "Corpus at ", dataset_path, " has no .archive_sha256 marker, so its ",
      "provenance cannot be verified. Delete the directory to re-extract from ",
      "the pinned archive.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }

  recorded <- readLines(marker, warn = FALSE)
  recorded <- if (length(recorded) > 0) recorded[[1]] else ""

  tar_assert_true(
    tolower(recorded) == tolower(archive_sha256),
    sprintf(
      "Extracted corpus was built from a different archive: marker %s, pinned %s. Delete %s to re-extract.",
      recorded, archive_sha256, dataset_path
    )
  )

  invisible(TRUE)
}

#' Verify a file's SHA-256 against an expected value
#'
#' Used by fetchRawData() to pin the source archive. A mismatch aborts, and
#' the error names both digests so a legitimately moved or re-cut archive can
#' be told apart from a corrupted download.
#'
#' @param path path to the file to check
#' @param expected expected SHA-256, hex, case-insensitive
#' @param what label for the file in the error message
#' @returns the actual digest, invisibly
#' @keywords internal
verify_sha256 <- function(path, expected, what = "file") {
  actual <- digest::digest(path, algo = "sha256", file = TRUE)
  tar_assert_true(
    tolower(actual) == tolower(expected),
    sprintf(
      "%s failed SHA-256 verification\n  expected: %s\n  actual:   %s\n  file:     %s",
      what, expected, actual, path
    )
  )
  invisible(actual)
}
