context('The source archive is verified before it is trusted')

test_that("verify_sha256 accepts a matching digest, case-insensitively", {
  path <- tempfile()
  writeLines("golden copy", path)
  digest <- digest::digest(path, algo = "sha256", file = TRUE)

  expect_silent(novels:::verify_sha256(path, digest))
  expect_silent(novels:::verify_sha256(path, toupper(digest)))
})

test_that("verify_sha256 rejects a mismatched digest and names both values", {
  path <- tempfile()
  writeLines("not the golden copy", path)
  expected <- strrep("0", 64)
  actual <- digest::digest(path, algo = "sha256", file = TRUE)

  message <- tryCatch(
    novels:::verify_sha256(path, expected),
    error = function(e) conditionMessage(e)
  )

  expect_match(message, "failed SHA-256 verification", fixed = TRUE)
  expect_match(message, expected, fixed = TRUE)
  expect_match(message, actual, fixed = TRUE)
})

test_that("fetchRawData refuses to download without a configured URL", {
  expect_error(
    fetchRawData(tempdir(), archive_url = "", archive_sha256 = "irrelevant"),
    "NOVELS_ARCHIVE_URL is not set"
  )
})

test_that("fetchRawData refuses to download without a pinned digest", {
  expect_error(
    fetchRawData(
      tempdir(),
      archive_url = "https://example.invalid/novels.7z",
      archive_sha256 = ""
    ),
    "must be pinned"
  )
})

test_that("verifyExtraction accepts a matching archive marker", {
  dir <- tempfile("corpus-")
  dir.create(dir)
  writeLines("metadata", file.path(dir, "novel_meta.txt"))
  writeLines("abc123", file.path(dir, ".archive_sha256"))
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  expect_true(novels:::verifyExtraction(dir, "ABC123"))
})

test_that("verifyExtraction rejects a marker from a different archive", {
  dir <- tempfile("corpus-")
  dir.create(dir)
  writeLines("metadata", file.path(dir, "novel_meta.txt"))
  writeLines("old-digest", file.path(dir, ".archive_sha256"))
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  expect_error(
    novels:::verifyExtraction(dir, "new-digest"),
    "different archive",
    fixed = TRUE
  )
})

test_that("verifyExtraction warns about an older extraction with no marker", {
  dir <- tempfile("corpus-")
  dir.create(dir)
  writeLines("metadata", file.path(dir, "novel_meta.txt"))
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  expect_warning(
    novels:::verifyExtraction(dir, "anything"),
    "cannot be verified",
    fixed = TRUE
  )
  expect_false(suppressWarnings(novels:::verifyExtraction(dir, "anything")))
})

test_that("verifyExtraction rejects a directory with no metadata", {
  dir <- tempfile("corpus-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  expect_error(
    novels:::verifyExtraction(dir, "anything"),
    "no novel_meta.txt",
    fixed = TRUE
  )
})

test_that("fetchRawData trusts a verified extraction without downloading", {
  data_path <- tempfile("raw-")
  dir.create(data_path)
  dir.create(file.path(data_path, "novels"))
  writeLines("metadata", file.path(data_path, "novels", "novel_meta.txt"))
  writeLines("abc123", file.path(data_path, "novels", ".archive_sha256"))
  on.exit(unlink(data_path, recursive = TRUE), add = TRUE)

  expect_equal(
    fetchRawData(data_path, "https://example.invalid/novels.7z", "abc123"),
    file.path(data_path, "novels")
  )
})
