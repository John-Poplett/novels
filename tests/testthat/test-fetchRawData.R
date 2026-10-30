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
