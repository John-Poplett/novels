context('Librivox-contaminated text is replaced from pinned snapshots')

# Background: five gutenberg_ids in the corpus point at LibriVox audio
# recordings, not text editions. patchContaminatedText() used to re-download
# each contaminated id, but the id itself is the audio edition, so Gutenberg
# returned the same audio page and the "patch" replaced contamination with
# identical contamination. The pipeline's baseline assertion ran *before*
# patching and never checked the outcome, so it passed silently.
#
# The replacement texts are now shipped compressed and pinned by SHA-256, so
# these tests never touch the network.

audio_page <- "This audio reading of A Journey to the Center of the Earth is read by Annie Coleman. Librivox recording."

toy_corpus <- function() {
  tibble::tibble(
    gutenberg_id = c(19513L, 100L),
    text = c(audio_page, "the real text of an uncontaminated novel")
  )
}

replacements <- c("19513" = 18857L)

read_stored <- function(id) {
  con <- xzfile(novels:::replacementTextPath(id), "rb")
  on.exit(close(con), add = TRUE)
  paste(readLines(con, warn = FALSE), collapse = "\n")
}

test_that("the shipped snapshots match their pinned digests", {
  pinned <- novels:::librivox_replacement_sha256

  for (id in names(pinned)) {
    path <- novels:::replacementTextPath(as.integer(id))
    actual <- digest::digest(path, algo = "sha256", file = TRUE)
    expect_identical(tolower(actual), tolower(pinned[[id]]))
  }
})

test_that("patching uses the stored replacement text, lowercased", {
  patched <- suppressMessages(novels::patchContaminatedText(toy_corpus(), replacements))

  expect_identical(patched$gutenberg_id, c(19513L, 100L))
  expect_identical(patched$text[1], read_stored(18857L))
  expect_false(grepl("librivox", patched$text[1], ignore.case = TRUE))
})

test_that("patching leaves uncontaminated books untouched", {
  df <- toy_corpus()
  patched <- suppressMessages(novels::patchContaminatedText(df, replacements))

  expect_identical(patched$text[2], df$text[2])
})

test_that("a missing replacement file is an error, not an empty book", {
  empty <- tempfile("replacements-")
  dir.create(empty)
  old <- options(novels.replacements_path = empty)
  on.exit(options(old), add = TRUE)

  expect_error(
    suppressMessages(novels::patchContaminatedText(toy_corpus(), replacements)),
    "not found",
    fixed = TRUE
  )
})

test_that("a changed replacement file fails its digest check", {
  dir <- tempfile("replacements-")
  dir.create(dir)
  con <- xzfile(file.path(dir, "18857.txt.xz"), "wb")
  writeLines("tampered text", con)
  close(con)
  old <- options(novels.replacements_path = dir)
  on.exit(options(old), add = TRUE)

  expect_error(
    suppressMessages(novels::patchContaminatedText(toy_corpus(), replacements)),
    "SHA-256 mismatch",
    fixed = TRUE
  )
})

test_that("the replacement table never maps an id to a contaminated id", {
  table <- novels:::librivox_replacements
  contaminated <- c(as.integer(names(table)), novels:::librivox_drops)

  expect_length(intersect(contaminated, unname(table)), 0)
  expect_false(anyDuplicated(unname(table)) > 0)
})

test_that("patched and dropped ids together are exactly the five known contaminated ids", {
  patched <- as.integer(names(novels:::librivox_replacements))
  dropped <- novels:::librivox_drops

  expect_length(intersect(patched, dropped), 0)
  expect_setequal(c(patched, dropped), c(19513L, 19640L, 19678L, 19782L, 19836L))
})

test_that("every replacement source has a pinned digest", {
  expect_setequal(
    unname(novels:::librivox_replacements),
    as.integer(names(novels:::librivox_replacement_sha256))
  )
})

test_that("dropByGutenbergId removes only the listed ids and keeps the rest intact", {
  df <- tibble::tibble(gutenberg_id = c(1L, 2L, 3L), text = c("a", "b", "c"))
  out <- novels:::dropByGutenbergId(df, c(2L, 99L))

  expect_identical(out$gutenberg_id, c(1L, 3L))
  expect_identical(out$text, c("a", "c"))
  expect_identical(novels:::dropByGutenbergId(df, integer()), df)
})

context('Titles that no longer describe the replaced text are corrected')

test_that("a corrected title is applied to every row of that book, and only those", {
  df <- tibble::tibble(
    gutenberg_id = c(19836L, 19836L, 100L),
    genre = c("Poetry", "Fiction", "Fiction"),
    title = c("Early Spring", "Early Spring", "Another Book")
  )
  out <- novels:::applyTitleCorrections(df, c("19836" = "The Verse-Book of a Homely Woman"))

  expect_identical(out$title, c(rep("The Verse-Book of a Homely Woman", 2), "Another Book"))
  expect_identical(out[c("gutenberg_id", "genre")], df[c("gutenberg_id", "genre")])
})

test_that("with no matching id the data is returned unchanged", {
  df <- tibble::tibble(gutenberg_id = 100L, title = "Another Book")

  expect_identical(novels:::applyTitleCorrections(df, c("19836" = "x")), df)
})

test_that("every corrected title belongs to an id in the replacement table", {
  expect_true(all(names(novels:::librivox_title_corrections) %in% names(novels:::librivox_replacements)))
})
