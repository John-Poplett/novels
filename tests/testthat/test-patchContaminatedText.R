context('Librivox-contaminated text is actually replaced, not merely re-fetched')

# Background: five gutenberg_ids in the corpus point at LibriVox audio
# recordings, not text editions. patchContaminatedText() used to re-download
# each contaminated id, but the id itself is the audio edition, so Gutenberg
# returned the same audio page and the "patch" replaced contamination with
# identical contamination. The pipeline's baseline assertion ran *before*
# patching and never checked the outcome, so it passed silently.
#
# The gutenbergr download is mocked: tests must not need the network. The mock
# behaves like Gutenberg -- the audio id yields the audio page, any other id
# yields a real text -- and records which ids were requested.

audio_page <- "This audio reading of A Journey to the Center of the Earth is read by Annie Coleman. Librivox recording."

toy_corpus <- function() {
  tibble::tibble(
    gutenberg_id = c(19513L, 100L),
    text = c(audio_page, "the real text of an uncontaminated novel")
  )
}

mock_gutenberg <- function(requested) {
  function(gutenberg_id, ...) {
    requested$ids <- c(requested$ids, gutenberg_id)
    tibble::tibble(gutenberg_id = rep(gutenberg_id, each = 2L)) |>
      dplyr::mutate(text = ifelse(gutenberg_id == 19513L, audio_page, "Real Line One"))
  }
}

replacements <- c("19513" = 18857L)

test_that("the replacement is fetched under its own id, not the contaminated one", {
  requested <- new.env()
  testthat::local_mocked_bindings(gutenberg_download = mock_gutenberg(requested), .package = "gutenbergr")

  suppressMessages(novels::patchContaminatedText(toy_corpus(), replacements))

  expect_identical(requested$ids, 18857L)
})

test_that("after patching, no librivox text remains (regression: the patch was a no-op)", {
  requested <- new.env()
  testthat::local_mocked_bindings(gutenberg_download = mock_gutenberg(requested), .package = "gutenbergr")
  patched <- suppressMessages(novels::patchContaminatedText(toy_corpus(), replacements))

  expect_length(novels::getLibrivoxContaminatedIds(patched), 0)
})

test_that("the contaminated id stays the key and the replacement is lowercased", {
  requested <- new.env()
  testthat::local_mocked_bindings(gutenberg_download = mock_gutenberg(requested), .package = "gutenbergr")
  patched <- suppressMessages(novels::patchContaminatedText(toy_corpus(), replacements))

  expect_identical(patched$gutenberg_id, c(19513L, 100L))
  expect_identical(patched$text[1], "real line one\nreal line one")
})

test_that("patching leaves uncontaminated books untouched", {
  requested <- new.env()
  testthat::local_mocked_bindings(gutenberg_download = mock_gutenberg(requested), .package = "gutenbergr")
  df <- toy_corpus()
  patched <- suppressMessages(novels::patchContaminatedText(df, replacements))

  expect_identical(patched$text[2], df$text[2])
})

test_that("a replacement id that returns no text is an error, not an empty book", {
  testthat::local_mocked_bindings(
    gutenberg_download = function(gutenberg_id, ...) tibble::tibble(gutenberg_id = integer(), text = character()),
    .package = "gutenbergr"
  )

  expect_error(
    suppressMessages(novels::patchContaminatedText(toy_corpus(), replacements)),
    "no text for replacement id"
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
