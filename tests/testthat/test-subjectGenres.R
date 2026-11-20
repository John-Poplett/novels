context('Genre strata come from subject headings, multi-label by design')

test_that("a heading assigns its stratum, and every fiction stratum implies Fiction", {
  expect_identical(novels:::assignSubjectGenres("Love stories; England -- Fiction"),
                   c("Fiction", "Romance"))
  expect_identical(novels:::assignSubjectGenres("Science fiction, American"),
                   c("Fiction", "Science Fiction"))
  expect_identical(novels:::assignSubjectGenres("Short stories, French -- Translations into English"),
                   c("Fiction", "Short Stories"))
})

test_that("strata overlap freely", {
  expect_identical(
    novels:::assignSubjectGenres("Detective and mystery stories; Historical fiction; Love stories"),
    c("Detective/Mystery", "Fiction", "Historical Fiction", "Romance"))
})

test_that("Fiction covers fiction with no specific heading, and Poetry is not fiction", {
  expect_identical(novels:::assignSubjectGenres("Domestic fiction"), "Fiction")
  expect_identical(novels:::assignSubjectGenres("Orphans -- Juvenile fiction"), "Fiction")
  expect_identical(novels:::assignSubjectGenres("English poetry -- 19th century"), "Poetry")
  expect_identical(novels:::assignSubjectGenres("World War, 1914-1918 -- Poetry"), "Poetry")
})

test_that("a heading about a form is not an example of it", {
  expect_identical(novels:::assignSubjectGenres("Fantasy fiction -- History and criticism -- Periodicals"),
                   character())
  expect_identical(novels:::assignSubjectGenres("Fiction -- Technique"), character())
  expect_identical(novels:::assignSubjectGenres("Romances -- Stories, plots, etc."), character())
})

test_that("medieval Romances are not Romance, and non-fiction gets nothing", {
  expect_identical(novels:::assignSubjectGenres("Roland (Legendary character) -- Romances"),
                   character())
  expect_identical(novels:::assignSubjectGenres("United States -- History -- Revolution, 1775-1783"),
                   character())
  expect_identical(novels:::assignSubjectGenres(""), character())
})

test_that("textForm applies the section 5.5 boundaries", {
  expect_identical(novels:::textForm(c(0, 1500, 1501, 15000, 15001, 50000, 50001)),
                   c("flash", "flash", "short story", "short story", "novella", "novella", "novel"))
  expect_identical(novels:::textForm(NA_real_), NA_character_)
  expect_error(novels:::textForm(-1), "non-negative")
})

test_that("Novels is fiction over 50,000 words, and only fiction", {
  expect_identical(novels:::novelStratum(c("Fiction", "Romance"), 50001),
                   c("Fiction", "Novels", "Romance"))
  expect_identical(novels:::novelStratum(c("Fiction", "Romance"), 50000), c("Fiction", "Romance"))
  expect_identical(novels:::novelStratum("Poetry", 90000), "Poetry")
  expect_identical(novels:::novelStratum("Fiction", NA_real_), "Fiction")
})

test_that("the shipped map has one row per stratum, with valid patterns", {
  map <- novels:::subjectGenreMap()
  expect_identical(anyDuplicated(map$stratum), 0L)
  expect_setequal(map$stratum, c(novels:::FICTION_STRATA, "Poetry", "Fiction"))
  for (p in map$pattern) expect_silent(grepl(p, "x"))
})
