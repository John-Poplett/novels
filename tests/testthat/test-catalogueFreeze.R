context('The catalogue is frozen only under the registered rules')

utc <- function(x) as.POSIXct(x, tz = "UTC")
day <- function(x) as.Date(x)

test_that("a catalogue regenerated on or after 2026-12-31 and retrieved by 2027-01-31 is accepted", {
  expect_true(novels:::checkCatalogueTiming(utc("2026-12-31 00:00:00"), day("2026-12-31")))
  expect_true(novels:::checkCatalogueTiming(utc("2027-01-30 23:59:59"), day("2027-01-31")))
})

test_that("a run before 2026-12-31 is refused, however fresh the catalogue", {
  expect_error(novels:::checkCatalogueTiming(utc("2026-12-30 12:00:00"), day("2026-12-30")),
               "frozen on or after 2026-12-31")
})

test_that("a run after 2027-01-31 is refused: the amendment says revisit the wave", {
  expect_error(novels:::checkCatalogueTiming(utc("2027-02-01 00:00:00"), day("2027-02-01")),
               "past the 2027-01-31 deadline")
})

test_that("a catalogue regenerated before 2026-12-31 is refused, such as a frozen mirror's", {
  expect_error(novels:::checkCatalogueTiming(utc("2026-12-30 23:59:59"), day("2027-01-05")),
               "before 2026-12-31")
  expect_error(novels:::checkCatalogueTiming(utc("2026-11-08 22:43:51"), day("2026-12-31")),
               "do not use a mirror")
})

test_that("a catalogue without a usable Last-Modified is refused", {
  expect_error(novels:::checkCatalogueTiming(utc(NA), day("2027-01-02")),
               "no usable Last-Modified")
  expect_error(novels:::checkCatalogueTiming("2027-01-01", day("2027-01-02")), "POSIXct")
  expect_error(novels:::checkCatalogueTiming(utc("2027-01-01"), "2027-01-02"), "Date")
})

write_catalogue <- function(rows, header = novels:::CATALOGUE_COLUMNS) {
  path <- tempfile(fileext = ".csv.gz")
  con <- gzfile(path, "w")
  writeLines(paste(header, collapse = ","), con)
  writeLines(rows, con)
  close(con)
  path
}

catalogue_rows <- c(
  '1,Text,1971-12-01,"Declaration, The",en,"Jefferson, Thomas",History,E201,Politics',
  '2,Sound,2004-01-01,A Recording,en,Someone,Music,M,Audio',
  '3,Text,2026-12-31,Last Day,en,Author,Fiction,PS,Adventure',
  '4,Text,2027-01-02,Too Late,en,Author,Fiction,PS,Adventure',
  '5,Text,2010-05-05,Un livre,fr,Auteur,Fiction,PQ,Adventure',
  '6,Text,,No Date,en,Author,Fiction,PS,Adventure'
)

test_that("readCatalogue reads every column as character, quoted commas intact", {
  cat <- novels:::readCatalogue(write_catalogue(catalogue_rows), min_rows = 6)
  expect_identical(names(cat), novels:::CATALOGUE_COLUMNS)
  expect_true(all(vapply(cat, is.character, logical(1))))
  expect_identical(cat$Title[1], "Declaration, The")
  expect_identical(cat$`Text#`, as.character(1:6))
})

test_that("readCatalogue keeps empty fields and the text 'NA' as written", {
  cat <- novels:::readCatalogue(write_catalogue(c(
    '7,Text,2001-01-01,NA,en,NA,,PS,'
  )), min_rows = 1)
  expect_identical(cat$Title, "NA")
  expect_identical(cat$Authors, "NA")
  expect_identical(cat$Subjects, "")
  expect_false(anyNA(unlist(cat)))
})

test_that("readCatalogue refuses a page served in place of the gzip file", {
  path <- tempfile()
  writeLines("<!DOCTYPE html><title>Temporarily Down for Maintenance</title>", path)
  expect_error(novels:::readCatalogue(path, min_rows = 1), "not a gzip file")
})

test_that("readCatalogue refuses a changed header", {
  header <- replace(novels:::CATALOGUE_COLUMNS, 9, "Shelves")
  expect_error(novels:::readCatalogue(write_catalogue(catalogue_rows, header), min_rows = 1),
               "format has changed")
})

test_that("readCatalogue refuses a truncated catalogue", {
  expect_error(novels:::readCatalogue(write_catalogue(catalogue_rows), min_rows = 7),
               "probably truncated")
  expect_error(novels:::readCatalogue(write_catalogue(catalogue_rows)), "probably truncated")
})

test_that("catalogueCounts applies the two catalogue rules of section 3", {
  cat <- novels:::readCatalogue(write_catalogue(catalogue_rows), min_rows = 6)
  expect_identical(novels:::catalogueCounts(cat), c(
    rows = 6L, text_en = 4L, text_en_issued_by_freeze = 2L, unparsed_issued = 1L
  ))
})
