test_that("waybackIndexUrl asks for status-200 captures of the book page", {
  expect_identical(
    waybackIndexUrl(215),
    paste0("https://web.archive.org/cdx/search/cdx?url=gutenberg.org/ebooks/215",
           "&from=2011&to=2013&fl=timestamp,digest&filter=statuscode:200"))
  expect_match(waybackIndexUrl(18857, "201201", "201212"), "from=201201&to=201212")
  expect_error(waybackIndexUrl(0), "positive whole number")
})

test_that("waybackCaptures reads the plain-text index", {
  cap <- waybackCaptures(c("20120502033536 ABCDEF123", "", "20121018051040 Z9Z9 "))
  expect_identical(cap$timestamp, c("20120502033536", "20121018051040"))
  expect_identical(cap$time[1], as.POSIXct("2012-05-02 03:35:36", tz = "UTC"))
  expect_identical(cap$digest, c("ABCDEF123", "Z9Z9"))
  expect_identical(nrow(waybackCaptures(character())), 0L)
  expect_error(waybackCaptures('[["timestamp","digest"]]'), "Not a capture-index line")
})

test_that("waybackBracket takes the nearest capture on each side", {
  cap <- waybackCaptures(c("20110716175114 A", "20120502033536 B",
                           "20121018051040 C", "20130117085548 D"))
  b <- waybackBracket(cap, "2012-05-15")
  expect_identical(b$side, c("before", "after"))
  expect_identical(b$digest, c("B", "C"))
  expect_true(b$days[1] < 0 && b$days[2] > 0)
  # nothing after: one row
  expect_identical(waybackBracket(cap, "2014-01-01")$digest, "D")
  # a capture on the day counts as before
  expect_identical(waybackBracket(cap, "2012-05-02")$side[1], "before")
  expect_identical(nrow(waybackBracket(cap[0, ], "2012-05-15")), 0L)
})

test_that("waybackPageUrl asks for the stored page, not the archive's rewrite", {
  expect_identical(waybackPageUrl(76, "20120502033540"),
                   "https://web.archive.org/web/20120502033540id_/http://www.gutenberg.org/ebooks/76")
  expect_error(waybackPageUrl(76, "2012"), "14 digits")
})

test_that("waybackDownloads reads every known form of the Downloads row", {
  # the three forms, as captured for book 215 (2012-05-02, 2013-01-17, 2013-04-30)
  bare <- c("<tr>", "<th>Downloads</th>", "<td>3434</td>", "</tr>")
  item <- '<tr><th>Downloads</th> <td itemprop="interactionCount">3802</td></tr>'
  window <- paste0('<tr><th>Downloads</th>\n<td itemprop="interactionCount">',
                   '6221 downloads in the last 30 days.</td></tr>')
  expect_identical(waybackDownloads(bare), data.frame(downloads = 3434L, window = "unstated"))
  expect_identical(waybackDownloads(item), data.frame(downloads = 3802L, window = "unstated"))
  expect_identical(waybackDownloads(window), data.frame(downloads = 6221L, window = "30 days"))
  expect_identical(waybackDownloads("<th>Downloads</th><td>17,538</td>")$downloads, 17538L)
})

test_that("a page without a Downloads row gives NA, not a number from elsewhere", {
  expect_identical(waybackDownloads(c("<html>", "<p>Downloads: 12</p>", "</html>")),
                   data.frame(downloads = NA_integer_, window = NA_character_))
  expect_identical(waybackDownloads(character())$downloads, NA_integer_)
})
