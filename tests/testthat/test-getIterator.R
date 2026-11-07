context('getIterator correctly threads ids through to the resulting iterator/DTM')

test_that("getIterator's ids become the DTM's rownames", {
  text <- c("the quick brown fox", "a lazy sleepy dog")
  titles <- c("Book One", "Book Two")

  it <- getIterator(text, titles)
  vocab <- text2vec::create_vocabulary(it)
  vectorizer <- text2vec::vocab_vectorizer(vocab)

  # Re-create the iterator (text2vec iterators are single-use) to build
  # the actual DTM, matching how buildDtm()/createPredictor() use it.
  it2 <- getIterator(text, titles)
  dtm <- text2vec::create_dtm(it2, vectorizer)

  expect_identical(rownames(dtm), titles)
})

test_that("getIterator defaults to auto-generated ids, not the old identity-function bug", {
  text <- c("the quick brown fox", "a lazy sleepy dog")
  it <- getIterator(text)  # no ids argument
  vocab <- text2vec::create_vocabulary(it)
  vectorizer <- text2vec::vocab_vectorizer(vocab)
  it2 <- getIterator(text)
  dtm <- text2vec::create_dtm(it2, vectorizer)

  # Shouldn't error, and shouldn't produce ids literally named "function"
  # or similar -- this is the direct regression test for the old
  # `ids = identity` default bug.
  expect_false(is.null(rownames(dtm)))
  expect_false(any(grepl("function", rownames(dtm), fixed = TRUE)))
})
