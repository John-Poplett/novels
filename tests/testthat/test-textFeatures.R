context("Text features: vectorizer, DTM and TF-IDF")

# text2vocab() keeps terms that occur at least 10 times and in at most half the
# documents. "alpha" in 3 of 6 documents and "beta" in the other 3 satisfies
# both, with ngram = 1 and documentMinimum = 1.
tiny_corpus <- function() {
  tibble::tibble(
    title = paste0("t", 1:6),
    text = c(
      rep(paste(rep("alpha", 10), collapse = " "), 3),
      rep(paste(rep("beta", 10), collapse = " "), 3)
    )
  )
}

test_that("buildVectorizer and buildDtm share one vocabulary", {
  df <- tiny_corpus()
  vectorizer <- buildVectorizer(df, ngram = c(1L, 1L), documentMinimum = 1)

  dtm <- buildDtm(df, vectorizer)

  expect_s4_class(dtm, "dgCMatrix")
  expect_equal(nrow(dtm), 6L)
  expect_equal(sort(colnames(dtm)), c("alpha", "beta"))
})

test_that("fitTfidf returns both the fitted model and the weighted matrix", {
  df <- tiny_corpus()
  raw <- buildDtm(df, buildVectorizer(df, ngram = c(1L, 1L), documentMinimum = 1))

  fit <- fitTfidf(raw)

  expect_named(fit, c("model", "dtm"))
  expect_s3_class(fit$model, "TfIdf")
  expect_equal(dim(fit$dtm), dim(raw))
  expect_equal(colnames(fit$dtm), colnames(raw))
})

test_that("transformTfidf applies the fitted weights without refitting", {
  df <- tiny_corpus()
  raw <- buildDtm(df, buildVectorizer(df, ngram = c(1L, 1L), documentMinimum = 1))
  fit <- fitTfidf(raw)

  expect_equal(
    as.matrix(transformTfidf(fit, raw)),
    as.matrix(fit$dtm),
    tolerance = 1e-12
  )
})

test_that("transformTfidf aligns a new batch to the training vocabulary", {
  df <- tiny_corpus()
  vectorizer <- buildVectorizer(df, ngram = c(1L, 1L), documentMinimum = 1)
  fit <- fitTfidf(buildDtm(df, vectorizer))

  new_batch <- buildDtm(df[1:2, ], vectorizer)
  out <- transformTfidf(fit, new_batch)

  expect_equal(ncol(out), ncol(fit$dtm))
  expect_equal(colnames(out), colnames(fit$dtm))
})
