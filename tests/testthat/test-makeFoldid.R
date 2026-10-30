context('makeFoldid groups CV folds by author, not by row')
library(novels)

test_that("every row for the same author gets the same foldid", {
  author <- c("Verne, Jules", "Verne, Jules", "Twain, Mark", "Dumas, Alexandre", "Twain, Mark")
  foldid <- makeFoldid(author, nfolds = 3, seed = 1)
  expect_equal(foldid[author == "Verne, Jules"][1], foldid[author == "Verne, Jules"][2])
  expect_equal(foldid[author == "Twain, Mark"][1], foldid[author == "Twain, Mark"][2])
})

test_that("makeFoldid is reproducible given a fixed seed", {
  author <- c("A", "A", "B", "C", "D", "E")
  expect_identical(
    makeFoldid(author, nfolds = 2, seed = 42),
    makeFoldid(author, nfolds = 2, seed = 42)
  )
})

test_that("foldid values stay within 1:nfolds and match author vector length", {
  author <- c("A", "B", "C", "D", "E", "F")
  foldid <- makeFoldid(author, nfolds = 3, seed = 7)
  expect_equal(length(foldid), length(author))
  expect_true(all(foldid %in% 1:3))
})
