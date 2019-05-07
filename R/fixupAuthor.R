fixupAuthor <- function(db) {
  db$author <- gsub(",\\s+(?:\\d{4,4}\\??)?-(?:\\d{4,4})?", "", db$author)
  db
}

