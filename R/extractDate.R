extractDate <- function(db) {
  db$date <- gsub(".*(?:,\\s+((?:\\d{4,4}\\??)?-(?:\\d{4,4}\\??)?))?$", "\\1", db$author)
  db
}

