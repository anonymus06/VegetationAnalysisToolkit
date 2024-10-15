#' Clean Column Names by Removing Junk Characters
#'
#' This function cleans up a vector of column names by removing non-alphanumeric characters
#' and specific patterns, such as "nm".
#'
#' @param col_names A character vector containing the column names to clean.
#'
#' @return A character vector of cleaned column names.
#'
#' @noRd
clean_column_names <- function(col_names) {
 sapply(col_names, function(name) {
  name <- gsub("[^[:alnum:]]", "", name)
  name <- gsub("nm", "", name)
  name
 })
}
