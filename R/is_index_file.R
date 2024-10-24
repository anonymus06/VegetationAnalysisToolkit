#' Function to determine if a data frame is from an index file
#'
#' @param file_path The file path to check.
#'
#' @return Logical, TRUE if the data frame is from an index file, FALSE otherwise.
#' @noRd
is_index_file <- function(file_path) {
  grepl("index.xlsx", file_path, ignore.case = TRUE)
}
