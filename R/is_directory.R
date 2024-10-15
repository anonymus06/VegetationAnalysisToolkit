#' Check if a File Path is a Directory
#'
#' This function checks if the provided file path corresponds to a directory.
#'
#' @param file Character String, the file path to check.
#'
#' @return Logical, TRUE if the file is a directory, FALSE otherwise.
#'
#' @noRd
is_directory <- function(file) {

 # Check if the file path is a directory
 dir_check <- file.info(file)$isdir

 # Handle NA values (in case the file path does not exist)
 if (is.na(dir_check)) {
  return(FALSE)
 }

 return(dir_check)
}
