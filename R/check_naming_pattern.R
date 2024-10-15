#' Check Naming Pattern
#'
#' This function checks whether the file name exactly matches any of the specified valid patterns.
#' It is designed to ensure that filenames strictly follow the provided naming conventions.
#'
#' @param file A character string representing the file path to check.
#' @param valid_patterns A character vector of valid regular expression patterns that filenames should match.
#'
#' @return Logical. Returns TRUE if the file name exactly matches any of the valid patterns, and FALSE otherwise.
#'
#' @noRd
check_naming_pattern <- function(file, valid_patterns) {
 any(sapply(valid_patterns, function(pattern) grepl(pattern, basename(file))))
}
