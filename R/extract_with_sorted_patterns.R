#' Extract the Longest Match from a Sorted Pattern List
#'
#' This function takes a text string and a pattern, splits the pattern into
#' individual elements, sorts the elements by their length in descending order,
#' and then extracts the longest matching pattern from the text.
#'
#' @param text A character string from which the pattern will be extracted.
#' @param pattern A character string containing multiple patterns separated by
#'                the pipe (`|`) character.
#'
#' @return A character string representing the longest matching pattern found
#'         in the input text, or `NA` if no match is found.
#'
#' @examples
#' \dontrun{
#' text <- "This is a sample text"
#' pattern <- "sample|text|is|a"
#' result <- extract_with_sorted_patterns(text, pattern)
#' print(result)  # Output: "sample"
#' }
#'
#' @importFrom stringr str_extract
#' @noRd
extract_with_sorted_patterns <- function(text, pattern) {
 pattern_list <- unlist(strsplit(pattern, "\\|"))
 sorted_patterns <- pattern_list[order(nchar(pattern_list), decreasing = TRUE)]
 sorted_pattern <- paste(sorted_patterns, collapse = "|")

 for (p in sorted_patterns) {
  if (str_detect(text, p)) {
   return(p)
  }
 }

 return(NA)
}
