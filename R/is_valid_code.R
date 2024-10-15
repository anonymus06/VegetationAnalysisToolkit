#' Check if Code is Valid Based on Pre-defined Elements
#'
#' This function checks if a given code is valid by determining if it consists only of elements from
#' predefined patterns for position, land use, and optional inter-row conditions.
#'
#' @param code A character string representing the code to be validated.
#' @param positionPattern A character string containing the valid position elements, separated by a pipe ("|").
#' @param landusePattern A character string containing the valid land use elements, separated by a pipe ("|").
#' @param interRowConditions A list (optional) containing additional conditions. Each element of the list should
#'        have an `extract` field that contains valid elements. Defaults to `NULL` if not provided.
#'
#' @details The function constructs a regular expression based on the valid elements from the provided position
#'          pattern, land use pattern, and any additional conditions in `interRowConditions`. It then checks if
#'          the entire code matches the regular expression.
#'
#' @return A logical value (`TRUE` or `FALSE`) indicating whether the code is valid, based on the provided patterns.
#'
#' @importFrom stringr str_split
#' @noRd
is_valid_code <- function(code, positionPattern, landusePattern, interRowConditions) {
 position_elements <- str_split(positionPattern, "\\|", simplify = TRUE)
 landuse_elements <- str_split(landusePattern, "\\|", simplify = TRUE)
 extract_elements <- if (!is.null(interRowConditions)) {
  unique(unlist(lapply(interRowConditions, function(cond) cond$extract)))
 } else {
  character(0)
 }
 valid_elements <- c(position_elements, landuse_elements, extract_elements)
 combined_pattern <- paste0("^(", paste(valid_elements, collapse = "|"), ")+$")

 return(grepl(combined_pattern, code))
}
