#' Validate Data Types in a Data Frame
#'
#' This function checks whether the data types of the columns in the given data frame match the expected types.
#'
#' @param current_df The data frame whose column data types are to be validated.
#' @param name A character string representing the name of the data frame (used in reporting issues).
#' @param data_types A character vector specifying the expected data types for the main data columns.
#' @param index_types A character vector specifying the expected data types for the index columns.
#' @param file_name A character string representing the name of the source file (used in reporting issues).
#' @param local_issues A list of issues encountered during validation, to which new issues will be appended.
#'
#' @return A list containing any validation issues found, appended to `local_issues`.
#'         If no issues are found, `local_issues` is returned unchanged.
#'
#' @noRd
check_data_types <- function(current_df, name, data_types, index_types, file_name, local_issues) {
 actual_types <- sapply(current_df, class) %>% unname()

 if (all(actual_types == "character")) {
  local_issues <- add_issue(
   local_issues,
   name,
   paste0( # e.g. adding new columns to it can disrupt its original data types
    "All columns in '", name, "' are read as character type. ",
    "This may indicate an issue with the data file format. ",
    "If you modified the input file prior to processing, it is possible that this caused the issue. ",
    "Please use the original, unmodified file and try again. Source file: '", file_name, "'"
   )
  )

 } else if (all(actual_types == data_types)) {
  # No issue if data types match the expected types for the main data
 } else if (all(actual_types == index_types)) {
  # No issue if data types match the expected types for the index file

 } else {
  local_issues <- add_issue(
   local_issues,
   name,
   paste0(
    "Data types in '", name, "' data frame do not match! ",
    "Expected data types for data file: '", paste0(data_types, collapse = ", "), "'. ",
    "Expected data types for Index file: '", paste0(index_types, collapse = ", "), "'. ",
    "Found: '", paste0(actual_types, collapse = ", "), "'. Source file: '", file_name, "'"
   )
  )
 }

 return(local_issues)
}
