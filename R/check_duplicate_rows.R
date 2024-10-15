#' Check for Duplicate Rows in a Data Frame
#'
#' This function checks whether any duplicate rows are present in the given data frame.
#'
#' @param current_df The data frame to be checked for duplicate rows.
#' @param name A character string representing the name of the data frame (used in reporting issues).
#' @param file_name A character string representing the name of the source file (used in reporting issues).
#' @param local_issues A list of issues encountered during validation, to which new issues will be appended.
#'
#' @return A list containing any validation issues found, appended to `local_issues`. If no duplicate rows are found, `local_issues` is returned unchanged.
#'
#' @noRd
check_duplicate_rows <- function(current_df, name, file_name, local_issues) {

 if (any(duplicated(current_df))) {
  local_issues <-
   add_issue(local_issues,
             name,
             paste0("Duplicate rows found in the data. Source file: '", file_name, "'"))
 }

 return(local_issues)
}
