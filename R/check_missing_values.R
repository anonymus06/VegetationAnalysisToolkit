#' Validate Missing Values in a Data Frame
#'
#' This function checks whether any missing values (NA) are present in the given data frame.
#'
#' @param current_df The data frame to be checked for missing values.
#' @param name A character string representing the name of the data frame (used in reporting issues).
#' @param file_name A character string representing the name of the source file (used in reporting issues).
#' @param local_issues A list of issues encountered during validation, to which new issues will be appended.
#'
#' @return A list containing any validation issues found, appended to `local_issues`.
#'         If no issues are found, `local_issues` is returned unchanged.
#'
#' @noRd
check_missing_values <- function(current_df, name, file_name, local_issues) {
 current_df[current_df == ""] <- NA

 if (any(colSums(is.na(current_df)) > 0)) {
  local_issues <-
   add_issue(local_issues,
             name,
             paste0("Missing values found in one or more columns. Source file: '", file_name, "'"))
 }

 return(local_issues)
}
