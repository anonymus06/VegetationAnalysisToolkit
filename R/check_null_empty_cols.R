#' Validate Data Existence and Structure
#'
#' This function checks if a data frame is not NULL, it is not empty and it has a good structure.
#'
#' @param current_df The data frame to be validated.
#' @param name A character string representing the name of the data frame (used in reporting issues).
#' @param data_columns A character vector specifying the expected column names for the main data.
#' @param file_name A character string representing the name of the source file (used in reporting issues).
#' @param local_issues A list of issues encountered during validation, to which new issues will be appended.
#'
#' @return A list containing any validation issues found, appended to `local_issues`.
#'         If no issues are found, `local_issues` is returned unchanged.
#'
#' @noRd
check_null_empty_cols <- function(current_df, name, data_columns, file_name, local_issues) {
  if (is.null(current_df)) {
    local_issues <-
      add_issue(
        local_issues,
        name,
        paste0("The data frame '", name, "' does not exist or is NULL. Source file: '", file_name, "'")
      )
    return(local_issues)
  }

  if (nrow(current_df) == 0) {
    local_issues <-
      add_issue(
        local_issues,
        name,
        paste0("The data frame '", name, "' is empty. Source file: '", file_name, "'")
      )
    return(local_issues)
  }

  if (ncol(current_df) == length(data_columns)) {
    # Handle data
  } else if (ncol(current_df) == 2) {
    # Handle Index file
  } else {
    local_issues <- add_issue(
      local_issues,
      name,
      paste0(
        "The data frame '",
        name,
        "' does not match any expected number of columns. ",
        "Detected dimensions: ",
        ncol(current_df),
        " columns. ",
        "Expected dimensions for data file: ",
        length(data_columns), ". ",
        "Expected dimensions for Index file: ",
        2, ". Source file: '", file_name, "'"
      )
    )
    return(local_issues)
  }

  return(local_issues)
}
