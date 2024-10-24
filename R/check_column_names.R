#' Validate Column Names in a Data Frame
#'
#' This function verifies that the column names of the given data frame match the expected names.
#'
#' @param current_df The data frame whose column names are to be validated.
#' @param name A character string representing the name of the data frame (used in reporting issues).
#' @param device_type A character string specifying the type of data ("PPNP" for PlantPen or "MC100" for MC-100).
#' @param data_columns A character vector specifying the expected column names for the main data.
#' @param index_columns A character vector specifying the expected column names for the index data.
#' @param file_name A character string representing the name of the source file (used in reporting issues).
#' @param local_issues A list of issues encountered during validation, to which new issues will be appended.
#'
#' @return A list containing any validation issues found, appended to `local_issues`.
#'         If no issues are found, `local_issues` is returned unchanged.
#'
#' @noRd
check_column_names <- function(current_df, name, device_type, data_columns, index_columns, file_name, local_issues) {
  mapped_names <- clean_column_names(names(current_df))

  if (device_type == "PPNP") {
    expected_data_columns <- c("index", "time", "id", "760 [nm]", "635 [nm]", "NDVI")
    expected_index_columns <- c("index", "description")
  } else if (device_type == "MC100") {
    expected_data_columns <- c("Sample")
    expected_index_columns <- c("index", "description")
  }

  if (all(data_columns %in% mapped_names)) {
    # No issue if main data columns match
  } else if (all(index_columns %in% mapped_names)) {
    # No issue if index columns match
  } else {
    # Add issue if neither set of columns match
    local_issues <- add_issue(
      local_issues,
      name,
      paste0(
        "Your data frame '", name, "' does not match the expected column names! ",
        "Expected columns for data file: '", paste0(expected_data_columns, collapse = ", "), "'. ",
        "Expected columns for Index file: '", paste0(index_columns, collapse = ", "), "'. Source file: '", file_name, "'"
      )
    )
  }

  return(local_issues)
}
