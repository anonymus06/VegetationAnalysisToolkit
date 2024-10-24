#' Perform All Validation Checks on Data
#'
#' This function performs both structural and content validation on the given data frames,
#' ensuring that data is set up in a usable structure and make sense in terms like sequence, format, and consistency.
#'
#' @param df A list of data frames containing the entire dataset, including both index and non-index data.
#' @param non_index_dfs A list of data frames excluding index files for more specific validation.
#' @param data_columns A character vector specifying the expected column names for the main data.
#' @param index_columns A character vector specifying the expected column names for the index data.
#' @param data_types A character vector specifying the expected data types for the main data columns.
#' @param device_type A character string specifying the type of data (e.g., "PPNP", "MC100").
#' @param index_types A character vector specifying the expected data types for the index columns.
#' @param variable The name of the variable being checked (e.g., "NDVI", "CCI").
#' @param data_frame_files A list of file names corresponding to the data frames in `df`, used for validation purposes.
#'
#' @details
#' The function operates in two phases, performing both structural and content validation on global and core data:
#'
#' 1. **Global Checks**: Applied to all data frames, including both index (user-created index files)
#' and non-index (measurement data) files.
#'
#' 2. **Core Data Checks**: Applied only to the non-index data frames (core dataset), which contain
#' the actual measurement data.
#'
#' Both global and core data undergo:
#' 1. **Structural Validation**:
#'      - `check_null_empty_cols()`: Ensures the data exists and matches the expected dimensions.
#'      - `check_column_names()`  : Verifies that column names are as expected.
#'      - `check_data_types()`    : Checks that the data types match the expected types for each column.
#'      - `check_missing_values()`: Identifies missing values.
#'      - `check_duplicate_rows()`: Detects duplicate rows.
#'
#' 2. **Content Validation**:
#'      - `check_index_sequence()`     : Verifies that index values are in the correct order.
#'      - `check_datetime_format()`    : Ensures date and time values are formatted properly.
#'      - `check_id_unit_consistency()`: Ensures consistency between variable names and their corresponding data.
#'
#' All identified issues from both global and core checks are aggregated and returned as a list.
#'
#' @return A list containing all issues found during validation. If no issues are found, an empty list is returned.
#' @noRd
perform_all_checks <- function(df, non_index_dfs, data_columns, index_columns, data_types, device_type, index_types, variable, data_frame_files) {
  local_issues <- list()

  global_issues <- lapply(names(df), function(name) {
    perform_global_checks(df, name, data_columns, index_columns, device_type, data_types, index_types, data_frame_files, list())
  })
  global_issues <- do.call(c, global_issues)

  core_issues <- lapply(names(non_index_dfs), function(name) {
    perform_core_checks(non_index_dfs, name, variable, device_type, data_frame_files, local_issues)
  })
  core_issues <- do.call(c, core_issues)

  if (length(global_issues) > 0) {
    local_issues <- c(local_issues, global_issues)
  }
  if (length(core_issues) > 0) {
    local_issues <- c(local_issues, core_issues)
  }

  return(local_issues)
}
