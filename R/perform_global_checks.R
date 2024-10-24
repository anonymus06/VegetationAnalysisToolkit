#' Perform Global Checks
#'
#' This function runs a series of checks to validate a given data frame.
#'
#' @param df A list of data frames where each data frame is checked for issues.
#' @param name The name of the data frame (used as an identifier).
#' @param data_columns A character vector specifying the expected column names for the main data.
#' @param index_columns A character vector specifying the expected column names for the index data.
#' @param device_type A character string specifying the type of data (e.g., "PPNP", "MC100").
#' @param data_types A character vector specifying the expected data types for the main data columns.
#' @param index_types A character vector specifying the expected data types for the index columns.
#' @param data_frame_files A list of file names corresponding to the data frames in `df`, used for validation purposes.
#' @param local_issues A list of issues encountered during previous checks, to which new issues will be appended.
#'
#' @details
#' This function performs several checks:
#' 1. Ensures that the data is not NULL, it is not empty and it has a good structure.
#' 2. Verifies that the required column names are present.
#' 3. Checks that the data types of the columns are as expected.
#' 4. Checks for missing values in the data.
#' 5. Checks for duplicate rows in the data.
#'
#' Each issue found is appended to the `local_issues` list, which is returned at the end of the function.
#'
#' @return A list of issues encountered during the validation of the data frame.
#'
#' @noRd
perform_global_checks <- function(df, name, data_columns, index_columns, device_type, data_types,
                                  index_types, data_frame_files, local_issues) {
  current_df <- df[[name]]
  file_name <- data_frame_files[[name]]

  local_issues <- check_null_empty_cols(current_df, name, data_columns, file_name, local_issues)
  local_issues <- check_column_names(current_df, name, device_type, data_columns, index_columns, file_name, local_issues)
  local_issues <- check_data_types(current_df, name, data_types, index_types, file_name, local_issues)
  local_issues <- check_missing_values(current_df, name, file_name, local_issues)
  local_issues <- check_duplicate_rows(current_df, name, file_name, local_issues)

  return(local_issues)
}
