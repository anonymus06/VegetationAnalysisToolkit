#' Perform Content-Specific Checks
#'
#' This function runs checks to validate a given data frame.
#'
#' @param df A list of data frames where each data frame is checked for content consistency.
#' @param name The name of the data frame (used as an identifier).
#' @param variable The name of the variable being checked (e.g., NDVI, Chlorophyll), which is used to tailor the checks.
#' @param device_type A character string specifying the type of data (e.g., "PPNP", "MC100").
#' @param data_frame_files A list of file names corresponding to the data frames in `df`, used for validation purposes.
#' @param local_issues A list of issues encountered during previous checks, to which new issues will be appended.
#'
#' @return A list of issues encountered during the content consistency and integrity checks.
#'
#' @noRd
perform_core_checks <- function(df, name, variable, device_type, data_frame_files, local_issues) {
  current_df <- df[[name]]
  file_name <- data_frame_files[[name]]

  result <- process_dataframe(as.data.frame(current_df))
  current_df <- result$current_df
  current_df_old <- result$current_df_old

  local_issues <- check_index_sequence(current_df, current_df_old, name, device_type, file_name, local_issues)
  local_issues <- check_datetime_format(current_df, name, file_name, local_issues)
  local_issues <- check_id_unit_consistency(current_df, name, variable, file_name, local_issues)

  return(local_issues)
}
