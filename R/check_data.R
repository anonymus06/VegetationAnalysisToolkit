#' Diagnostic tool for data validation
#'
#' A comprehensive function to check data integrity and log potential issues.
#'
#' @param df A list containing data frames to be checked.
#' @param device_type A character string specifying the type of data ("PPNP" for PlantPen or "MC100" for MC-100).
#' @param data_frame_files A list of file names corresponding to the data frames in `df`, used for validation purposes.
#' @param variable The name of the variable being checked to tailor the checks for specific data types.
#'
#' @return A list containing:
#'         - `is_valid`: A logical value indicating whether the data passed the checks (TRUE) or failed (FALSE).
#'         - `validation_warnings`: A list of validation warnings encountered during the checks.
#'
#' @noRd
check_data <- function(df, device_type, data_frame_files, variable) {
  # ---- Initial Part Of Procedure ----

  if (device_type == "PPNP") {
    data_columns <-
      c(
        "index",
        "time",
        "id",
        "X760",
        "X635",
        "NDVI"
      )

    index_columns <-
      c(
        "index",
        "description"
      )

    data_types <-
      c(
        "integer",
        "character",
        "character",
        "integer",
        "integer",
        "numeric"
      )

    index_types <-
      c(
        "numeric",
        "character"
      )
  } else if (device_type == "MC100") {
    data_columns <-
      c(
        "Sample",
        "Time/Date",
        "Units",
        "Reading",
        "Lat",
        "Lon",
        "DOP",
        "# Sat"
      )

    index_columns <-
      c(
        "index",
        "description"
      )

    data_types <-
      c("character")

    index_types <-
      c(
        "numeric",
        "character"
      )
  } else {
    stop("Unknown 'device_type' parameter in 'check_data() function!'")
  }

  issues <- list()
  result <- list(is_valid = TRUE, validation_warnings = list())

  index_files <- sapply(data_frame_files, is_index_file)
  non_index_dfs <- df[!index_files]


  # ---- Main Part Of Procedure ----

  issues <- perform_all_checks(
    df, non_index_dfs, data_columns, index_columns, data_types, device_type,
    index_types, variable, data_frame_files
  )

  if (length(issues) > 0) {
    result$is_valid <- FALSE
    result$validation_warnings <- issues
  }

  return(result)
}
