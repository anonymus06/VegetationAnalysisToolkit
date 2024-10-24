#' Perform Validation on the Data
#'
#' This function performs validation on the provided data, handling any warnings or errors that arise during
#' the validation process. It logs validation issues into the provided environment and returns whether the data is valid.
#'
#' @param df The data frame to be validated.
#' @param device_type The type of data being validated (e.g., "PPNP", "MC100").
#' @param data_frame_files A list mapping data frame names to their respective file paths.
#' @param variable A string representing the variable being validated (e.g., NDVI, PRI, or CCI).
#' @param env An environment object used for logging validation warnings, errors, and messages.
#'
#' @return A logical value indicating whether the data is valid (`TRUE` if valid, `FALSE` if validation failed).
#' @noRd
perform_validation <- function(df, device_type, data_frame_files, variable, env) {
  validation_result <- withCallingHandlers(
    {
      check_data(df, device_type, data_frame_files, variable)
    },
    warning = function(w) {
      handle_general_condition(w, "warning", env)
    },
    error = function(e) {
      handle_general_condition(e, "error", env)
    }
  )

  if (length(validation_result$validation_warnings) > 0) {
    log_validation_issues(validation_result$validation_warnings, env)
  } else {
    add_message(env, "Validation completed without any known issues. Note: Not all potential issues are checked.", "issues")
  }

  return(validation_result$is_valid)
}
