#' Process Data for MC-100 Chlorophyll Concentration Meter
#'
#' This function processes raw data from the MC-100 Chlorophyll Concentration Meter.
#'
#' @param folder_path The directory path where the data files are located.
#' @param output The directory path where the processed files will be saved.
#' @param device_type A character string specifying the type of data ("PPNP" for PlantPen or "MC100" for MC-100).
#' @param lower_limit The lower limit for data filtering.
#' @param upper_limit The upper limit for data filtering.
#' @param variable The target variable name for consistency checks, typically "CCI" for chlorophyll content index.
#' @param validate A logical flag to indicate whether data integrity checks should be performed.
#' @param split_code Logical. If `TRUE`, measurement codes will be split based on a configuration file.
#'
#' @details
#' Key operations of the function include:
#' \itemize{
#'   \item Optional, validating the data.
#'   \item Applying a series of transformations and filters to clean the data.
#'   \item Saving the processed data in `xlsx` format for further analysis.
#'   \item Optional, splitting the measurement code columns based on your given criteria.
#' }
#'
#' For input file formatting details, please refer to the package's vignette.
#'
#' @return None explicitly, but the processed data is saved as Excel files in the specified output directory.
#'
#' @examples
#' \dontrun{
#' process_MC100_Chlorophyll("path/to/data", "path/to/output", 0.2, 2.5, "CCI", TRUE)
#' }
#' @export
process_MC100_Chlorophyll <- function(folder_path, output, device_type = "MC100",
                                      lower_limit, upper_limit, variable, validate, split_code = FALSE) {
  env <- setup_general_warnings_env()
  validation_failed <- FALSE

  read_data_action <- function() safely_read_data(folder_path, device_type, env)
  map_data_action <- function() safely_map_data(all_data, device_type, split_code, env, variable, folder_path)
  save_data_action <- function() safely_save_data(all_data, output, device_type, lower_limit, upper_limit, split_code, env, variable)

  loaded_data <- with_error_handling(read_data_action)
  all_data <- loaded_data$all_data

  if (validate) {
    validation_failed <- !perform_validation(all_data, device_type, loaded_data$data_frame_files, variable, env)
  }

  if (!validation_failed) {
    all_data <- with_error_handling(map_data_action)
    with_error_handling(save_data_action)
  }

  summarize_and_log_issues(env, validation_run = validate)
  print_collected_messages(env, lower_limit, upper_limit, variable, device_name = "MC-100 Chlorophyll Concentration Meter", split_code, validate)
}
