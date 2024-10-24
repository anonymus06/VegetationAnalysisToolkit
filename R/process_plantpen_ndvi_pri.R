#' Process Data for PlantPen NDVI & PRI
#'
#' This function reads and processes raw measurement files of the PlantPen NDVI & PRI device.
#'
#' @param folder_path The path to the folder containing the input data files.
#' @param output The directory where the processed data will be saved.
#' @param device_type A character string specifying the type of data ("PPNP" for PlantPen or "MC100" for MC-100).
#' @param lower_limit The lower limit for values.
#' @param upper_limit The upper limit for values.
#' @param variable The variable name to process (e.g. NDVI).
#' @param validate A boolean flag indicating whether to validate the input data.
#' @param split_code Logical. If `TRUE`, the measurement codes will be split based on a configuration file.
#'
#' @details
#' It performs several key actions:
#' \itemize{
#'   \item Optional, validating the data.
#'   \item Filtering out values outside of specified limits.
#'   \item Saving the processed data in `xlsx` format for further analysis.
#'   \item Optional, splitting the measurement code columns based on your given criteria.
#' }
#'
#' @section Input File Guidelines:
#' For detailed information on how to format your input files, please refer to the `Project Data Management Guidelines` section
#' in the package's `VegetationAnalysisToolkit` vignette file. These resources provide complete instructions on:
#'
#' \itemize{
#'   \item **File Naming Convention**: Proper naming of input files to facilitate processing.
#'   \item **Required Data Structure**: The expected structure of raw data files and corresponding index files.
#'   \item **Index File Requirement**: Guidelines for using Excel index files to specify measurement locations.
#'   \item **Split_Code Functionality**: How to create the `config` file for your project.
#' }
#'
#' It is highly recommended to consult the vignette to ensure your input data files adhere to these
#' guidelines, which will help avoid processing errors and ensure accurate results.
#'
#' @return None explicitly, but processed data is saved as Excel files in the specified output directory.
#'
#' @examples
#' \dontrun{
#' vignette("VegetationAnalysisToolkit")
#' }
#' \dontrun{
#' process_PlantPen_NDVI_PRI("path/to/ndvi_data", "path/to/output", 0.1, 0.9, "NDVI", TRUE)
#' }
#' @export
process_PlantPen_NDVI_PRI <- function(folder_path, output, device_type = "PPNP",
                                      lower_limit, upper_limit, variable, validate, split_code = FALSE) {
  env <- setup_general_warnings_env()
  validation_failed <- FALSE

  read_data_action <- function() safely_read_data(folder_path, device_type, env)
  filter_data_action <- function() safely_filter_data(loaded_data$all_data, variable, env)
  map_data_action <- function() safely_map_data(all_data, device_type, split_code, env, variable, folder_path)
  save_data_action <- function() safely_save_data(all_data, output, device_type, lower_limit, upper_limit, split_code, env, variable)

  loaded_data <- with_error_handling(read_data_action)
  all_data <- with_error_handling(filter_data_action)

  if (validate) {
    validation_failed <- !perform_validation(all_data, device_type, loaded_data$data_frame_files, variable, env)
  }

  if (!validation_failed) {
    all_data <- with_error_handling(map_data_action)
    with_error_handling(save_data_action)
  }

  summarize_and_log_issues(env, validation_run = validate)
  print_collected_messages(env, lower_limit, upper_limit, variable, device_name = "PlantPen NDVI & PRI", split_code, validate)
}
