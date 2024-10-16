#' Safely Read Data
#'
#' This function reads data from a specified folder path using the `read_data` function, with built-in error handling
#' to capture and log any issues.
#'
#' @param folder_path A character string specifying the path to the folder containing data.
#' @param device_type A character string specifying the device type ("PPNP" for PlantPen or "MC100" for MC-100).
#' @param env An environment object that stores issues and messages generated during the process.
#' @param skip_rows An integer specifying the number of rows to skip when reading the data files. Default is 4.
#' @param valid_patterns A character vector of valid filename patterns to match (optional).
#' If `NULL`, defaults are set based on the device type.
#'
#' @return A list containing:
#'         - `all_data`: A list of data frames containing data and index files, organized by file name or sheet name.
#'         - `data_frame_files`: A list mapping data frame names to their respective file paths.
#'         Returns `NULL` if an error occurs.
#'
#' @noRd
safely_read_data <- function(folder_path, device_type, env,
                             skip_rows, valid_patterns = NULL) {
 tryCatch({
  result <- read_data(folder_path, device_type, env, skip_rows, valid_patterns)
  return(result)
 }, error = function(e) {
  handle_general_condition(e, "error", env)
  return(NULL)
 }, warning = function(w) {
  handle_general_condition(w, "warning", env)
  # invokeRestart("muffleWarning")
 })
}
