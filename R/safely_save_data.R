#' Safely Save Data
#'
#' This function wraps the `save_data` function with built-in error and warning handling. It ensures that any issues
#' encountered during the saving process are logged into the provided environment and appropriate feedback is given.
#'
#' @param df A list or data frame of data to be processed.
#' @param output The directory path where the Excel files will be saved.
#' @param device_type A character string specifying the device type ("PPNP" for PlantPen or "MC100" for MC-100).
#' @param lower_limit The lower limit of acceptable values, below which data will be excluded from the filtered dataset.
#' @param upper_limit The upper limit of acceptable values, above which data will be excluded from the filtered dataset.
#' @param split_code Logical. If `TRUE`, the measurement codes will be split based on a configuration file.
#' @param env An environment object used for logging warnings or errors during the process.
#' @param variable A string representing the variable being processed (e.g., NDVI, PRI, or CCI).
#'
#' @return Returns `NULL` if an error occurs during the process. If the data is successfully saved,
#'         it prints a message about the saved data, but does not explicitly return a value.
#'
#' @importFrom utils tryCatch
#' @noRd
safely_save_data <- function(df, output, device_type,
                             lower_limit, upper_limit, split_code, env, variable) {
 tryCatch({
  save_data(df, output, device_type, lower_limit, upper_limit, split_code, env, variable)
 }, error = function(e) {
  handle_general_condition(e, "error", env)
  return(NULL)
 }, warning = function(w) {
  handle_general_condition(w, "warning", env)
  # invokeRestart("muffleWarning")
 })
}
