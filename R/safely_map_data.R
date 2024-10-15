#' Safely Map Data
#'
#' This function wraps the `filter_data` function with built-in error and warning handling. It ensures that any issues
#' encountered during the filtering process are logged into the provided environment.
#'
#' @param all_data A data frame or list of data frames to be filtered.
#' @param device_type description
#' @param split_code description
#' @param env An environment object used for logging warnings or errors during the process.
#' @param variable description
#' @param folder_path description
#'
#' @return
#'
#' @importFrom utils tryCatch
#' @noRd
safely_map_data <- function(all_data, device_type, split_code, env, variable = NULL, folder_path) {
 tryCatch({
  map_all_data(all_data, split_code, env, variable, device_type, folder_path)
 }, error = function(e) {
  handle_general_condition(e, "error", env)
  return(NULL)
 }, warning = function(w) {
  handle_general_condition(w, "warning", env)
  invokeRestart("muffleWarning")
 })
}
