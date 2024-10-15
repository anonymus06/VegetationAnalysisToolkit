#' Safely Filter Data
#'
#' This function wraps the `filter_data` function with built-in error and warning handling. It ensures that any issues
#' encountered during the filtering process are logged into the provided environment.
#'
#' @param all_data A data frame or list of data frames to be filtered.
#' @param variable description
#' @param env An environment object used for logging warnings or errors during the process.
#'
#' @return Returns the filtered data, or `NULL` if an error occurs during the filtering process.
#'
#' @importFrom utils tryCatch
#' @noRd
safely_filter_data <- function(all_data, variable, env) {
 tryCatch({
  filtered_data <- filter_data(all_data, variable)
  return(filtered_data)
 }, error = function(e) {
  handle_general_condition(e, "error", env)
  return(NULL)
 }, warning = function(w) {
  handle_general_condition(w, "warning", env)
  # invokeRestart("muffleWarning")
 })
}
