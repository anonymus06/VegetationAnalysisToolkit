#' Safely Map Data
#'
#' This function wraps the `filter_data` function with built-in error and warning handling. It ensures that any issues
#' encountered during the filtering process are logged into the provided environment.
#'
#' @param all_data A data frame or list of data frames to be filtered.
#' @param device_type A character string specifying the type of data ("PPNP" for PlantPen or "MC100" for MC-100).
#' @param split_code Logical. If `TRUE`, the measurement codes will be split based on a configuration file.
#' @param env An environment object used for logging warnings or errors during the process.
#' @param variable The variable name to process (e.g. NDVI).
#' @param folder_path The path to the folder containing the input data files.
#'
#' @return The processed data frame (or list of data frames) if successful.
#' If an error occurs, the function returns `NULL`.
#'
#' @noRd
safely_map_data <- function(all_data, device_type, split_code, env, variable = NULL, folder_path) {
  tryCatch(
    {
      map_all_data(all_data, split_code, env, variable, device_type, folder_path)
    },
    error = function(e) {
      handle_general_condition(e, "error", env)
      return(NULL)
    },
    warning = function(w) {
      handle_general_condition(w, "warning", env)
      invokeRestart("muffleWarning")
    }
  )
}
