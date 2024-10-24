#' Map All Data
#'
#' This function applies the differently custom designed `map` functions to our data frames
#' depending on the device that we use.
#'
#' @param all_data A list of data frames to be processed.
#' @param split_code Logical, whether to split the 'Code' column into 'Position', 'Landuse', and 'InterRow'.
#' @param env The environment used for logging or handling any issues.
#' @param variable description
#' @param device_type description
#' @param folder_path description
#'
#' @return A list of processed data frames.
#'
#' @noRd
map_all_data <- function(all_data, split_code, env, variable, device_type, folder_path) {
  if (device_type == "PPNP") {
    last_df <- all_data[[length(all_data)]]
    mapply(
      function(data, data_name) {
        map_PPNP(data, data_name, last_df, split_code, env, variable, folder_path)
      },
      all_data[-length(all_data)],
      names(all_data)[-length(all_data)],
      SIMPLIFY = FALSE
    )
  } else if (device_type == "MC100") {
    map_MC100(all_data, split_code, env, variable, folder_path)
  } else {
    stop("Unknown map function")
  }
}
