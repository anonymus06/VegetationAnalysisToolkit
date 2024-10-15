#' Create a Combined DataFrame for Processed Data with Optional Filtering
#'
#' @description This function combines multiple data frames into one and optionally filters values
#'              based on specified upper and lower limits. The function also allows for splitting of the
#'              'Code' column into additional columns such as 'Position', 'Landuse', and 'InterRow' based on user input.
#'
#' @param all_data A list of data frames containing data.
#' @param variable A string specifying the column name (e.g., "NDVI").
#' @param apply_data_filter Logical, indicating whether data filtering should be applied.
#' @param lower_limit Numeric, the lower limit for data filtering.
#' @param upper_limit Numeric, the upper limit for data filtering.
#' @param split_code Logical, whether to split the 'Code' column into 'Position', 'Landuse', and 'InterRow'.
#'
#' @return A combined data frame with optional data filtering and additional columns if `split_code` is TRUE.
#'
#'
#' @importFrom dplyr bind_rows filter select
#' @noRd
create_combined_data <- function(all_data, variable, apply_data_filter = FALSE, lower_limit = NULL, upper_limit = NULL, split_code) {

 combined_df <- bind_rows(all_data)

 if(split_code) {
  combined_df <- combined_df %>%
   select(-index) %>%
   select(Date, Code, Position, Landuse, InterRow, !!sym(variable))
 } else {
  combined_df <- combined_df %>%
   select(-index) %>%
   select(Date, Code, !!sym(variable))
 }

 if (apply_data_filter && !is.null(lower_limit) && !is.null(upper_limit)) {
  combined_df <- combined_df %>%
   filter(!!sym(variable) > lower_limit) %>%
   filter(!!sym(variable) < upper_limit)
 }

 return(combined_df)
}
