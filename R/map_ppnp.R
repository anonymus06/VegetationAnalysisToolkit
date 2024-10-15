#' Transform PlantPen NDVI & PRI Data Files
#'
#' This function processes raw measurement data by merging it with user created index file data frame.
#' It then applies transformations such as date column addition, column renaming, data type conversion and
#' (optionally) splitting the 'code' column into multiple columns.
#'
#' @param data The raw data frame to be processed.
#' @param data_name The name of the data file being processed, used for deriving the date.
#' @param last_df The index data frame (typically the last data frame in the sequence) for merging.
#' @param split_code Logical. If `TRUE`, the function will split the `Code` column into multiple columns based on a configuration file.
#' @param env An environment object for logging warnings or errors during the processing.
#' @param variable A character string representing the target variable name (e.g., "NDVI", "PRI").
#' @param folder_path description
#'
#' @importFrom dplyr %>% filter
#' @return A transformed data frame ready for further analysis.
#'
#' @noRd
map_PPNP <- function(data, data_name, last_df, split_code, env, variable, folder_path){
 txt_elements <- data

 filtered_data <- txt_elements %>%
  filter(index %in% last_df$index) %>%
  merge(last_df, by = "index", all.x = TRUE)

 columns_to_keep <- c("index", variable, "description")

 if (nrow(filtered_data) > 0 && all(columns_to_keep %in% names(filtered_data))) {
  filtered_data <- filtered_data[, columns_to_keep, drop = FALSE]
 } else {
  stop("The function failed because 'filtered_data' is empty or missing required columns: ",
       paste(columns_to_keep, collapse = ", "),
       ". Ensure the input data and column names are correct.")
 }

 date_str <- gsub("[^0-9]", "", data_name)
 new_names <- c("index", variable, "Code", "Date")
 config_path <- file.path(dirname(folder_path), "config.txt")

 filtered_data <- add_date_column(filtered_data, date_str) %>%
  rename_columns_by_position(new_names) %>%
  convert_data

 if (split_code) {
  if (file.exists(config_path)) {
   config <- read_simple_config(config_path)
   filtered_data <- filtered_data %>%
    split_code_column(config, env)
  } else {
   warning(
    "Input configuration file not found at: ", config_path, ".",
    "If you want to utilize the functionality of `split_code`, ensure your configuration file is
    placed at the correct path and follows the guidelines specified in the README."
   )
  }
 }

 return(list(filtered_data))
}
