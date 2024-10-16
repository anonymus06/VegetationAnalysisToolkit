#' Save Processed Data
#'
#' This function saves raw and filtered data to separate Excel files with headers. It combines all provided
#' data frames, applies filtering based on specified limits, and provides feedback on the filtering process.
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
#' @return The function does not return a value explicitly, but saves two Excel files to the specified directory:
#' one with the raw combined data and another with the filtered data. It also logs the details of the saved files
#' and the filtering process.
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @noRd
save_data <- function(df, output, device_type, lower_limit, upper_limit, split_code, env, variable) {
 if (device_type == "PPNP") {
  combined_df_all <- create_combined_data(df, variable, apply_data_filter = FALSE,
                                          lower_limit = NULL, upper_limit = NULL, split_code = split_code)
  combined_df_filtered <- create_combined_data(df, variable, apply_data_filter = TRUE,
                                               lower_limit = lower_limit, upper_limit = upper_limit, split_code = split_code)
 } else if (device_type == "MC100") {
  combined_df_all <- create_combined_data(df, variable, apply_data_filter = FALSE,
                                          lower_limit = NULL, upper_limit = NULL, split_code = split_code)
  combined_df_filtered <- create_combined_data(df, variable, apply_data_filter = TRUE,
                                               lower_limit = lower_limit, upper_limit = upper_limit, split_code = split_code)
 } else {
  stop("Invalid device type provided.")
 }

 # Provide feedback on the amount of data filtered
 calculate_and_print_feedback(combined_df_all, combined_df_filtered, output, variable, env)

 output_file_path_basic <- paste0(output, variable, "_raw.xlsx")
 output_file_path_filtered <- paste0(output, variable, "_filtered.xlsx")

 # Header information
 basic_file_name <- paste0(basename(getwd()), "/", output_file_path_basic)
 filtered_file_name <- paste0(basename(getwd()), "/", output_file_path_filtered)
 basic_file_content <- paste0("Unfiltered ", variable, " data")
 filtered_file_content <- paste0("Filtered ", variable, " data")

 # Generate headers
 project_header_basic <- generate_project_header(basic_file_name, basic_file_content, "save_processed_data.R")
 project_header_filtered <- generate_project_header(filtered_file_name, filtered_file_content, "save_processed_data.R")

 # This function converts headers to data frames for easy writing to Excel
 header_to_df <- function(header) {
  data.frame(Header = header, stringsAsFactors = FALSE)
 }

 header_df_basic <- header_to_df(project_header_basic)
 header_df_filtered <- header_to_df(project_header_filtered)

 wb_basic <- createWorkbook()
 wb_filtered <- createWorkbook()
 addWorksheet(wb_basic, "Sheet1")
 addWorksheet(wb_filtered, "Sheet1")
 writeData(wb_basic, "Sheet1", header_df_basic, startRow = 1, colNames = FALSE)
 writeData(wb_basic, "Sheet1", combined_df_all, startRow = nrow(header_df_basic) + 2, colNames = TRUE)
 writeData(wb_filtered, "Sheet1", header_df_filtered, startRow = 1, colNames = FALSE)
 writeData(wb_filtered, "Sheet1", combined_df_filtered, startRow = nrow(header_df_filtered) + 2, colNames = TRUE)
 saveWorkbook(wb_basic, output_file_path_basic, overwrite = TRUE)
 saveWorkbook(wb_filtered, output_file_path_filtered, overwrite = TRUE)

 add_message(env, paste("Data (filtered and raw) saved to:", output), "info")
 add_message(env, paste0("In filtered data, values outside of bounds: ", lower_limit, " and ", upper_limit,
                         " [", variable, "] were discarded!"), "info")
}
