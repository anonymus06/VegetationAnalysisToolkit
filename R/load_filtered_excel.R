#' Load the Filtered Excel File
#'
#' This function lists the files in the specified folder that match the variable name
#' and the "_filtered.xlsx" pattern. It ensures there's only one valid filtered file,
#' and loads the data from it.
#'
#' @param variable A string representing the variable being processed (e.g., NDVI, PRI, or CCI).
#' @param output_path The directory path where the Excel files are located.
#' @param start_row The row number from which the data should be read (default is 8).
#' @return A data frame containing the filtered data from the Excel file.
#' @importFrom openxlsx read.xlsx
#' @noRd
load_filtered_excel <- function(variable, output_path, start_row = 8) {

 # Create a pattern to match the filtered Excel file for the specified variable
 pattern <- paste0("^", tolower(variable), "_filtered\\.xlsx$") # Case-insensitive match

 # List files in the directory matching the pattern
 files <- list.files(output_path, pattern = pattern, full.names = TRUE, ignore.case = TRUE)

 # Exclude temporary files created by Excel (starting with "~$")
 files <- files[!grepl("^~\\$", basename(files))]

 # Check if any filtered file is found
 if (length(files) == 0) {
  stop("No filtered file found in the output folder.")
 }

 # Ensure only one filtered file exists
 if (length(files) > 1) {
  stop("Multiple filtered files found in the output folder. Please ensure only one filtered file exists.")
 }

 # Read the filtered data from the Excel file
 filtered_file_path <- files[1]
 df <- read.xlsx(filtered_file_path, startRow = start_row)

 return(df)
}

# Example usage:
# df <- load_filtered
