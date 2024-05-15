#'

#' Read NDVI Data with Index files

#' @param folder_pathway The path to the folder containing NDVI data and its index files.
#'
#' @return A list containing data frames of NDVI data and index files from all read files,
#' organized by file name or sheet name for Excel files.
#'
#' @details The function first lists all files in the specified directory. It then iterates over each
#' file, checking its type and structure before reading the data. For Excel files, it reads each sheet
#' separately, excluding temporary files that may be generated during file editing. For text files, it
#' assumes a specific format (tab-separated values) but includes a provision for handling files with
#' varying structures through potential future enhancements. The function also includes checks to avoid
#' reading directories or temporary files that might be present in the folder.
#'
#' Special attention is given to the structure of text files, specifically the need for an empty tab
#' following the 'description' column to correctly read NDVI values. The function will halt with an
#' error message if it detects that NDVI values cannot be read due to file structure issues, advising
#' on potential remedies.
#'
#' @note The function assumes a relatively consistent structure for the data files, particularly for the
#' placement of the NDVI column in text files. If your data deviates significantly from the expected
#' format, manual adjustments or preprocessing may be necessary before using this function.
#'
#' Future Improvements:
#' The current implementation assumes a fixed structure for text files, specifically that they are
#' tab-separated and that the relevant data starts after skipping the first four rows.
#' There are two main areas identified for enhancement to improve the robustness and flexibility of this
#' function:

#' 1. Dynamic Separator Handling:
#'  - Currently, the function expects the separator to be a tab. However, data files may use
#'    different separators (e.g., spaces, commas).
#'  - A potential improvement is to automatically detect and adapt to the file's actual separator.
#'    This could involve testing for common separators and selecting the one that parses the file correctly.
#'  - Implementing a tryCatch block or similar error handling could allow the function to attempt
#'    reading the file with different separators and proceed with the one that succeeds, enhancing
#'    compatibility with various file formats.

#' 2. Adaptive Row Skipping:
#'  - The function is hardcoded to skip the first four rows of the text files, assuming these rows
#'    contain metadata or headers not relevant to the actual data.
#'  - Files may have a variable number of header or junk rows, requiring a more flexible approach.
#'  - An improvement could involve analyzing the initial rows of the file to determine where the actual
#'    data begins. This might include looking for the row where valid data (e.g., numeric values for NDVI)
#'    first appears or identifying common header patterns to skip.
#'  - Such adaptive row skipping would make the function more resilient to varying file structures and reduce
#'    the need for manual file preprocessing.

#' Implementing these improvements would significantly enhance the function's usability across a broader
#' range of data file structures, reducing manual adjustments and increasing the efficiency of data
#' ingestion workflows.
#'
#' @importFrom openxlsx getSheetNames loadWorkbook read.xlsx
#' @importFrom utils read.table
read_NDVI <- function(folder_pathway){

  # List all the files in the directory with their paths
  files <- list.files(folder_pathway, full.names = TRUE)

  all_data <- list() # Initialize an empty list to store the data

  # Reading in data in xlsx and txt format!
  for (file in files) { # Loop through each file

    # Check if the file is a directory
    if (file.info(file)$isdir) {
      cat("Skipping directory from reading:", file, "\n")
      next  # Skip the current iteration and move to the next file
    }

    # Check the file extension
    file_extension <- tools::file_ext(file)

    # Handle Excel files, excluding temporary files
    # [These are the NDVI index files]
    if (file_extension == "xlsx" && !startsWith(basename(file), "~$")) {

      sheets <- getSheetNames(file)
      wb <- loadWorkbook(file)
      all_data[sheets] <- lapply(sheets, function(sheet) {
        read.xlsx(wb, sheet = sheet, startRow = 1)
      })

      # Handle text files, also excluding temporary files
      # [These are the NDVI data files]
    } else if (file_extension == "txt" && !startsWith(basename(file), "~$")) {

      sheet_data <- read.table(file, header = TRUE, sep = "\t", skip = 4)

      # Future improvement placeholder for handling varying file structures!

      if (all(is.na(sheet_data$NDVI))) {
        # Stop the script and display an error message
        stop("Your input text file's structure is corrupted! There has to be an empty tab after the 'description' column name! Download your raw files again, or as an alternative: delete 'description' column name and any unnecessary tabs following NDVI column from the input text file, then try to run it again!")
      }

      all_data[basename(file)] <- list(sheet_data)

      # Skip temporary files to avoid duplicates
    } else if (startsWith(basename(file), "~$")) {

      cat("Temporary file found and skipped: ", basename(file), "\n")
      next

      # Log any skipped files for user awareness
    } else{

      cat("Skipped elements from reading:", basename(file), "\n")

    }

  }

  return(all_data)
}

#' Read Chlorophyll Data with Index files

#' @param folder_path A character string specifying the path to the folder containing the chlorophyll
#' data files.
#'
#' @return A list containing data frames of chlorophyll data, organized by file name for CSV files or
#' sheet name for Excel files.
#'
#' @details The function begins by listing all files in the specified directory. For each file, it checks
#' if the file is a directory or a temporary file, skipping these. For Excel files, it reads each sheet
#' separately, ensuring that even if a file is temporarily generated (not starting with "~$"), it's considered
#' for data extraction. For CSV files, it employs a specific separator (";") and ensures UTF-8 encoding for
#' compatibility with diverse data formats. The function includes error handling via tryCatch to manage issues
#' in file reading, logging errors without halting execution.
#'
#' @note This function currently assumes specific file extensions (.xlsx for Excel and .CSV for CSV files) and
#' encoding for CSV files. Future improvements may include dynamic separator handling and adaptive row skipping
#' to accommodate files with varying structures and formats.
#'
#' Future Improvements:
#' - Extend support for additional file formats and separators, moving beyond the current limitations of ";"
#'   for CSV files and the strict file extension checks.
#' - Implement adaptive row skipping for CSV files to automatically adjust to files with different header
#'   structures, enhancing the function's flexibility and user-friendliness.
#'
#' @importFrom openxlsx getSheetNames loadWorkbook read.xlsx
#' @importFrom utils read.csv
read_Chlorophyll <- function(folder_path) {

  files <- list.files(folder_path, full.names = TRUE) # List all the files in the dir with their paths

  all_data <- list() # Initialize an empty list to store the data

  # Reading in data in xlsx and csv format!
  for (file in files) { # Loop through each file

    tryCatch({
      # Check if the file is a directory
      if (file.info(file)$isdir) {
        cat("Skipping directory from reading:", file, "\n")
        next  # Skip the current iteration and move to the next file
      }

      file_extension <- tools::file_ext(file) # Extract the file extension for type checking

      # Handle Excel files, ensuring to read data from non-temporary files only
      # [These are the Chlorophyl index files]
      if (file_extension == "xlsx" && !startsWith(basename(file), "~$")) {
        # Read-in each sheet from that excel file!!!
        sheets <- getSheetNames(file) # Get sheet names from the workbook
        wb <- loadWorkbook(file) # Load the workbook
        all_data[sheets] <- lapply(sheets, function(sheet) {  # Read each sheet into a list element, indexed by sheet name
          read.xlsx(wb, sheet = sheet, startRow = 1)
        })

        # Handle CSV files, specifying the separator and encoding
        # [These are the Chlorophyl data files]
      } else if (file_extension == "CSV" && !startsWith(basename(file), "~$")) {

        sheet_data <- read.csv(file, sep = ";", header = TRUE, encoding = "UTF-8", check.names = FALSE)

        # Future improvement placeholder for handling varying file structures!

        all_data[basename(file)] <- list(sheet_data)

        # Skip temporary files to prevent duplicate readings
      } else if (startsWith(basename(file), "~$")) {
        cat("Temporary file found and skipped: ", basename(file), "\n")
        next

        # Log any files that were skipped due to unexpected conditions
      } else {
        cat("Skipped elements from reading:", basename(file), "\n")
      }

    }, error = function(e) {
      # Log any errors encountered during file reading for troubleshooting
      cat("Error reading file:", file, "- Error message:", e$message, "\n")
    })
  }

  # Optionally, check if all_data is empty and handle it
  if (length(all_data) == 0) {
    stop("No data was read. Please check the folder path and file formats.")
  }

  return(all_data)
}
