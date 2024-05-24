#' Ensure that each file starts with a header comment that explains the purpose of the file and the functions it contains.

#' Process Chlorophyll Data for Analysis
#'
#' @param folder_path The directory path where the chlorophyll data files are located.
#' @param output The directory path where the processed files will be saved.
#' @param filename The base name for the output files.
#' @param lower_limit The lower limit for chlorophyll data filtering.
#' @param upper_limit The upper limit for chlorophyll data filtering.
#' @param variable The target variable name for consistency checks, typically "CCI" for chlorophyll content index.
#' @param validate A logical flag to indicate whether data integrity checks should be performed.
#'
#' @description This function streamlines the process of reading, validating, transforming, and saving chlorophyll data.
#'              It ensures that the data is in the correct format and structure for further analysis.
#'
#' @details
#' The function performs several key operations:
#'   1. Reads in chlorophyll data from specified folder paths.
#'   2. Optionally validates the data for integrity and structure.
#'   3. Applies a series of transformations to standardize and enrich the data.
#'   4. Saves the transformed data into designated output files.
#'
#' @section README:
#' Guidelines for Preparing Your Input Files:
#'
#' Ensure your input data files are properly formatted and named according to the following guidelines to
#' facilitate successful processing:
#'
#' - **File Naming Convention**: Name your data files to include the date of measurement. Use the format %y%m%d for dates (e.g., 230528 for May 28, 2023) or %y%m%d%h for specific hours (e.g., 23052810 for 10 AM on May 28, 2023). Simple date formats are preferred, such as 230528 or 23052810.
#'
#' - **Required Data Structure**: Your data has to be in the first sheet and first column of a CSV file. Although the sheet name can be arbitrary, ensure that column names start from the first row.
#'   Your data should include the following sequence of column name-data pairs in the first column:
#'   <Sample, Time/Date, Units, Reading, Lat, Lon, DOP, # Sat, Point>. This structure is crucial for the script to correctly interpret and process your data.
#'
#' - **Index File Requirement**: Accompany your measurement data with an Excel file containing indexes and associated codes for each measurement. Name the tabs within this Excel file after the measurement dates to correlate them directly with the data files.
#'
#' **Coding and Marking of Data Sites**:
#' In the provided example (refer to the index file's "position" column), data sites are designated with specific codes:
#' RA, RF, EA, EF, SZA, SZF, CSUA, CSUF, CSUASK, CSUFSK, CSUAHP, CSUAHPSK, CSRA, CSRF, CSRASK, CSRFSK, CSRAHP, CSRAHPSK, CSTA, CSTF, CSTASK, CSTFSK.
#'
#' **Supported Site Coding**:
#' - **POSITION**: F (Front), A (Adjacent), HP (High Point)
#' - **LANDUSE**: CSU (Crop Standard Use), CSR (Crop Sustainable Rotation), CST (Crop Sustainable Tillage), R (Rural), E (Ecological), SZ (Special Zone)
#' - **INTERROW**: S (Standard Row), SK (Skipped Row)
#'
#' It is recommended to manually review your input data files for adherence to these guidelines before proceeding with the script. This preliminary check helps ensure smooth data processing and accurate results.
#'
#' @examples
#' # Process chlorophyll data with validation and save the results
#' \dontrun{
#' process_Chlorophyll("path/to/data", "path/to/output", "chlorophyll_processed",
#' 0.2, 2.5, "CCI", TRUE)
#' }
#'
#' @return None explicitly, but the processed data is saved as Excel files in the specified output directory.
#'
#' @importFrom dplyr %>%
#'
#' @export
process_Chlorophyll <- function(folder_path, output, filename, lower_limit, upper_limit, variable, validate){
  # Read-in input data
  # The function 'read_Chlorophyll' is expected to handle the reading of chlorophyll data
  # stored in various formats and structure them for further processing.
  all_data <- read_Chlorophyll(folder_path)

  # Checks for data integrity
  # This optional step, activated by 'validate == TRUE', checks the consistency and completeness
  # of the chlorophyll data, ensuring it meets the expected standards for analysis.
  if (validate == TRUE){ check_data(all_data, variable)}

  # Apply data transformations
  # This sequence of transformations prepares the chlorophyll data for analysis by standardizing its format,
  # adding date columns, renaming columns for consistency, converting data types, and splitting codes for detailed categorization.
  all_data <- all_data %>% map_CCI

  # Save final results
  save_cci(all_data, output, filename, lower_limit, upper_limit)
}

#' Process NDVI Data for Analysis
#'
#' @param folder_path The directory path where the NDVI data files are located.
#' @param output The directory path where the processed files will be saved.
#' @param filename The base name for the output files.
#' @param lower_limit The lower limit for NDVI data filtering.
#' @param upper_limit The upper limit for NDVI data filtering.
#' @param variable The target variable name for consistency checks, typically "NDVI".
#' @param validate A logical flag to indicate whether data integrity checks should be performed.
#'
#' @description This function reads, validates, transforms, and saves NDVI data from raw measurement files.
#'              It filters out extreme values and saves the data in a structured format for further analysis.
#'
#' @details
#' The function performs several key operations:
#'   1. Reads in NDVI data from specified folder paths.
#'   2. Removes unnecessary columns that are not relevant for analysis.
#'   3. Optionally validates the data for integrity and structure.
#'   4. Applies transformations to standardize and enrich the data.
#'   5. Saves the transformed data into designated output files.
#'
#' @section Input File Guidelines:
#' Ensure your input data files are properly formatted and named according to the following guidelines to
#' facilitate successful processing:
#'
#' - **File Naming Convention**: Your input file names can be any strings, but for Excel files, sheet names must
#'   reflect the measurement date in the format %y%m%d (e.g., plantpen_NDVI_230928) to facilitate date calculation.
#'
#' - **Required Data Structure**: In the Excel file, column names (index, description) must start from the first row.
#'   The raw data Text file should include columns: <index, time, id, 760 [nm], 635 [nm], NDVI>. The script is
#'   optimized to skip the first four non-relevant rows.
#'
#' - **Index File Requirement**: Accompany your NDVI measurements with an Excel file specifying each measurement's location.
#'
#' **Coding and Marking of Data Sites**:
#' Data sites are designated with specific codes as seen in the "description" column of the Excel index file.
#' Supported site coding is detailed in the sample output and includes codes for POSITION, LANDUSE, and INTERROW.
#'
#' It is recommended to manually review your input data files for adherence to these guidelines before proceeding with
#' the script. This preliminary check helps ensure smooth data processing and accurate results.
#'
#' @examples
#' \dontrun{
#' process_NDVI("path/to/ndvi_data", "path/to/output", "ndvi_processed", 0.1, 0.9, "NDVI", TRUE)
#' }
#'
#' @return None explicitly, but processed NDVI data is saved as Excel files in the specified output directory.
#'
#' @export
process_NDVI <-
 function(folder_path,
          output,
          filename,
          lower_limit,
          upper_limit,
          variable,
          validate) {
  general_warnings_env <- new.env()
  general_warnings_env$all_warnings <- list()
  validation_warnings_env <- new.env()
  validation_warnings_env$validation_warnings <- list()
  validation_failed <- FALSE

  # Custom warning collector for general warnings
  collect_general_warning <- function(w) {
   general_warnings_env$all_warnings <-
    append(general_warnings_env$all_warnings,
           list(conditionMessage(w)))
   invokeRestart("muffleWarning")
  }


  # Read-in input data from specified folder path
  all_data <- tryCatch({
   read_NDVI(folder_path)
  }, error = function(e) {
   handle_general_condition(e, "error")
  })

  # Filter out non-data columns from the raw NDVI files
  all_data <- tryCatch({
   filter_data(all_data)
  }, error = function(e) {
   handle_general_condition(e, "error")
  })

  # Optionally validate the data for integrity and structure
  if (validate) {
   validation_result <- withCallingHandlers({
    check_data(all_data, variable)
   }, warning = collect_general_warning, error = function(e) {
    handle_general_condition(e, "error")
   })
   is_valid <- validation_result$is_valid
   validation_warnings <- validation_result$validation_warnings

   if (length(validation_warnings) > 0) {
    summary_msg <-
     paste(
      "Input data validation completed with",
      length(validation_warnings),
      "warnings.\nCheck 'val_issues.txt' for details."
     )
    log_validation_issue(paste(
     "Detailed warnings:\n",
     paste(validation_warnings, collapse = "\n")
    ))
    warning(summary_msg)
   } else {
    message("Validation completed without warnings.")
   }

   if (!is_valid) {
    validation_failed <- TRUE
   }
  }

  if (!validation_failed) {

   # Apply a series of transformations to prepare the data for analysis
   last_df <-
    all_data[[length(all_data)]]  # Extract the last dataframe for special processing
   all_data <- tryCatch({
    mapply(
     map_NDVI,
     all_data[-length(all_data)],
     names(all_data)[-length(all_data)],
     MoreArgs = list(last_df = last_df),
     SIMPLIFY = FALSE
    )
   }, error = function(e) {
    handle_general_condition(e, "error")
   })

   # Save the processed data to Excel files in the specified output directory
   tryCatch({
    save_ndvi(all_data, output, filename, lower_limit, upper_limit)
   }, error = function(e) {
    handle_general_condition(e, "error")
   })
  }


  # Summarize and print general warnings
  if (length(general_warnings_env$all_warnings) > 0) {
   summary_msg <-
    paste(
     "Processing completed with",
     length(general_warnings_env$all_warnings),
     "general warnings.\nCheck 'error_log.txt' for details."
    )
   log_general_issue(paste(
    "Detailed warnings:\n",
    paste(general_warnings_env$all_warnings, collapse = "\n")
   ))
   warning(summary_msg)
  } else {
   message("Processing completed without general warnings.")
  }

 }

#' Function to select the desired rows (defined in the excel file) from the raw data (txt file)!
#'
#' @param data The raw data frame to be processed.
#' @param data_name The name of the data file being processed.
#' @param last_df The last data frame for reference.
#'
#' @details This function filters and transforms the data based on specified criteria and merges it with the last data frame.
#'
#' @importFrom dplyr %>% filter
#'
#' @return A transformed data frame.
map_NDVI <- function(data, data_name, last_df){
  # Assuming txt_elements is the last dataframe being processed
  txt_elements <- data

  # Filter and merge
  filtered_data <- txt_elements %>%
    filter(index %in% last_df$index) %>%
    merge(last_df, by = "index", all.x = TRUE)

  columns_to_keep <- c("index", "NDVI", "description")   # Column names to keep

  # Check if filtered_data has rows and if columns_to_keep exist in filtered_data
  if (nrow(filtered_data) > 0 && all(columns_to_keep %in% names(filtered_data))) {
    filtered_data <- filtered_data[, columns_to_keep, drop = FALSE]
  } else {
    # Handle the case where filtered_data is empty or columns are missing
    stop("The 'map_elements' function failed to run because 'filtered_data' is empty or does not contain all required columns: ", paste(columns_to_keep, collapse = ", "), "! This may be due to 'txt_elements' or 'last_df' being empty, no matching indices between the data frames, or the input data having different column names than expected. Review your input files and ensure the 'columns_to_keep' variable is correctly defined with the expected column names. This variable is crucial for filtering the relevant data columns.")

  }

  date_str <- gsub("[^0-9]", "", data_name)
  #print(date_str)
  #print(data_name)

  # Further processing...

  new_names <- c("index", "NDVI", "Code", "Date") # Define the new column names
  filtered_data <- addDateColumn(filtered_data, date_str) %>% # We add date columns to work!
    renameColumnsByPosition(new_names) %>%  # Rename the selected columns!
    convertData %>% # Change data type: [Sample NDVI Code Date -> numeric-numeric-character-date]
    splitCodeColumn # Apply splitCodeColumn to get Landuse, Position and InterRow columns!

  return(filtered_data)
}

#' Map and Transform Elements from Data Files

#' @param data A list containing the raw data frames to be processed.
#'
#' @return A list of transformed data frames, each corresponding to a merged and transformed pair of raw data and index data.
#'
#' @details The function follows these steps:
#'          1. Preprocesses all data frames to standardize their structure and add necessary identifiers.
#'          2. Merges each data frame with its corresponding index frame based on a common identifier.
#'          3. Applies a series of transformations to the merged data frames to select specific columns,
#'             add date columns, rename columns, and convert data types as necessary.
#'
#'          The preprocessing step involves splitting columns where necessary and assigning unique identifiers
#'          to ensure that data from different segments can be accurately merged. The merging step combines
#'          data frames with their indexes to enrich the data with additional context. Finally, the transformation
#'          step standardizes the format of the data frames, making them ready for analysis.
map_CCI <- function(data){

  # ----initial setup

  # Preprocess the data to prepare for merging.
  preprocessed_data <- preprocess_all_data_frames(data)

  # Extract the preprocessed data frames and their corresponding index frames.
  csv_elements <- preprocessed_data$data_files_processed
  index_elements <- preprocessed_data$index_files_processed

  # ----main

  # Merge the preprocessed data frames with their corresponding index frames.
  # This step enriches each data frame with additional context from the index frames.
  merged_data_frames <- merge_data_and_index_frames(csv_elements, index_elements)

  # Apply transformations to the merged data frames.
  # This includes selecting specific columns, renaming them, adding date columns, converting data types,
  # and potentially splitting columns for more detailed categorization.
  transformed_data_frames <- apply_transformations(merged_data_frames)

  # Return the list of transformed data frames, ready for analysis.
  return(transformed_data_frames)
}















