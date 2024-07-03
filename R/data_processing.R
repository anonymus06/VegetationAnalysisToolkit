#' Functions specifically involved in processing data.


#' Process NDVI Data for Analysis
#'
#' @param folder_path The path to the folder containing the input NDVI data files.
#' @param output The directory where the processed data will be saved.
#' @param filename The base name for the output files.
#' @param lower_limit The lower limit for NDVI values.
#' @param upper_limit The upper limit for NDVI values.
#' @param variable The variable name to process within the NDVI data.
#' @param validate A boolean flag indicating whether to validate the input data.
#'
#' @description This function reads raw NDVI measumenet files made by XXX, optionally validates the datasets,
#' filters out extreme values and saves the data in a structured format for further analysis.
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
process_NDVI <- function(folder_path, output, filename, lower_limit, upper_limit, variable, validate) {
 # Setup environments
 env <- setup_general_warnings_env()
 # issues <- env$all_issues
 # messages <- env$messages

 validation_failed <- FALSE # todo- put unto env functiion


 # Define actions as functions
 read_ndvi_action <- function() safely_read_NDVI(folder_path, env)
 # read_ndvi_action <- function() safely_read_NDVI(folder_path, issues)
 filter_data_action <- function() safely_filter_data(loaded_data$all_data, env)
 map_ndvi_action <- function() safely_map_NDVI(all_data, env)
 # map_ndvi_action <- function() {
 #  last_df <- all_data[[length(all_data)]]  # Extract the last dataframe for special processing
 #
 #  mapply(
 #   safely_map_NDVI,
 #   all_data[-length(all_data)],
 #   names(all_data)[-length(all_data)],
 #   MoreArgs = list(last_df = last_df, env = env),
 #   SIMPLIFY = FALSE
 #  )
 # }

 # save_ndvi_action <- function() safely_save_ndvi(all_data, output, filename, lower_limit, upper_limit, issues, messages)
 save_ndvi_action <- function() safely_save_ndvi(all_data, output, filename, lower_limit, upper_limit, env)

 # add_message(env, "Device: ", "info")
 # add_message(env, paste0("Selected vairable: ", variable), "info")
 # add_message(env, paste0("Lower limit: ", lower_limit), "info")
 # add_message(env, paste0("Upper limit: ", upper_limit), "info")
 # add_message(env, "", "info")

 # Execute actions with error handling
 loaded_data <- with_error_handling(read_ndvi_action, env)
 all_data <- with_error_handling(filter_data_action, env)

 # Optionally validate the data for integrity and structure
 if (validate) {
  # validation_failed <- !perform_validation(all_data, "NDVI", loaded_data$data_frame_files, issues, messages)
  validation_failed <- !perform_validation(all_data, "NDVI", loaded_data$data_frame_files, env)

 }

 # Process and save data if validation passed
 if (!validation_failed) {
  all_data <- with_error_handling(map_ndvi_action, env)
  with_error_handling(save_ndvi_action, env)
 }

 summarize_and_log_issues(env, validation_run = validate)   # Summarize and log general warnings and errors

 # print(env$messages)
 # Print all collected messages
 # print_collected_messages(env)
 print_collected_messages(env, lower_limit, upper_limit, variable, device = "Your Device Name", validate)

}

#' A function that gets rid of every junk column that goes after the NDVI column

#' @param df A list of data frames where the last data frame contains the NDVI column.
#'
#' @return A modified list of data frames where the last data frame in the list having columns only up
#' to and including the NDVI column.
#'
#' @details The function identifies the position of the NDVI column in the last data frame of
#' the input list. Based on this position, the function then truncates the last
#' data frame in the list to include only columns up to and including the NDVI column.
#'
#' @note This function assumes that there is an `NDVI` column in the last data frame of the list.
#' If these assumption does not hold, the function may not perform as expected.
filter_data <- function(df){

 # Identify the position of the NDVI column in the last data frame of the list
 ndvi_col_index <- which(names(df[[length(df)]]) == "NDVI")
 # Check if NDVI column is found
 if (length(ndvi_col_index) == 0) { # todo: maybe it could also work with this method
  # stop("NDVI column not found in the dataset") # todo: with other method! - to log file with message
  return(NULL)
 }
 # ndvi_col_index <- 6 # if coloumns are deleted, position can change!

 # Update the last data frame in the list to retain only columns up to and including the NDVI column #todo: more reliable method - the last one ll always be the data file because of how it reads it, but still!
 # This operation ensures that all columns following the NDVI column are removed
 df[[length(df)]] <- df[[length(df)]][, 1:ndvi_col_index]

 return(df)
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

#' Save NDVI Data to Excel Files

#' @param df A list or data frame of NDVI data to be processed.
#' @param output The directory path where the Excel files will be saved.
#' @param filename The base name for the output Excel files. The function will
#'        append "_filtered" to the filename for the filtered data file.
#' @param lower_limit The lower limit of acceptable NDVI values. Rows with NDVI
#'        values below this limit will be filtered out from the final data.
#' @param upper_limit The upper limit of acceptable NDVI values. Rows with NDVI
#'        values above this limit will be filtered out from the final data.
#'
#' @details Filters NDVI data, then saves the raw and filtered data to separate Excel files.
#'
#' @return The function does not explicitly return a value but saves two Excel
#'         files to the specified output directory: one containing the raw
#'         combined NDVI data and another containing the filtered NDVI data.
#'         It also prints to the console the location of the saved files and
#'         information about the filtered data.
#'
#' @importFrom openxlsx write.xlsx
save_ndvi <- function(df, output, filename, lower_limit, upper_limit, env) {
 # Combine all provided NDVI data frames into one and create a filtered version!
 combined_df_all <- create_combined_ndvi(df)
 combined_df_filtered <- create_combined_ndvi(df, apply_ndvi_filter = TRUE, lower_limit = lower_limit, upper_limit = upper_limit) # Specify the range for acceptable NDVI values

 # Save the filtered data frame as a global variable for ease of access
 # filtered <<- combined_df_filtered

 # Provide feedback on the amount of data filtered
 calculate_and_print_feedback(combined_df_all, combined_df_filtered, "ndvi", env)

 # Define the filename for the raw data Excel file and save it
 output0 <- paste0(output, filename)
 write.xlsx(combined_df_all, output0, rowNames = FALSE)

 # Modify the filename for the filtered data Excel file and save it
 output0 <- paste0(output, sub(".xlsx$", "_filtered.xlsx", filename))
 write.xlsx(combined_df_filtered, output0, rowNames = FALSE)

 # Print confirmation messages with details about the saved files and filtering
 # cat("Data (filtered and raw) saved to", output, "\n")
 # cat("In filtered data values outside of bounds:", lower_limit, "and", upper_limit, "[NDVI] were discarded!\n")
 # messages <- add_message(messages, paste("Data (filtered and raw) saved to", output_dir), "info")
 # messages <- add_message(messages, paste("In filtered data values outside of bounds:", lower_limit, "and", upper_limit, "[NDVI] were discarded!"), "info")
 add_message(env, paste("Data (filtered and raw) saved to:", output_dir), "info")
 add_message(env, paste("In filtered data values outside of bounds:", lower_limit, "and", upper_limit, "were discarded!"), "info")
}

#' Save CCI Data to Excel Files

#' @param df A list or data frame of CCI data to be processed.
#' @param output The directory path where the Excel files will be saved.
#' @param filename The base name for the output Excel files, to which "_filtered"
#'        will be appended for the file containing filtered data.
#' @param lower_limit The lower limit of acceptable CCI values, below which data
#'        will be excluded from the filtered dataset.
#' @param upper_limit The upper limit of acceptable CCI values, above which data
#'        will be excluded from the filtered dataset.
#'
#' @details Filters CCI data, then saves the raw and filtered data to separate Excel files.
#'
#' @return Similar to `save_ndvi`, the function does not return a value explicitly, it saves two
#'         Excel files to the specified directory: one with the raw combined CCI data
#'         and another with the filtered CCI data. It also outputs to the console the
#'         paths to the saved files and details about the filtering process.
#'
#' @importFrom openxlsx write.xlsx
save_cci <- function(df, output, filename, lower_limit, upper_limit) {
 # Combine CCI data frames into one and also create a version filtered by specified CCI value limits
 combined_df_all <- create_combined_cci(df)
 combined_df_filtered <- create_combined_cci(df, apply_cci_filter = TRUE, lower_limit = lower_limit, upper_limit = upper_limit) # Specify the range for acceptable NDVI values

 # Save the filtered data frame as a global variable for convenience
 # filtered <<- combined_df_filtered

 # Calculate feedback on the amount of data filtered and print it
 calculate_and_print_feedback(combined_df_all, combined_df_filtered, "cci")

 # Save the data frame to an Excel file!
 output0 <- paste0(output, filename)
 write.xlsx(combined_df_all, output0, rowNames = FALSE)

 # Adjust filename for the filtered data Excel file and save it
 output0 <- paste0(output, sub(".xlsx$", "_filtered.xlsx", filename))
 write.xlsx(combined_df_filtered, output0, rowNames = FALSE)

 # Print confirmation messages detailing the saved files and the filtering process
 cat("Data (filtered and raw) saved to", output, "\n")
 cat("In filtered data values outside of bounds:", lower_limit, "and", upper_limit, "[CCI] were discarded!\n")
}

#' Merge Data and Index Frames

#' This function merges data frames from csv_elements with their corresponding index frames from index_elements.
#' The matching is based on a common 'UniqueID' column present in both sets of data frames. It is designed to work
#' with data frames named such that the base name of a CSV file matches the name of its corresponding index frame.
#'
#' @param csv_elements A named list of data frames originating from CSV files.
#' @param index_elements A named list of index data frames.
#'
#' @return A list of merged data frames, named by the base name of the original CSV files, without the .CSV extension.
merge_data_and_index_frames <- function(csv_elements, index_elements) {

 # Initialize an empty list to store the results of merging data frames.
 merged_data_frames <- list()

 # Iterate over csv_elements
 for (csv_name in names(csv_elements)) {
  # Extract the base name by removing the .CSV extension, used for matching with index names.
  base_name <- gsub("\\.CSV$", "", csv_name)

  # Check if there's a corresponding index frame for the current CSV file.
  if(base_name %in% names(index_elements)) {
   # Retrieve the data frame and its corresponding index frame.
   data_df <- csv_elements[[csv_name]]
   index_df <- index_elements[[base_name]]

   # Merge by 'UniqueID' if it exists in both data frames
   if("UniqueID" %in% colnames(data_df) && "UniqueID" %in% colnames(index_df)) {
    merged_df <- merge(data_df, index_df, by = "UniqueID", all = TRUE)
    merged_data_frames[[base_name]] <- merged_df
   } else {
    # Issue a warning if 'UniqueID' is missing in either the data or index frame.
    warning(paste("Missing 'UniqueID' in either data or index data frame for base name:", base_name))
   }
  } else {
   # Issue a warning if no corresponding index frame is found for the base name.
   warning(paste("Base name not found in index files:", base_name))
  }
 }

 return(merged_data_frames)
}

#' Apply Transformations to Merged Data Frames

#' @param merged_data_frames A list of data frames that have been merged with their corresponding index data.
#'
#' @return A list of transformed data frames, with the same names as the input merged data frames.
#'
#' @details The function iterates over each data frame in the input list, performing the following steps:
#'          1. Filters the data frame to retain only specified columns.
#'          2. Adds a date column derived from the data frame's name.
#'          3. Renames the columns based on predefined new names.
#'          4. Converts data types of the columns to ensure consistency.
#'          5. Optionally splits one of the columns into multiple columns for detailed categorization.
apply_transformations <- function(merged_data_frames) {

 # Iterate over each merged data frame to apply transformations
 transformed_data_frames <- lapply(names(merged_data_frames), function(name) {
  df <- merged_data_frames[[name]]
  columns_to_keep <- c("index", "Reading", "Position") # Define columns to keep from the merged data frame

  # Check if all required columns are present in the data frame
  if(all(columns_to_keep %in% names(df))) {
   # Keep only specified columns. This ensures that the number of columns in 'df_filtered'
   # will always match the number of 'new_names' defined later for renaming.
   df_filtered <- df[columns_to_keep] # Keep only specified columns

   # Extract date string from name, assuming name contains numeric date info
   date_str <- gsub("[^0-9]", "", name)

   # Define new names for the columns
   new_names <- c("Index", "CCI", "Code", "Date")

   # Sequentially apply transformations to the filtered data frame
   # Transformations applied in sequence:
   # 1. Add a date column derived from the base name.
   # 2. Rename columns according to 'new_names'.
   # 3. Convert data types, if necessary.
   # 4. Optionally, split columns for further detailed categorization.

   df_filtered <- addDateColumn(df_filtered, date_str) %>% # We add date columns to work!
    renameColumnsByPosition(new_names) %>%  # Rename the selected columns!
    convertData %>% # Change data type: [Sample NDVI Code Date -> numeric-numeric-character-date] - supresswarnings!
    splitCodeColumn # Apply splitCodeColumn to get Landuse, Position and InterRow columns!

   return(df_filtered) # Return the transformed dataframe
  } else {
   # If the data frame does not contain all required columns, halt execution.
   # This check is crucial for ensuring that subsequent transformations have a valid starting point.
   stop("Data frame does not contain all required columns: ", paste(columns_to_keep, collapse=", "))
  }
 })

 # Re-assign names to keep track of which transformed data frame corresponds to which original name
 names(transformed_data_frames) <- names(merged_data_frames) # maybe it is totally redundant, if so, skip it!

 return(transformed_data_frames)
}

#' Adds a date column to a data frame based on a date string extracted from file names.

#' @param df A data frame to which the date column will be added.
#' @param date_str A string representing a date, extracted from the names of data files.
#'        The function expects this string to be in one of two formats:
#'        - 'yymmddhh' (year, month, day, hour)
#'        - 'yymmdd' (year, month, day)
#'        The function attempts to detect the format automatically based on the string length
#'        and pattern.
#'
#' @return The input data frame with an added 'Date' column. If the date string matches
#'         one of the expected formats, this column will contain date values corresponding
#'         to the string. If the format is unrecognized, the column will be filled with NA,
#'         and a warning will be issued.
addDateColumn <- function(df, date_str) { # date_str will come from the index files!

 # Check for 'yymmddhh' format, indicating a date with year, month, day, and hour.
 if (grepl("\\d{6}\\d{2}", date_str)) { # e.g. "23081709"
  # If the string matches, extract and convert it to a date, ignoring the hour.
  df$Date <- as.Date(substr(date_str, 1, 6), format = "%y%m%d")

  # Check for 'yymmdd' format, indicating a date with year, month, and day.
 } else if (grepl("\\d{6}", date_str)) { # e.g. "230817" or plantpen_NDVI_230620
  # If the string matches, convert it directly to a date.
  df$Date <- as.Date(date_str, format = "%y%m%d")

 } else {
  # Handle other cases or provide a default value
  df$Date <- NA
  warning("Unable to assign dates to all data entries! Ensure your input file names conform to the expected formats and try to run the code again!\n")
 }

 return(df)
}

#' Renames columns of a data frame based on the provided list of new names.

#' @param df A data frame whose columns are to be renamed.
#' @param new_names A character vector of new names for the columns of the data frame.
#'                  The length of this vector must match the number of columns in the data frame.
#'
#' @return The data frame with its columns renamed if the lengths match,
#'         otherwise returns NULL as an indication of a mismatch.
renameColumnsByPosition <- function(df, new_names) {

 # Calculates the number of columns in the input data frame
 num_columns <- length(df)

 # Check if the number of columns matches the length of the new names provided.
 if (num_columns == length(new_names)) {
  # If true, rename the columns of the data frame with the new names.
  names(df)[1:length(new_names)] <- new_names
 } else {

  # If the numbers do not match, it indicates a potential error in input.
  # Return NULL to indicate the mismatch and stop the function execution.
  # This is a safeguard to prevent incorrect data manipulation.
  # [Though it is a little bit of a redundant step as the length of "num_columns"
  # will always be equal with the length of "new_names"!]

  return(NULL)
 }

 return(df)
}

#' Converts the data types of specific columns in a data frame.

#' @param df A data frame whose columns need data type conversion.
#'
#' @return A data frame with the first two columns converted to numeric, the third column to character,
#'         and the fourth column to date format. If date conversion fails, NA is inserted for those values.
#'
#' @note It's important to ensure the input data frame has at least four columns and that the data
#'       in each column is appropriate for the intended conversion. In the specific context where this function
#'       is utilized within the code, these prerequisites are ensured by the logical structure and sequence
#'       of operations leading up to this point. Therefore, while general caution is advised, the unique
#'       circumstances of this function's application mitigate the need for additional checks within the function itself.
convertData <- function(df) {

 # Convert the first two columns to numeric, handling European-style decimal separators.
 df[, c(1, 2)] <- lapply(df[, c(1, 2)], function(x) {
  as.numeric(gsub(",", ".", x))
 })

 # Convert the third column to character data type.
 df[, 3] <- as.character(df[, 3])

 # Convert the fourth column to Date format, using error handling to manage conversion failures.
 df[, 4] <- tryCatch(
  as.Date(df[, 4], format = "%Y-%m-%d"),
  error = function(e) {
   # Handle the error here, e.g., replace with NAs
  }
 )

 return(df)
}

#' Splits the "Code" column into three new columns based on specific patterns.

#' @param df A data frame with a column named "Code" that contains strings to be split.
#'
#' @return A data frame with three new columns ("Position", "Landuse", "InterRow") extracted from the "Code" column.
#'         If a pattern does not match exactly, the resulting column value will be NA.
#'
#' @details This function is designed to accurately parse and categorize components of a code string
#'          into distinct categories based on predefined patterns. The function prioritizes exact matches
#'          to ensure that only fully matching codes are extracted. In cases where a code component is
#'          expected but not present in the exact form (e.g., "GERGELY1" should not match any pattern),
#'          the function will not extract a partial match, aiming to maintain the integrity of the categorized data.
#'
#' @importFrom stringr str_extract
splitCodeColumn <- function(df) {

 df <- df %>%
  mutate(
   Position = str_extract(Code, "F|A|HP|CS|U|2|3|4"),
   Landuse = str_extract(Code, "CSU|CSR|CST|SZ|E|R|Ts1|T1|Cc1|Ts4|T2_new"),
   InterRow = ifelse(
    grepl("CSU|CST|CSR", Code) & !grepl("SK", Code),
    "S",
    str_extract(Code, "SK")
   )
   # Conditional columns!
   # e.g.: CSUFSK -> InterRow: SK | CSUF -> InterRow: S | CSUFS -> InterRow: S | CSUF{whatever} -> InterRow: S
   # <=> in the interrow column, everything is coded as "S" (=Sor) what is not wrote as "SK" (=Sorkoz) in the excel sheets code columns!
  )

 return(df)
}

#' Create a Combined NDVI DataFrame with Optional Filtering

#' @param all_data A list of data frames containing NDVI data.
#' @param apply_ndvi_filter A logical value indicating whether NDVI filtering should be applied. If TRUE, the function filters NDVI values based on the specified lower and upper limits.
#' @param lower_limit The lower limit for NDVI filtering.
#' @param upper_limit The upper limit for NDVI filtering.
#'
#' @return A data frame combining all NDVI data with optional filtering applied.
#'
#' @importFrom dplyr bind_rows filter select
create_combined_ndvi <- function(all_data, apply_ndvi_filter = FALSE, lower_limit = NULL, upper_limit = NULL) {
 combined_df <- bind_rows(all_data) %>%
  select(-index) %>% # Remove 'index' column as it may be redundant (maybe we could get rid of it earlier)
  # select(-Code) %>% # Optionally, keep 'Code' to handle or inspect codes not processed by algorithms
  select(Date, Code, Position, Landuse, InterRow, NDVI) # Select relevant columns for final output

 # Apply NDVI filtering if requested and limits are provided
 if (apply_ndvi_filter && !is.null(lower_limit) && !is.null(upper_limit)) {
  combined_df <- combined_df %>%
   filter(NDVI > lower_limit) %>%
   filter(NDVI < upper_limit) # Filter NDVI values within specified range
 }

 return(combined_df)
}

#' Create a Combined CCI DataFrame with Optional Filtering

#' @param all_data A list of data frames containing CCI data.
#' @param apply_cci_filter A logical value indicating whether CCI filtering should be applied. If TRUE, the function filters CCI values based on the specified lower and upper limits.
#' @param lower_limit The lower limit for CCI filtering.
#' @param upper_limit The upper limit for CCI filtering.
#'
#' @return A data frame combining all CCI data with optional filtering applied.
#'
#' @note If there are specific `Code` values that the algorithm might not handle properly, consider keeping the `Code` column in the analysis for troubleshooting.
#'
#' @importFrom dplyr bind_rows filter select
create_combined_cci <- function(all_data, apply_cci_filter = FALSE, lower_limit = NULL, upper_limit = NULL) {
 combined_df <- bind_rows(all_data) %>%
  select(-Index) %>% # Remove 'Index' column as it may be redundant
  # select(-Code) %>% # Optionally, keep 'Code' to handle or inspect codes not processed by algorithms
  select(Date, Code, Position, Landuse, InterRow, CCI) # Select relevant columns for final output

 # Apply CCI filtering if requested and limits are provided
 if (apply_cci_filter && !is.null(lower_limit) && !is.null(upper_limit)) {
  combined_df <- combined_df %>%
   filter(CCI > lower_limit, CCI < upper_limit) # Filter CCI values within specified range
 }

 return(combined_df)
}

#' Preprocess All Data Frames with Segmentation

#' This function preprocesses both data and index data frames by segmenting them
#' and adding unique identifiers. It is designed to handle data frames where
#' multiple segments exist within a single data frame, often demarcated by specific
#' header rows. The function differentiates between data and index files based on
#' their naming conventions.
#'
#' @param data A list containing data frames to be preprocessed.
#'
#' @return A list with two elements: `data_files_processed` and `index_files_processed`,
#'         each containing preprocessed data frames with added unique identifiers.
#'
#' @importFrom dplyr %>%
preprocess_all_data_frames <- function(data) {

 # ---- initial part of procedure ----

 # Identify data and index file names based on their naming conventions
 data_file_names <- names(data)[grepl("\\.CSV$", names(data))]
 index_file_names <- names(data)[!grepl("\\.CSV$", names(data))]

 # Function to preprocess data frames, adding unique identifiers based on segments
 preprocess_data_df_with_segments <- function(data_df) {
  # Initialize a segment counter
  segment_counter <- 1
  # Initialize an empty vector to store unique identifiers
  unique_ids <- vector("character", nrow(data_df))

  # Iterate through rows to identify segments and assign unique identifiers
  for (i in 1:nrow(data_df)) {
   if (data_df$Sample[i] == "Sample") { # Identify segment headers
    segment_counter <- segment_counter + 1 # Increment segment counter
    next # Skip this row in the final data
   }

   # Create a unique identifier for the current row
   # Here, we're assuming 'Sample' column is already processed to a consistent format (e.g., as integer)
   sample_num <- sprintf("%03d", as.integer(data_df$Sample[i]))
   unique_ids[i] <- paste0(segment_counter, "_", sample_num)
  }

  # Add unique identifiers to the data frame, excluding NA sample rows
  data_df$UniqueID <- unique_ids[data_df$Sample != "NA"]

  return(data_df)
 }

 # Similar preprocessing function for index data frames
 preprocess_index_df_with_segments <- function(data_df) {
  segment_counter <- 1
  unique_ids <- vector("character", nrow(data_df))
  for (i in 1:nrow(data_df)) {
   if (data_df$index[i] == "index") {
    segment_counter <- segment_counter + 1
    next # Skip this row in the final data
   }

   sample_num <- sprintf("%03d", as.integer(data_df$index[i])) # Ensure 'Sample' is a string with leading zeros
   unique_ids[i] <- paste0(segment_counter, "_", sample_num)
  }

  data_df$UniqueID <- unique_ids[data_df$index != "NA"]

  return(data_df)
 }


 # ---- main part of procedure ----

 # Apply preprocessing to data files, ensuring they are split into columns if necessary
 data_files_processed <- lapply(data[data_file_names], function(df) {
  if (is.data.frame(df) && nrow(df) > 0) {
   preprocess_data_df_with_segments(splitColumns(df))
  } else {
   return(df)  # Return unchanged if not a data frame or empty
  }
 })

 # Apply preprocessing to index files
 index_files_processed <- lapply(data[index_file_names], function(df) {
  if (is.data.frame(df) && nrow(df) > 0) {
   preprocess_index_df_with_segments(df)
  } else {
   return(df)  # Return unchanged if not a data frame or empty
  }
 })

 return(list(data_files_processed = data_files_processed,
             index_files_processed = index_files_processed))
}

#' Filtering out rows that might inadvertently contain column names as data
#'
#' @param df The data frame to filter.
#' @param required_columns A vector of required column names.
#'
#' @return The filtered data frame.
filter_rows_not_in_columns <- function(df, required_columns) {
 rows_to_keep <- apply(df, 1, function(row) !all(row %in% required_columns))
 filtered_df <- df[rows_to_keep, ]
 return(filtered_df)
}
