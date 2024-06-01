#'

clean_column_names <- function(col_names) {
 sapply(col_names, function(name) {
  name <-
   gsub("[^[:alnum:]]", "", name)  # Remove non-alphanumeric characters
  name <- gsub("nm", "", name)            # Remove the "nm" suffix
  name
 })
}

check_sequential_index <- function(df, verbose=FALSE) {
 # Helper function to check if a sequence is sequential

 # Identifying segments with "Sample" headers for separate sequentiality checks
 split_indices <- which(df$Sample == "Sample")
 # Append an additional index to include the last data segment
 split_indices <- c(split_indices, nrow(df) + 1)

 index0 <- 1
 for (i in seq_along(split_indices)) {
  index <- split_indices[i]
  # Adjust the segment range to handle the last chunk correctly
  if (i == length(split_indices)) {
   seq_df <- df[index0:(nrow(df)),]
  } else {
   seq_df <- df[index0:(index - 1),]
  }
  index0 <- index + 1 # Prepare for the next segment

  # Exclude "Sample" header rows and convert to numeric for sequentiality check
  numeric_samples <- suppressWarnings(as.numeric(seq_df$Sample[seq_df$Sample != "Sample"]))

  # Verify sequentiality within the segment
  if (any(diff(numeric_samples) != 1)) {
   if(verbose){ print(sprintf("Non-sequential values found in 'Sample' column within rows %d to %d.", index0, index - 1))}
   return(FALSE)
  }
  if(verbose){ print(sprintf("Checked sequentiality up to row %d: OK", index - 1))}
 }

 return(TRUE)  # Return TRUE if all segments pass the sequentiality check
}

#Filtering out rows that might inadvertently contain column names as data
filter_rows_not_in_columns <- function(df, required_columns) {
 rows_to_keep <- apply(df, 1, function(row) !all(row %in% required_columns))
 filtered_df <- df[rows_to_keep, ]
 return(filtered_df)
}

process_dataframe <- function(df) {

 # Check if the number of columns is 1 (= Chlorofile data) and split the columns if true
 if (ncol(df) == 1) {
  df <- splitColumns(df)
 }

 # Save an old copy for sequential index check
 df_old <- df

 # Filter out rows that match column names
 df <- filter_rows_not_in_columns(df, names(df))

 # Return the processed dataframe and its old copy
 return(list(current_df = df, current_df_old = df_old))
}

#' Helper function to add identified issues to the list
#'
#' @param issues A list containing named vectors of issue messages.
#' @param name The category name under which the issue message should be added.
#' @param issue_message The message describing the issue.
#'
#' @return The updated list of issues with the new issue message appended.
add_issue <- function(issues, name, issue_message) {
  issues[[name]] <- c(issues[[name]], issue_message)
  return(issues)
}

#' Logging Function for Validation Issues
#'
#' @param msg Character string containing the message to be logged.
#' @param log_file Character string specifying the file to which the log message should be written. Defaults to "val_issues.txt".
#'
#' @details This function logs validation-related messages to a specified log file.
#'          Each log entry is prepended with a timestamp indicating when the log entry was made.
#'          The log messages are appended to the existing content of the log file.
#'
#' @return None. This function is used for its side effect of logging messages to a file.
#'
#' @examples
#' log_validation_issue("This is a validation issue.")
log_validation_issue <- function(msg, log_file = "val_issues.txt") {
 timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
 con <- file(log_file, open = "a")
 if (is.character(msg)) {
  message <- paste(timestamp, "-", msg)
  writeLines(message, con = con)
 } else if (is.list(msg)) {
  lapply(unlist(msg), function(m) {
   message <- paste(timestamp, "-", gsub("\n", "\\n", m))
   writeLines(message, con = con)
  })
 }
 close(con)
}

#' Logging Function for General Errors and Warnings
#'
#' @param msg Character string containing the message to be logged.
#' @param log_file Character string specifying the file to which the log message should be written. Defaults to "error_log.txt".
#'
#' @details This function logs general error and warning messages to a specified log file.
#'          Each log entry is prepended with a timestamp indicating when the log entry was made.
#'          The log messages are appended to the existing content of the log file.
#'
#' @return None. This function is used for its side effect of logging messages to a file.
#'
#' @examples
#' log_general_issue("This is a general warning or error message.")
log_general_issue <- function(msg, log_file = "error_log.txt") {
 timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
 cat(paste(timestamp, "-", msg, "\n"), file = log_file, append = TRUE)
}

#' Handler for General Warnings and Errors
#'
#' @param cond A condition object representing the warning or error to be handled.
#' @param type Character string specifying the type of condition: either "error" or "warning". Defaults to "error".
#'
#' @details This function handles general warnings and errors by logging the message using `log_general_issue`.
#'          If the condition is an error, it stops execution by calling `stop` with the message.
#'          If the condition is a warning, it re-issues the warning using `warning`.
#'
#' @return None. This function is used for its side effect of handling and logging conditions.
#'
#' @examples
#' tryCatch(
#'   { stop("An example error") },
#'   error = function(e) { handle_general_condition(e, "error") }
#' )
#'
#' tryCatch(
#'   { warning("An example warning") },
#'   warning = function(w) { handle_general_condition(w, "warning") }
#' )
handle_general_condition <- function(cond, type = "error") {
 msg <- paste(type, ":", conditionMessage(cond))
 log_general_issue(msg)
 if (type == "error") {
  stop(msg)
 } else {
  warning(msg)
 }
}

#' Calculate and Print Feedback for Data Filtering

#' @param all_data A data frame containing the original, unfiltered dataset.
#' @param filtered_data A data frame containing the dataset after filtering has been applied.
#' @param varname A character string representing the variable name used for generating output filenames
#'        for feedback reports (e.g., "ndvi" or "cci").
#'
#' @details The function first calculates the total number of observations and the number of filtered
#'          observations for each 'Code' in the dataset. It then computes the number of observations removed
#'          and the percentage of observations removed for each 'Code'. This summary is saved to a text file
#'          named after the variable name with "_feedback.txt" suffix.
#'
#'          Additionally, the function provides a more detailed feedback report grouped by both 'Date' and 'Code',
#'          calculating similar statistics. This detailed report is saved to a text file with "_detailed_feedback.txt"
#'          suffix, allowing for a more granular analysis of the filtering process.
#'
#'          The function is designed to assist in evaluating the impact of filtering criteria on the dataset,
#'          offering insights into how data reduction varies across different 'Code' categories and specific dates.
#'
#' @return The function does not return a value but generates two text files:
#'         - One summarizing the filtering feedback by 'Code'.
#'         - Another providing detailed filtering feedback by both 'Date' and 'Code'.
#'         The user is notified via the console about the generation of these files.
#'
#' @note The generated feedback files are saved in the current working directory. Users should ensure
#'       appropriate permissions are available for file creation and that the working directory is
#'       set to a desired path before running the function.
#'
#' @importFrom dplyr group_by summarise mutate left_join
#' @importFrom utils capture.output
calculate_and_print_feedback <- function(all_data, filtered_data, varname) {

  # Summary feedback by Code
  feedback <- all_data %>%
    group_by(Code) %>%
    summarise(Total = n(), .groups = 'drop') %>%
    left_join(filtered_data %>% group_by(Code) %>% summarise(Filtered = n(), .groups = 'drop'),
              by = "Code") %>%
    mutate(Filtered = ifelse(is.na(Filtered), 0, Filtered),
           Removed = Total - Filtered,
           PercentageRemoved = (Removed / Total) * 100)

  # Notification about the availability of feedback in text files
  cat("For feedback on data filtering, please check the 'feedback.txt' for a summary by Code, and 'detailed_feedback.txt' for comprehensive details grouped by Date and Code!\n")

  # Save the feedback content to a text file
  feedback_content <- capture.output({
    cat("Feedback on Data Filtering, Grouped by Code:\n")
    print(feedback, n = Inf)
  })
  writeLines(feedback_content, paste0(varname,"_feedback.txt"))

  # Detailed feedback by Date and Code
  feedback <- all_data %>%
    group_by(Date, Code) %>%
    summarise(Total = n(), .groups = 'drop') %>%
    left_join(filtered_data %>% group_by(Date, Code) %>% summarise(Filtered = n(), .groups = 'drop'),
              by = c("Date", "Code")) %>%
    mutate(Filtered = ifelse(is.na(Filtered), 0, Filtered),
           Removed = Total - Filtered,
           PercentageRemoved = (Removed / Total) * 100)

  # Save detailed feedback to text file
  feedback_content <- capture.output({
    cat("Detailed Feedback on Data Filtering, Grouped by Date and Code:\n")
    print(feedback, n = Inf)
  })
  writeLines(feedback_content, paste0(varname,"_detailed_feedback.txt"))

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
#' @details The function first combines all the NDVI data frames into a single
#'          data frame. It then creates a second data frame where it applies
#'          filters based on the specified NDVI value limits. This allows for
#'          the exclusion of data points that fall outside the desired NDVI range.
#'          Both the combined (raw) and filtered data frames are then saved to
#'          separate Excel files in the specified output directory. The function
#'          also calculates and prints feedback on the amount of data filtered
#'          and saves the filtered data frame as a global variable for further use.
#'
#' @return The function does not explicitly return a value but saves two Excel
#'         files to the specified output directory: one containing the raw
#'         combined NDVI data and another containing the filtered NDVI data.
#'         It also prints to the console the location of the saved files and
#'         information about the filtered data.
#'
#' @note The function uses the <<- operator to assign the filtered data frame to
#'       a global variable named `filtered`. This is intended for convenience
#'       in further analyses but should be used with caution, especially in
#'       environments where global variable usage might lead to unintended side
#'       effects.
#'
#' @importFrom openxlsx write.xlsx
save_ndvi <- function(df, output, filename, lower_limit, upper_limit) {

  # Combine all provided NDVI data frames into one and create a filtered version!
  combined_df_all <- create_combined_ndvi(df)
  combined_df_filtered <- create_combined_ndvi(df, apply_ndvi_filter = TRUE, lower_limit = lower_limit, upper_limit = upper_limit) # Specify the range for acceptable NDVI values

  # Save the filtered data frame as a global variable for ease of access
  # filtered <<- combined_df_filtered

  # Provide feedback on the amount of data filtered
  calculate_and_print_feedback(combined_df_all, combined_df_filtered, "ndvi")

  # Define the filename for the raw data Excel file and save it
  output0 <- paste0(output, filename)
  write.xlsx(combined_df_all, output0, rowNames = FALSE)

  # Modify the filename for the filtered data Excel file and save it
  output0 <- paste0(output, sub(".xlsx$", "_filtered.xlsx", filename))
  write.xlsx(combined_df_filtered, output0, rowNames = FALSE)

  # Print confirmation messages with details about the saved files and filtering
  cat("Data (filtered and raw) saved to", output, "\n")
  cat("In filtered data values outside of bounds:", lower_limit, "and", upper_limit, "[NDVI] were discarded!\n")

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
#' @details The function first combines all the CCI data frames into a single data
#'          frame. It then applies filtering based on the specified lower and upper
#'          CCI value limits, creating a second data frame with the filtered data.
#'          Both the raw (combined) and filtered data frames are saved to separate
#'          Excel files in the specified output directory. The function also calculates
#'          and provides feedback on the volume of data filtered. Additionally, it
#'          saves the filtered data frame as a global variable for easy access in
#'          subsequent analyses.
#'
#' @return Although the function does not return a value explicitly, it saves two
#'         Excel files to the specified directory: one with the raw combined CCI data
#'         and another with the filtered CCI data. It also outputs to the console the
#'         paths to the saved files and details about the filtering process.
#'
#' @note Similar to `save_ndvi`, this function uses the <<- operator to assign the
#'       filtered data frame to a global variable named `filtered`. This practice
#'       facilitates further analysis but should be approached with caution to avoid
#'       potential side effects in environments where global variables might lead to
#'       conflicts or unexpected behavior.
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

#' Create a Combined NDVI DataFrame with Optional Filtering

#' @param all_data A list of data frames containing NDVI data.
#' @param apply_ndvi_filter A logical value indicating whether NDVI filtering should be applied. If TRUE, the function filters NDVI values based on the specified lower and upper limits.
#' @param lower_limit The lower limit for NDVI filtering. Only NDVI values greater than this limit will be included in the final data frame if filtering is applied.
#' @param upper_limit The upper limit for NDVI filtering. Only NDVI values less than this limit will be included in the final data frame if filtering is applied.
#'
#' @return A data frame combining all NDVI data with optional filtering applied.
#'
#' @details The function first combines all NDVI data frames from the input list into a single data frame.
#'          It then selects specific columns to retain in the final combined data frame. The `index` column is removed as it might be redundant.
#'          The function allows for optional NDVI value filtering. If `apply_ndvi_filter` is set to TRUE and both `lower_limit` and `upper_limit` are provided,
#'          the function filters the combined data frame to include only the rows where NDVI values fall within the specified range.
#'          This is useful for focusing on NDVI values of interest by excluding outliers or values outside a specific range.
#'
#' @note Assuming `data_list` is a list of NDVI data frames
#'
#' @importFrom dplyr bind_rows filter select
create_combined_ndvi <- function(all_data, apply_ndvi_filter = FALSE, lower_limit = NULL, upper_limit = NULL) {

  combined_df <- bind_rows(all_data) %>%
    select(-index) %>% # Remove 'index' column as it may be redundant (maybe we could get rid of it earlier)
    #select(-Code) %>% # Optionally, keep 'Code' to handle or inspect codes not processed by algorithms
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
#' @param lower_limit The lower limit for CCI filtering. Only CCI values greater than this limit will be included in the final data frame if filtering is applied.
#' @param upper_limit The upper limit for CCI filtering. Only CCI values less than this limit will be included in the final data frame if filtering is applied.
#'
#' @return A data frame combining all CCI data with optional filtering applied.
#'
#' @details The function first combines all CCI data frames from the input list into a single data frame.
#'          It then selects specific columns to retain in the final combined data frame. The `Sample` column is removed as it might be redundant.
#'          The function allows for optional CCI value filtering. If `apply_cci_filter` is set to TRUE and both `lower_limit` and `upper_limit` are provided,
#'          the function filters the combined data frame to include only the rows where CCI values fall within the specified range.
#'          This is beneficial for focusing on CCI values of interest by excluding outliers or values outside a specific range.
#'
#' @note It's important to ensure the input list `all_data` contains data frames structured to include a `CCI` column.
#'        If there are specific `Code` values that the algorithm might not handle properly, consider keeping the `Code` column in the analysis for troubleshooting.
#'
#' @importFrom dplyr bind_rows filter select
create_combined_cci <- function(all_data, apply_cci_filter = FALSE, lower_limit = NULL, upper_limit = NULL) {

  combined_df <- bind_rows(all_data) %>%
    select(-Index) %>% # Remove 'Index' column as it may be redundant
    #select(-Code) %>% # Optionally, keep 'Code' to handle or inspect codes not processed by algorithms
    select(Date, Code, Position, Landuse, InterRow, CCI) # Select relevant columns for final output

  # Apply CCI filtering if requested and limits are provided
  if (apply_cci_filter && !is.null(lower_limit) && !is.null(upper_limit)) {
    combined_df <- combined_df %>%
      filter(CCI > lower_limit, CCI < upper_limit) # Filter CCI values within specified range
  }

  return(combined_df)
}
