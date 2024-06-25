#' Functions related to logging and handling errors and warnings, as well as giving feedback to the user.


safely_read_NDVI <- function(folder_path, env) {
 tryCatch({
  read_NDVI(folder_path)
 }, error = function(e) {
  handle_general_condition(e, "error", env)
  # summary_msg <- summarize_and_log_issues(env)
  # message(summary_msg)
  return(NULL)
 }, warning = function(w) {
  handle_general_condition(w, "warning", env)
  # invokeRestart("muffleWarning")
 })
}

safely_filter_data <- function(all_data, env) {
 tryCatch({
  filter_data(all_data)
 }, error = function(e) {
  handle_general_condition(e, "error", env)
  # summary_msg <- summarize_and_log_issues(env)
  # message(summary_msg)
  # stop()
  return(NULL)
 }, warning = function(w) {
  handle_general_condition(w, "warning", env)
  # invokeRestart("muffleWarning")
 })
}

safely_map_NDVI <- function(all_data, env) {
 last_df <- all_data[[length(all_data)]] # better method to find the one with data?

 tryCatch({
  map_all_NDVI(all_data, last_df)
 }, error = function(e) {
  handle_general_condition(e, "error", env)
  return(NULL)
 }, warning = function(w) {
  handle_general_condition(w, "warning", env)
  invokeRestart("muffleWarning")
 })
}

safely_save_ndvi <- function(all_data, output, filename, lower_limit, upper_limit, env) {
 tryCatch({
  save_ndvi(all_data, output, filename, lower_limit, upper_limit)
 }, error = function(e) {
  handle_general_condition(e, "error", env)
  # summary_msg <- summarize_and_log_issues(env)
  # message(summary_msg)
  return(NULL)
 }, warning = function(w) {
  handle_general_condition(w, "warning", env)
  # invokeRestart("muffleWarning")
 })
}

#' Handler for General Warnings and Errors
#'
#' @param cond A condition object representing the warning or error to be handled.
#' @param type Character string specifying the type of condition: either "error" or "warning". Defaults to "error".
#'
#' @details This function handles general warnings and errors by logging the message using `log_general_issue`.
#'          If the condition is an error, it stops execution by calling `stop` with the message.
#'          If the condition is a warning, it re-issues the warning using `warning`.
handle_general_condition <- function(cond, type = "error", env) {
 msg <- paste(type, ":", conditionMessage(cond))
 log_general_issue(msg)
 env$all_issues <- append(env$all_issues, list(msg))

 if (type == "warning") {
  invokeRestart("muffleWarning")
 }
}

#' Logging Function for Validation Issues
#'
#' @param msg Character string containing the message to be logged.
#' @param log_file Character string specifying the file to which the log message should be written. Defaults to "val_issues.txt".
#'
#' @details This function logs validation-related messages to a specified log file.
#'          Each log entry is prepended with a timestamp indicating when the log entry was made.
#'          The log messages are appended to the existing content of the log file.
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

# Log validation issues
log_validation_issues <- function(validation_warnings) {
 summary_msg <- paste("Input data validation completed with", length(validation_warnings), "warnings.\nCheck 'val_issues.txt' for details.")
 log_validation_issue(validation_warnings)
 warning(summary_msg)
}

#' Logging Function for General Errors and Warnings
#'
#' @param msg Character string containing the message to be logged.
#' @param log_file Character string specifying the file to which the log message should be written. Defaults to "error_log.txt".
#'
#' @details This function logs general error and warning messages to a specified log file.
#'          Each log entry is prepended with a timestamp indicating when the log entry was made.
#'          The log messages are appended to the existing content of the log file.
log_general_issue <- function(msg, log_file = "error_log.txt") {
 timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
 con <- file(log_file, open = "a")
 if (is.character(msg)) {
  message <- paste(timestamp, "-", msg)
  writeLines(message, con = con)
 } else if (is.list(msg)) { # redundant?
  lapply(unlist(msg), function(m) {
   message <- paste(timestamp, "-", gsub("\n", "\\n", m)) # gsub - reudundant?
   writeLines(message, con = con)
  })
 }
 close(con)
}

# Perform validation on the data
perform_validation <- function(df, data_type, data_frame_files, validation_warnings_env) {
 validation_result <- withCallingHandlers({
  check_data(df, data_type, data_frame_files)
 }, warning = function(w) {
  handle_general_condition(w, "warning", validation_warnings_env)
 }, error = function(e) {
  handle_general_condition(e, "error", validation_warnings_env)
 })

 if (length(validation_result$validation_warnings) > 0) {
  log_validation_issues(validation_result$validation_warnings)
 } else {
  message("Validation completed without warnings.")
 }

 return(validation_result$is_valid)
}

# Setup environment for general warnings
setup_general_warnings_env <- function() {
 env <- new.env()
 env$all_warnings <- list()
 return(env)
}

summarize_and_log_issues <- function(env) {
 if (length(env$all_issues) > 0) {
  summary_msg <- paste0("Processing completed with ", length(env$all_issues), " issues (errors/warnings).\nCheck 'error_log.txt' for details.")
  # log_general_issue(paste0("Detailed issues:\n", paste(env$all_issues, collapse = "\n")))
  message(summary_msg)
 } else {
  message("Processing completed without any issues.")
  # return("Processing completed without any issues.")
 }
}

#' Log issues
#'
#' @param file The file with an issue.
#' @param issue The issue description.
log_issue <- function(file, issue) {
 message <- paste("Issue with file:", file, "-", issue)
 handle_general_condition(simpleError(message), "warning")
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
  summarise(Total = n(), .groups = "drop") %>%
  left_join(filtered_data %>% group_by(Code) %>% summarise(Filtered = n(), .groups = "drop"),
            by = "Code"
  ) %>%
  mutate(
   Filtered = ifelse(is.na(Filtered), 0, Filtered),
   Removed = Total - Filtered,
   PercentageRemoved = (Removed / Total) * 100
  )

 # Notification about the availability of feedback in text files
 cat("For feedback on data filtering, please check the 'feedback.txt' for a summary by Code, and 'detailed_feedback.txt' for comprehensive details grouped by Date and Code!\n")

 # Save the feedback content to a text file
 feedback_content <- capture.output({
  cat("Feedback on Data Filtering, Grouped by Code:\n")
  print(feedback, n = Inf)
 })
 writeLines(feedback_content, paste0(varname, "_feedback.txt"))

 # Detailed feedback by Date and Code
 feedback <- all_data %>%
  group_by(Date, Code) %>%
  summarise(Total = n(), .groups = "drop") %>%
  left_join(filtered_data %>% group_by(Date, Code) %>% summarise(Filtered = n(), .groups = "drop"),
            by = c("Date", "Code")
  ) %>%
  mutate(
   Filtered = ifelse(is.na(Filtered), 0, Filtered),
   Removed = Total - Filtered,
   PercentageRemoved = (Removed / Total) * 100
  )

 # Save detailed feedback to text file
 feedback_content <- capture.output({
  cat("Detailed Feedback on Data Filtering, Grouped by Date and Code:\n")
  print(feedback, n = Inf)
 })
 writeLines(feedback_content, paste0(varname, "_detailed_feedback.txt"))
}
