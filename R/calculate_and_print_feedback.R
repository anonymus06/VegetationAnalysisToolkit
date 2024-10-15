#' Calculate and Print Feedback for Data Filtering
#'
#' @param all_data A data frame containing the original, unfiltered dataset.
#' @param filtered_data A data frame containing the dataset after filtering has been applied.
#' @param output The directory path where the feedback files will be saved.
#' @param variable A string representing the variable being processed (e.g., NDVI, PRI, or CCI).
#' @param env An environment object for logging messages and issues.
#'
#' @return This function does not return a value, but it generates two text files: a summary and a detailed
#'         report on the filtering process. The user is notified via console messages about the generation of
#'         these files.
#'
#' @importFrom dplyr group_by summarise mutate left_join
#' @importFrom utils capture.output
#'
#' @noRd
calculate_and_print_feedback <- function(all_data, filtered_data, output, variable, env) {

 summary_file_name <- paste0(output, variable, "_feedback.txt")
 detailed_file_name <- paste0(output, variable, "_detailed_feedback.txt")
 summary_file_content <- "Overview of data filtering results"
 detailed_file_content <- "Detailed data filtering results"

 project_header_summary <- generate_project_header(summary_file_name, summary_file_content, "calculate_and_print_feedback.R")
 project_header_detailed <- generate_project_header(detailed_file_name, detailed_file_content, "calculate_and_print_feedback.R")

 feedback <- all_data %>%
  group_by(Code) %>%
  summarise(Total = n(), .groups = "drop") %>%
  left_join(filtered_data %>% group_by(Code) %>% summarise(Filtered = n(), .groups = "drop"),
            by = "Code"
  ) %>%
  mutate(
   Filtered = ifelse(is.na(Filtered), 0, Filtered),
   Removed = Total - Filtered,
   PercentageRemoved = round((Removed / Total) * 100, 2)
  )

 summary_feedback <- capture.output({
  cat("Summary of Data Filtering, Grouped by Code:\n")
  cat(sprintf("%-10s %-7s %-9s %-7s %-20s\n", "Code", "Total", "Filtered", "Removed", "Percentage Removed (%)"))
  cat(sprintf("%-10s %-7s %-9s %-7s %-20s\n", "----", "-----", "--------", "-------", "---------------------"))
  for (i in 1:nrow(feedback)) {
   cat(sprintf("%-10s %-7d %-9d %-7d %-20.2f\n", feedback$Code[i], feedback$Total[i], feedback$Filtered[i], feedback$Removed[i], feedback$PercentageRemoved[i]))
  }
 })

 detailed_feedback <- all_data %>%
  group_by(Date, Code) %>%
  summarise(Total = n(), .groups = "drop") %>%
  left_join(filtered_data %>% group_by(Date, Code) %>% summarise(Filtered = n(), .groups = "drop"),
            by = c("Date", "Code")
  ) %>%
  mutate(
   Filtered = ifelse(is.na(Filtered), 0, Filtered),
   Removed = Total - Filtered,
   PercentageRemoved = round((Removed / Total) * 100, 2)
  )

 detailed_feedback_content <- capture.output({
  cat("Detailed Data Filtering Feedback, Grouped by Date and Code:\n")
  cat(sprintf("%-10s %-10s %-7s %-9s %-7s %-20s\n", "Date", "Code", "Total", "Filtered", "Removed", "Percentage Removed (%)"))
  cat(sprintf("%-10s %-10s %-7s %-9s %-7s %-20s\n", "----", "----", "-----", "--------", "-------", "---------------------"))
  for (i in 1:nrow(detailed_feedback)) {
   cat(sprintf("%-10s %-10s %-7d %-9d %-7d %-20.2f\n", detailed_feedback$Date[i], detailed_feedback$Code[i], detailed_feedback$Total[i], detailed_feedback$Filtered[i], detailed_feedback$Removed[i], detailed_feedback$PercentageRemoved[i]))
  }
 })

 writeLines(c(project_header_summary, summary_feedback), summary_file_name)
 writeLines(c(project_header_detailed, detailed_feedback_content), detailed_file_name)
 add_message(env, paste0("Data filtering results are available in '", variable, "_feedback.txt' (summary) and '", variable, "_detailed_feedback.txt' (detailed)."), "info")
}
