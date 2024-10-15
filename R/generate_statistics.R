#' Generate Summary and Detailed Statistics for Filtered Data
#'
#' This function generates both summary and detailed statistics for a filtered dataset
#' from an Excel file, saving the results in text files. It can be applied to NDVI,
#' CCI, or other measurement variables, provided the dataset structure remains the same.
#'
#' @param output_path A character string specifying the path to the directory containing the filtered data files.
#' @param start_date Optional. A character string or Date object specifying the start date to filter the dataset (in "YYYY-MM-DD" format).
#' @param end_date Optional. A character string or Date object specifying the end date to filter the dataset (in "YYYY-MM-DD" format).
#' @param variable A character string specifying the name of the measurement variable (e.g., "NDVI", "CCI"). Default is "NDVI".
#'
#' @importFrom dplyr group_by summarise mutate filter %>%
#'
#' @return The function writes two text files containing summary and detailed statistics, and prints a message indicating their locations.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' generate_statistics(output_path = "data/", start_date = "2023-05-01", end_date = "2023-08-30", variable = "NDVI")
#' }
generate_statistics <- function(output_path, start_date = NULL, end_date = NULL, variable = "NDVI") {
 data <- load_filtered_excel(variable, output_path)
 data$Date <- as.Date(data$Date, origin = "1899-12-30")

 if (!is.null(start_date) && !is.null(end_date)) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  data <- data %>% filter(Date >= start_date & Date <= end_date)
 }

 if (nrow(data) == 0) {
  stop("No data available for the specified date range.")
 }

 basic_file_name <- paste0(output_path, variable, "_statistics_output.txt")
 detailed_file_name <- paste0(output_path, variable, "_detailed_statistics_output.txt")
 basic_file_content <- paste0(variable, " Statistics Summary")
 detailed_file_content <- paste0(variable, " Detailed Analysis")

 total_observations <- nrow(data)
 unique_codes <- length(unique(data$Code))
 date_range <- format(range(data$Date), "%Y-%m-%d")

 stats_by_group <- data %>%
  group_by(Code) %>%
  summarise(
   Mean = mean(!!sym(variable), na.rm = TRUE),
   SD = sd(!!sym(variable), na.rm = TRUE),
   Min = min(!!sym(variable), na.rm = TRUE),
   Max = max(!!sym(variable), na.rm = TRUE),
   N = n(),
   .groups = 'drop'
  ) %>%
  mutate(across(c(Mean, SD, Min, Max), \(x) round(x, 3)))
 stats_by_group_date <- data %>%
  group_by(Date, Code) %>%
  summarise(
   Mean = mean(!!sym(variable), na.rm = TRUE),
   SD = sd(!!sym(variable), na.rm = TRUE),
   Min = min(!!sym(variable), na.rm = TRUE),
   Max = max(!!sym(variable), na.rm = TRUE),
   N = n(),
   .groups = 'drop'
  ) %>%
  mutate(across(c(Mean, SD, Min, Max), \(x) round(x, 3)))

 output <- capture.output({
  cat("Data Summary:\n")
  cat("Total observations: ", total_observations, "\n")
  cat("Number of unique codes: ", unique_codes, "\n")
  cat("Date range: ", date_range[1], " to ", date_range[2], "\n\n")

  cat("Descriptive Statistics, Grouped by Code:\n")
  cat(sprintf("%-10s %-10s %-10s %-10s %-10s %-5s\n", "Code", paste0("Mean_", variable), paste0("SD_", variable), paste0("Min_", variable), paste0("Max_", variable), "N"))
  cat(sprintf("%-10s %-10s %-10s %-10s %-10s %-5s\n", "----", "---------", "-------", "-------", "-------", "-"))
  for (i in 1:nrow(stats_by_group)) {
   cat(sprintf("%-10s %-10.3f %-10.3f %-10.3f %-10.3f %-5d\n", stats_by_group$Code[i], stats_by_group$Mean[i], stats_by_group$SD[i], stats_by_group$Min[i], stats_by_group$Max[i], stats_by_group$N[i]))
  }
 })
 detailed_output <- capture.output({
  cat("Data Summary:\n")
  cat("Total observations: ", total_observations, "\n")
  cat("Number of unique codes: ", unique_codes, "\n")
  cat("Date range: ", date_range[1], " to ", date_range[2], "\n\n")

  cat("Descriptive Statistics, Grouped by Date and Code:\n")
  cat(sprintf("%-10s %-10s %-10s %-10s %-10s %-10s %-5s\n", "Date", "Code", paste0("Mean_", variable), paste0("SD_", variable), paste0("Min_", variable), paste0("Max_", variable), "N"))
  cat(sprintf("%-10s %-10s %-10s %-10s %-10s %-10s %-5s\n", "----", "----", "---------", "-------", "-------", "-------", "-"))
  for (i in 1:nrow(stats_by_group_date)) {
   cat(sprintf("%-10s %-10s %-10.3f %-10.3f %-10.3f %-10.3f %-5d\n", stats_by_group_date$Date[i], stats_by_group_date$Code[i], stats_by_group_date$Mean[i], stats_by_group_date$SD[i], stats_by_group_date$Min[i], stats_by_group_date$Max[i], stats_by_group_date$N[i]))
  }
 })

 writeLines(c(basic_file_content, output), basic_file_name)
 writeLines(c(detailed_file_content, detailed_output), detailed_file_name)
 message(paste0("The statistical analysis has been completed and the results have been saved to '",
                basic_file_name, "' and '", detailed_file_name, "'!\n"))
}
