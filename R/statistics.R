
#' @export
generate_statistics <- function(output_path, start_date = NULL, end_date = NULL) {

 # List the files in the folder
 files <- list.files(output_path, pattern = "_filtered\\.xlsx$", full.names = TRUE)

 # Filter out temporary files created by Excel (starting with "~$")
 files <- files[!grepl("^~\\$", basename(files))]

 # Check if any filtered file is found
 if (length(files) == 0) {
  stop("No filtered file found in the specified folder.")
 }

 if (length(files) > 1) {
  stop("Multiple filtered files found in the specified folder. Please ensure only one filtered file exists.")
 }


 # Read the filtered data from the Excel file
 filtered_file_path <- files[1]
 data <- read.xlsx(filtered_file_path, startRow = 8)

 # Convert Excel date serial numbers to R Date format
 data$Date <- as.Date(data$Date, origin = "1899-12-30")

 # Filter data by date range if specified
 if (!is.null(start_date) && !is.null(end_date)) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  data <- data %>% filter(Date >= start_date & Date <= end_date)
 }

 # Check if there's data after filtering by date
 if (nrow(data) == 0) {
  stop("No data available for the specified date range.")
 }

 # Header information
 # file_content <- "NDVI Statistics Summary and Detailed Analysis"
 # file_name <- c(paste0(basename(getwd()), "/", output_path, "statistics_output.txt"), paste0(basename(getwd()), "/", output_path, "detailed_statistics_output.txt"))


 # Header information
 basic_file_name <- paste0(basename(getwd()), "/", output_path, "statistics_output.txt")
 detailed_file_name <- paste0(basename(getwd()), "/", output_path, "detailed_statistics_output.txt")
 basic_file_content <- "NDVI Statistics Summary"
 detailed_file_content <- "NDVI Detailed Analysis" #todo: elnevezések javít

 # Generate headers
 project_header_basic <- generate_project_header(basic_file_name, basic_file_content)
 project_header_detailed <- generate_project_header(detailed_file_name, detailed_file_content)



 # Data Summary
 total_observations <- nrow(data)
 unique_codes <- length(unique(data$Code))
 date_range <- format(range(data$Date), "%Y-%m-%d")

 # Descriptive Statistics by Group
 stats_by_group <- data %>%
  group_by(Code) %>%
  summarise(
   Mean_NDVI = mean(NDVI, na.rm = TRUE),
   SD_NDVI = sd(NDVI, na.rm = TRUE),
   Min_NDVI = min(NDVI, na.rm = TRUE),
   Max_NDVI = max(NDVI, na.rm = TRUE),
   N = n(),
   .groups = 'drop'  # Ensure no warning about grouped output
  ) %>%
  mutate(across(c(Mean_NDVI, SD_NDVI, Min_NDVI, Max_NDVI), round, 3))

 # Descriptive Statistics by Group and Date
 stats_by_group_date <- data %>%
  group_by(Date, Code) %>%
  summarise(
   Mean_NDVI = mean(NDVI, na.rm = TRUE),
   SD_NDVI = sd(NDVI, na.rm = TRUE),
   Min_NDVI = min(NDVI, na.rm = TRUE),
   Max_NDVI = max(NDVI, na.rm = TRUE),
   N = n(),
   .groups = 'drop'  # Ensure no warning about grouped output
  ) %>%
  mutate(across(c(Mean_NDVI, SD_NDVI, Min_NDVI, Max_NDVI), round, 3))

 # Overall Statistics
 # overall_stats <- data %>%
 #  summarise(
 #   Mean_NDVI = round(mean(NDVI, na.rm = TRUE), 3),
 #   SD_NDVI = round(sd(NDVI, na.rm = TRUE), 3),
 #   Min_NDVI = round(min(NDVI, na.rm = TRUE), 3),
 #   Max_NDVI = round(max(NDVI, na.rm = TRUE), 3),
 #   N = n()
 #  )

 # Generate output for general statistics
 output <- capture.output({ # todo: feedback-be is ugyan ez legyne a header / saved excelbe is  / amikor beolvasom azokat a sorokat kihagyja

  # cat("* Project:          ", project_name, "\n")
  # cat("* File content:     ", file_content, "\n")
  # cat("* File name:        ", file_name[1], "\n")
  # cat("* Script name:      ", script_name, "\n")
  # cat("* Software version: ", software_version, "\n")
  # cat("* Generated at:     ", generated_at, "\n\n")

  cat("Data Summary:\n")
  cat("Total observations: ", total_observations, "\n")
  cat("Number of unique codes: ", unique_codes, "\n")
  cat("Date range: ", date_range[1], " to ", date_range[2], "\n\n")

  cat("Descriptive Statistics, Grouped by Code:\n")
  cat(sprintf("%-10s %-10s %-10s %-10s %-10s %-5s\n", "Code", "Mean_NDVI", "SD_NDVI", "Min_NDVI", "Max_NDVI", "N"))
  cat(sprintf("%-10s %-10s %-10s %-10s %-10s %-5s\n", "----", "---------", "-------", "-------", "-------", "-"))
  for (i in 1:nrow(stats_by_group)) {
   cat(sprintf("%-10s %-10.3f %-10.3f %-10.3f %-10.3f %-5d\n", stats_by_group$Code[i], stats_by_group$Mean_NDVI[i], stats_by_group$SD_NDVI[i], stats_by_group$Min_NDVI[i], stats_by_group$Max_NDVI[i], stats_by_group$N[i]))
  }

  # cat("\nOverall Statistics:\n")
  # cat(sprintf("%-10s %-10s %-10s %-10s %-5s\n", "Mean_NDVI", "SD_NDVI", "Min_NDVI", "Max_NDVI", "N"))
  # cat(sprintf("%-10.3f %-10.3f %-10.3f %-10.3f %-5d\n", overall_stats$Mean_NDVI, overall_stats$SD_NDVI, overall_stats$Min_NDVI, overall_stats$Max_NDVI, overall_stats$N))
 })



 # Generate output for detailed statistics
 detailed_output <- capture.output({

  # cat("* Project:          ", project_name, "\n")
  # cat("* File content:     ", file_content, "\n")
  # cat("* File name:        ", file_name[2], "\n")
  # cat("* Script name:      ", script_name, "\n")
  # cat("* Software version: ", software_version, "\n")
  # cat("* Generated at:     ", generated_at, "\n\n")

  cat("Data Summary:\n")
  cat("Total observations: ", total_observations, "\n")
  cat("Number of unique codes: ", unique_codes, "\n")
  cat("Date range: ", date_range[1], " to ", date_range[2], "\n\n")

  cat("Descriptive Statistics, Grouped by Date and Code:\n")
  cat(sprintf("%-10s %-10s %-10s %-10s %-10s %-10s %-10s\n", "Date", "Code", "Mean_NDVI", "SD_NDVI", "Min_NDVI", "Max_NDVI", "N"))
  cat(sprintf("%-10s %-10s %-10s %-10s %-10s %-10s %-10s\n", "----", "----", "---------", "-------", "-------", "-------", "-"))
  for (i in 1:nrow(stats_by_group_date)) {
   cat(sprintf("%-10s %-10s %-10.3f %-10.3f %-10.3f %-10.3f %-5d\n", stats_by_group_date$Date[i], stats_by_group_date$Code[i], stats_by_group_date$Mean_NDVI[i], stats_by_group_date$SD_NDVI[i], stats_by_group_date$Min_NDVI[i], stats_by_group_date$Max_NDVI[i], stats_by_group_date$N[i]))
  }
 })



 # Write general statistics to file
 writeLines(c(project_header_basic, output), paste0(output_path, "/statistics_output.txt"))

 # Write detailed statistics to file
 writeLines(c(project_header_detailed, detailed_output), paste0(output_path, "/detailed_statistics_output.txt"))


 # message(paste0("The statistical analysis has been completed and the results have been saved to '", file_name[1], "' and '", file_name[2], "'!\n"))
 message(paste0("The statistical analysis has been completed and the results have been saved to '", basic_file_name, "' and '", detailed_file_name, "'!\n"))

}
