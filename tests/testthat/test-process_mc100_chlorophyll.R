#' Test for process_MC100_Chlorophyll function
#'
#' @details
#' This test checks the functionality of the `process_MC100_Chlorophyll`
#' function. It ensures that:
#' - The function successfully processes input data and generates output files.
#' - The output files contain the expected structure.
#' - The output files' values are in the desired range.
#'
#' @importFrom readxl read_excel
#' @importFrom testthat expect_true
#'
#' @return None
#' @noRd
test_that("process_MC100_Chlorophyll works correctly", {

  # Set up temporary directories
  temp_input <- file.path(tempdir(), "temp_input", "/")
  temp_output <- file.path(tempdir(), "temp_output", "/")

  if (!dir.exists(temp_input)) {
    dir.create(temp_input, recursive = TRUE)
  }
  if (!dir.exists(temp_output)) {
    dir.create(temp_output, recursive = TRUE)
  }

  # print(paste("Temporary input directory:", temp_input))
  # print(paste("Temporary output directory:", temp_output))

  # Get the source path and copy test data
  source_path <- system.file("extdata", "mc100_chlorophyll_data", package = "VegetationAnalysisToolkit")
  config_path <- system.file("extdata", "config.txt", package = "VegetationAnalysisToolkit")
  if (!dir.exists(source_path)) {
    stop("Source path does not exist. Check if the data is properly installed with the package.")
  }
  # print(paste("Source data path:", source_path))
  success <- file.copy(source_path, temp_input, recursive = TRUE)
  file.copy(config_path, temp_input)

  if (!success) {
    stop("File copy failed. Check the source and destination paths.")
  } else {
    print("File copy successful.")
  }



  # Call the function
  process_folder <- file.path(temp_input, "mc100_chlorophyll_data")
  process_MC100_Chlorophyll(
    folder_path = process_folder,
    output = temp_output,
    lower_limit = 0.0,
    upper_limit = 10.0,
    variable = "CCI",
    validate = FALSE,
    split_code = FALSE
  )

  # Check if the output file was created
  output_files <- list.files(temp_output, pattern = "\\.xlsx$", full.names = TRUE)
  expect_true(length(output_files) > 0, info = "No output files were created")

  # Validate the content of the output file
  output_file <- output_files[1]
  data <- readxl::read_excel(output_file, sheet = "Sheet1", skip = 8)
  expected_columns <- c("Date", "Code", "CCI")
  expect_true(all(expected_columns %in% colnames(data)), info = "Missing expected columns in the CCI Data sheet")

  # Ensure NDVI values are within the expected range
  cci_values <- data$CCI
  expect_true(all(cci_values >= 0.0 & cci_values <= 10.0), info = "CCI values are out of the expected range")

  # Ensure the NDVI Data sheet is not empty
  expect_true(nrow(data) > 0, info = "The CCI Data sheet is empty")

  # Check if other expected output files (like logs or summaries) are created
  other_files <- list.files(temp_output, pattern = "\\.txt$", full.names = TRUE)
  expect_true(length(other_files) > 0, info = "No log or summary files were created")
})
