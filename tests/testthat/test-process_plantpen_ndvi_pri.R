#' Test for process_PlantPen_NDVI_PRI function
#'
#' @details
#' This test checks the functionality of the `process_PlantPen_NDVI_PRI`
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
test_that("process_PlantPen_NDVI_PRI works correctly", {

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

  # Get the source path and copy data from there
  source_path <- system.file("extdata", "plantpen_ndvi_pri_data", package = "VegetationAnalysisToolkit")
  config_path <- system.file("extdata", "config.txt", package = "VegetationAnalysisToolkit")
  print(paste("Source data path:", source_path))
  if (!dir.exists(source_path)) {
    stop("Source path does not exist. Check if the data is properly installed with the package.")
  }

  success <- file.copy(source_path, temp_input, recursive = TRUE)
  file.copy(config_path, temp_input)

  if (!success) {
    stop("File copy failed. Check the source and destination paths.")
  } else {
    print("File copy successful.")
  }



  # Call the function
  process_folder <- file.path(temp_input, "plantpen_ndvi_pri_data")
  process_PlantPen_NDVI_PRI(
    folder_path = process_folder,
    output = temp_output,
    lower_limit = 0.1,
    upper_limit = 0.9,
    variable = "NDVI",
    validate = FALSE,
    split_code = FALSE
  )

  # Check for generated output file
  output_files <- list.files(temp_output, pattern = "\\.xlsx$", full.names = TRUE)
  expect_true(length(output_files) > 0, info = "No output files were created")

  # Load and validate output data structure
  output_file <- output_files[1]
  data <- readxl::read_excel(output_file, sheet = "Sheet1", skip = 8)
  expected_columns <- c("Date", "Code", "NDVI") # Example column names
  expect_true(all(expected_columns %in% colnames(data)), info = "Missing expected columns in the NDVI Data sheet")

  # Ensure NDVI values are within the expected range
  ndvi_values <- data$NDVI
  expect_true(all(ndvi_values >= 0.1 & ndvi_values <= 0.9), info = "NDVI values are out of the expected range")

  # Ensure the NDVI Data sheet is not empty
  expect_true(nrow(data) > 0, info = "The NDVI Data sheet is empty")

  # Check if other expected output files (like logs or summaries) are created
  other_files <- list.files(temp_output, pattern = "\\.txt$", full.names = TRUE)
  expect_true(length(other_files) > 0, info = "No log or summary files were created")
})
