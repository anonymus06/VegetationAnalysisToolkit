#' Test for generate_statistics function
#'
#' @details
#' This test checks the functionality of the `generate_statistics`
#' function. It ensures that:
#' - The function successfully generates output files.
#'
#' @importFrom testthat expect_true
#'
#' @return None
#' @noRd
test_that("generate_statistics works correctly", {

  # Set up temporary directories
  temp_output <- file.path(tempdir(), "temp_output", "/")
  if (!dir.exists(temp_output)) {
    dir.create(temp_output, recursive = TRUE)
  }
  print(paste("Temporary output directory:", temp_output))

  # Locate and copy example data to the temp folder
  source_path <- system.file("testdata", package = "VegetationAnalysisToolkit")
  if (!dir.exists(source_path)) {
    stop("Source path does not exist. Check if the data is properly installed with the package.")
  }
  print(paste("Source data path:", source_path))
  success <- file.copy(source_path, temp_output, recursive = TRUE)

  if (!success) {
    stop("File copy failed. Check the source and destination paths.")
  } else {
    print("File copy successful.")
  }

  print(list.files(temp_output, recursive = TRUE))


  # Call the function
  process_folder <- file.path(temp_output, "testdata\\")
  generate_statistics(process_folder, variable = "CCI")

  # Check that output files were generated
  output_files <- list.files(process_folder, pattern = "\\.txt$", full.names = TRUE)
  expect_true(length(output_files) > 0, info = "No output files were created")
})
