#' Test for plot_measurement_data function
#'
#' @details
#' This test checks the functionality of the `plot_measurement_data`
#' function. It ensures that:
#' - The function successfully processes input data and generates a plot.
#' - The returned object is a plotly object.
#'
#' @importFrom testthat expect_true
#'
#' @return None
#' @noRd
test_that("plot_measurement_data works correctly", {

  # Set up temporary directories for testing
  temp_output <- file.path(tempdir(), "temp_output", "/")
  if (!dir.exists(temp_output)) {
   dir.create(temp_output, recursive = TRUE)
  }
  print(paste("Temporary output directory:", temp_output))

  # Use example data from the package's testdata folder
  source_path <- system.file("testdata", package = "VegetationAnalysisToolkit")
  print(paste("Source data path:", source_path))

  # if (!dir.exists(source_path)) {
  #  stop("Source path does not exist. Check if the data is properly installed with the package.")
  # }

  # Copy test data into the temporary output folder
  success <- file.copy(source_path, temp_output, recursive = TRUE)
  print(list.files(temp_output, recursive = TRUE))

  if (!success) {
    stop("File copy failed. Check the source and destination paths.")
  } else {
    print("File copy successful.")
  }



  # Call the function being tested
  process_folder <- file.path(temp_output, "testdata\\")
  plot <- plot_measurement_data(process_folder,
    variable = "CCI",
    code = "CSTA",
    tstart = "2023-07-10",
    tend = "2023-08-18",
    plot_type = "time_series"
  )

  # Check that the returned object is a plotly object
  expect_true(inherits(plot, "plotly"), info = "The function did not return a plotly object")
})
