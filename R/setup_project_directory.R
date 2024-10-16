#' Set Up Project Directory for VegetationAnalysisToolkit
#'
#' This function sets up an example directory structure for a Vegetation Analysis Toolkit project,
#' including sample data, example scripts, and documentation files.
#'
#' @param project_dir A character string specifying the name of the project directory.
#'                    Defaults to "VegetationAnalysisToolkit_Project".
#'
#' @details This function creates the following directory structure within the project directory:
#' - `data/`: Contains sample data files for NDVI and Chlorophyll data.
#' - `examples/`: Contains example R scripts.
#' - `outputs/`: A folder for storing analysis results.
#' - `docs/`: Documentation files like vignettes and the README file.
#'
#' Additionally, it sets up a `main.R` script for users to start their analysis workflow,
#' and generates a `README.txt` with instructions for getting started.
#'
#' @return The normalized path to the created project directory.
#'
#' @examples
#' \dontrun{
#' setup_project_directory("My_Vegetation_Project")
#' }
#' @export
setup_project_directory <- function(project_dir = "VegetationAnalysisToolkit_Project") {
 if (!dir.exists(project_dir)) {
  dir.create(project_dir, recursive = TRUE)
 }
 dirs_to_create <- c("data", "examples", "outputs", "docs")
 subdirs_to_create <- c("plantpen_ndvi_pri_data", "mc100_chlorophyll_data")
 sample_to_create <- c("sample")

 for (dir in dirs_to_create) {
  dir.create(file.path(project_dir, dir), recursive = TRUE)
  if (dir == "data") {
   dir.create(file.path(project_dir, dir, sample_to_create))
   for (subdir in subdirs_to_create) {
    dir.create(file.path(project_dir, dir, sample_to_create, subdir))
   }
  }
 }

 data_files <- list.files(system.file("extdata", package = "VegetationAnalysisToolkit"),
                          full.names = TRUE, recursive = TRUE)

 for (file in data_files) {
  if (grepl("plantpen_ndvi_pri_data", file)) {
   file.copy(file, file.path(project_dir, "data", "sample", "plantpen_ndvi_pri_data"))
  } else if (grepl("mc100_chlorophyll_data", file)) {
   file.copy(file, file.path(project_dir, "data", "sample", "mc100_chlorophyll_data"))
  } else if (grepl("config", file)){
   file.copy(file, file.path(project_dir, "data", "sample"))
  }
 }
 example_files <- list.files(system.file("examples", package = "VegetationAnalysisToolkit"),
                             full.names = TRUE)
 file.copy(example_files, file.path(project_dir, "examples"), recursive = TRUE)

 vignette_files <- list.files(system.file("doc", package = "VegetationAnalysisToolkit"),
                              full.names = TRUE, recursive = TRUE)
 if (length(vignette_files) > 0) {
  file.copy(vignette_files, file.path(project_dir, "docs"), recursive = TRUE)
 }

 readme_file <- system.file("README.md", package = "VegetationAnalysisToolkit")
 if (file.exists(readme_file)) {
  file.copy(readme_file, file.path(project_dir, "docs"))
 }

 instructions_file <- file.path(project_dir, "README.txt")
 writeLines(c(
  "Welcome to your Vegetation Analysis Toolkit Workspace!",
  "",
  "This directory contains the following structure:",
  "",
  "- data/: Contains sample data files for testing and examples.",
  "  - sample/: Example project data.",
  "    - plantpen_ndvi_pri_data/: Sample NDVI data.",
  "    - mc100_chlorophyll_data/: Sample Chlorophyl data.",
  "- examples/: Contains example scripts to help you get started.",
  "- outputs/: Use this folder to store the results of your analyses.",
  "- docs/: Contains documentation files like vignettes and the README.md.",
  "",
  "To get started:",
  "1. Open the example scripts in the examples/ folder to explore the basic functionalities.",
  "2. Use the 'data/' directory for your input data and store output results in 'outputs/'.",
  "3. Customize the 'main.R' script for your specific analysis workflows.",
  "",
  "Enjoy your data exploration!"
 ), con = instructions_file)

 main_r_file <- file.path(project_dir, "main.R")
 writeLines(c(
  "# Main script for the VegetationAnalysisToolkit project.",
  "# Start typing your commands here!"
 ), con = main_r_file)

 # Save the date of the first use of this function!
 user_config_dir <- tools::R_user_dir("VegetationAnalysisToolkit", which = "/inst/extdata/config")
 if (!dir.exists(user_config_dir)) {
  dir.create(user_config_dir, recursive = TRUE)
 }
 first_run_file <- file.path(user_config_dir, "setup_first_run_date.txt")
 if (!file.exists(first_run_file)) {
  # If the file doesn't exist, capture today's date as the first run date
  first_run_date <- Sys.Date()
  writeLines(as.character(first_run_date), first_run_file)
 }

 return(normalizePath(project_dir))
}
