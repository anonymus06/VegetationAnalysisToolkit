#' Generate Project Header for Output Files
#'
#' This function generates a standard project header to be included in output files.
#' The header contains metadata such as the project name, file content description,
#' file name, script name, software version, and the timestamp when the file was generated.
#'
#' @param file_name A character string representing the name of the output file.
#' @param file_content A character string describing the content of the output file.
#' @param script_name description
#'
#' @return A character string representing the formatted project header.
#'
#' @importFrom tools toTitleCase
#' @importFrom utils packageVersion
#' @noRd
generate_project_header <- function(file_name, file_content, script_name) {

 project_name <- tools::toTitleCase(basename(getwd()))
 software_version <- paste0("VegetationAnalysisToolkit ", as.character(utils::packageVersion("VegetationAnalysisToolkit")))
 generated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

 capture.output({
  cat("* Project:          ", project_name, "\n")
  cat("* File content:     ", file_content, "\n")
  cat("* File name:        ", file_name, "\n")
  cat("* Script name:      ", script_name, "\n")
  cat("* Software version: ", software_version, "\n")
  cat("* Generated at:     ", generated_at, "\n\n")
 })
}
