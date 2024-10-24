#' Package Startup Message
#'
#' This function displays a startup message with version, release date, and instructions when the package is attached.
#'
#' @param libname The library name.
#' @param pkgname The package name.
#'
#' @return No return value. The function displays a startup message when the package is attached.
#' @noRd
.onAttach <- function(libname, pkgname) {
 if (interactive()) {
  package_info <- utils::packageDescription(pkgname)
  version <- package_info$Version
  release_date <- NA

  if (!is.null(package_info$Packaged) && nzchar(package_info$Packaged)) {
   release_date <- strsplit(package_info$Packaged, " ")[[1]][1]
  }
  if (is.na(release_date)) {
   release_date <- "Unknown"
  }

  packageStartupMessage(paste("Welcome to VegetationAnalysisToolkit", version, "(", release_date, ")!"))
  packageStartupMessage("")
  packageStartupMessage("This program comes with ABSOLUTELY NO WARRANTY; for details type 'show_w()'.")
  packageStartupMessage("This is free software, and you are welcome to redistribute it under certain conditions; type 'show_c()' for details.")
  packageStartupMessage("")
  packageStartupMessage("Kickstart your work by typing 'setup_project_directory()' to create a project folder with sample data!")
  packageStartupMessage("Or dive straight into the details just type 'open_vignette()' for documentation and examples.")
  packageStartupMessage("Need assistance? Type 'help(package=\"VegetationAnalysisToolkit\")' for guidance.")
  packageStartupMessage("")
  check_for_cake_day(pkgname)
 }
}
