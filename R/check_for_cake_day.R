#' Check for Cake Day Anniversary
#'
#' This function checks if the current date is the anniversary of the first time the package was run and displays a message.
#'
#' @param package_name The name of the package.
#'
#' @noRd
check_for_cake_day <- function(package_name = "VegetationAnalysisToolkit") {
 user_config_dir <- tools::R_user_dir(package_name, which = "config")

 if (dir.exists(user_config_dir)) {
  first_run_file <- file.path(user_config_dir, "setup_first_run_date.txt")
  if (file.exists(first_run_file)) {
   first_run_date <- as.Date(readLines(first_run_file))
   today <- Sys.Date()
   if (format(today, "%m-%d") == format(first_run_date, "%m-%d") && first_run_date != today) {
    message("🎉 Happy Cake Day! 🎂 It's been another year since you set up your first 'VegetationAnalysisToolkit' project!")
   }
  }
 }

}
