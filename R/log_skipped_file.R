#' Log Skipped Files
#'
#' This function logs files that were skipped during processing, along with a reason for why they were skipped.
#'
#' @param file A character string representing the path of the file that was skipped.
#' @param reason A character string explaining why the file was skipped (e.g., "temporary file", "invalid naming pattern").
#' @param env The environment object where messages are logged for tracking skipped files.
#'
#' @return No return value. The function logs the skipped file and reason to the environment.
#'
#' @details This function can be used to track and log any files that were not processed due to specific reasons,
#'          such as invalid file types, naming patterns, or being temporary system files. The messages are appended
#'          to the environment's message list for later use.
#'
#' @noRd
log_skipped_file <- function(file, reason, env) {
 add_message(env, paste("Skipped", reason, "from reading:", file), "info")
}
