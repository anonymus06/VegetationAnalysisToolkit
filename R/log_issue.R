#' Log issues
#'
#' @param file The file with an issue.
#' @param issue The issue description.
#' @param env An environment object used for logging the issue.
#'
#' @details This function logs a specific issue related to a file. The issue is formatted into a message
#'          and passed to `handle_general_condition` for logging as a warning. The environment (`env`) is
#'          used to track the issue for further reference.
#'
#' @return The function does not return a value. It logs the issue using `handle_general_condition`.
#'
#' @noRd
log_issue <- function(file, issue, env) {
  message <- paste("Issue with file:", file, "-", issue)
  handle_general_condition(simpleError(message), "warning", env)
}
