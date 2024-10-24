#' Summarize and Add Issues to Environment for Console Output
#'
#' This function summarizes the issues encountered during processing and adds them to the environment as a message
#' for console output.
#'
#' @param env An environment object used for logging warnings or errors during the process.
#' @param validation_run Logical, defaults to TRUE. If FALSE, a message indicating that validation was not run is added to the environment.
#'
#' @return description
#' @noRd
summarize_and_log_issues <- function(env, validation_run = TRUE) {
  if (length(env$all_issues) > 0) {
    summary_msg <- paste0(
      "Processing completed with ",
      length(env$all_issues),
      " issue",
      ifelse(length(env$all_issues) == 1, "", "s"),
      " (errors/warnings). Check 'error_log.txt' for details. Processing is terminated."
    )
    add_message(env, summary_msg, "issues")
  } else {
    # Optionally add a success message if no issues were found
    # add_message(env, "Processing completed without any issues. No problems detected in executed steps.", "issues")
  }

  if (!validation_run) {
    add_message(env, "Data validation was not run.", "issues")
  }
}
