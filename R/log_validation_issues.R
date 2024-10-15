#' Log Validation Issues
#' This function logs validation warnings and generates a summary message.
#'
#' @param validation_warnings A list or character vector of validation warnings to be logged.
#' @param env An environment object used for logging the summary message.
#'
#' @return This function does not return a value. It logs the warnings and adds a message to the environment.
#' @noRd
log_validation_issues <- function(validation_warnings, env) {
 summary_msg <- paste0("Input data validation completed with ",
                       length(validation_warnings),
                       " warning",
                       ifelse(length(validation_warnings) == 1, "", "s"),
                       " Check 'val_issues.txt' for details. The script has been aborted.")

 log_validation_issue(validation_warnings)
 add_message(env, summary_msg, "issues")
}
