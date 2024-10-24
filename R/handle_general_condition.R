#' Handler for General Warnings and Errors
#'
#' @param cond A condition object representing the warning or error to be handled.
#' @param type Character string specifying the type of condition: either "error" or "warning". Defaults to "error".
#' @param env An environment object used for logging warnings or errors during the process.
#'
#' @details This function handles general warnings and errors by logging the message using `log_general_issue`.
#'          If the condition is an error, it stops execution by calling `stop` with the message.
#'          If the condition is a warning, it re-issues the warning using `warning`.
#' @noRd
handle_general_condition <- function(cond, type = "error", env) {
  msg <- paste(type, ":", conditionMessage(cond))
  log_general_issue(msg)
  env$all_issues <- append(env$all_issues, list(msg))
}
