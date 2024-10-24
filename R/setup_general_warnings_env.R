#' Setup Environment for General Warnings and Messages
#'
#' This function sets up an environment to track general warnings, issues, and messages during data processing or validation.
#'
#' @return An environment object with fields to store all issues (`all_issues`) and messages (`messages`).
#'
#' @details This environment can be passed into functions that perform validations or logging, allowing them
#'          to add issues and messages dynamically, which can later be retrieved or processed as needed.
#' @noRd
setup_general_warnings_env <- function() {
  env <- new.env()
  env$all_issues <- list()
  env$messages <- list()
  return(env)
}
