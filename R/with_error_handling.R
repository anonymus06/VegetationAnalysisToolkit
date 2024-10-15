#' Execute an Action with Error Handling
#'
#' This function executes a given action wrapped in error handling. If an error occurs during the execution of the action,
#' it will catch the error, display a message, and return `NULL`. If no error occurs, the result of the action is returned.
#'
#' @param action A function representing the action to be executed.
#' @param ... Additional arguments to be passed to the action function.
#'
#' @return The result of the action if successful, or `NULL` if an error occurs.
#' @noRd
with_error_handling <- function(action, ...) {
 result <- tryCatch(
  {
   action(...)
  },
  error = function(e) {
   message(paste0("An error occurred: ", e$message))
   return(NULL)
  }
 )

 if (is.null(result)) {
  message("The result is NULL. Check the action for potential issues.")
  return(invisible(NULL))
 }

 return(result)
}
