#' Logging Function for General Errors and Warnings
#'
#' @param msg Character string containing the message to be logged.
#' @param log_file Character string specifying the file to which the log message should be written. Defaults to "error_log.txt".
#'
#' @details This function logs general error and warning messages to a specified log file.
#'          Each log entry is prepended with a timestamp indicating when the log entry was made.
#'          The log messages are appended to the existing content of the log file.
#'
#' @return The function does not return a value. It writes log messages to the specified log file.
#'
#' @noRd
log_general_issue <- function(msg, log_file = "error_log.txt") {
 timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
 con <- file(log_file, open = "a")
 on.exit(close(con))

 if (is.character(msg)) {
  message <- paste(timestamp, "-", msg)
  writeLines(message, con = con)
 } else if (is.list(msg)) { # redundant?
  lapply(unlist(msg), function(m) {
   message <- paste(timestamp, "-", gsub("\n", "\\n", m))
   writeLines(message, con = con)
  })
 }
}
