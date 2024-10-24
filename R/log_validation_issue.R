#' Logging Function for Validation Issues
#'
#' @param msg Character string containing the message(s) to be logged.
#' @param log_file Character string specifying the file to which the log message should be written. Defaults to "val_issues.txt".
#'
#' @details This function logs validation-related messages to a specified log file.
#'          Each log entry is prepended with a timestamp indicating when the log entry was made.
#'          The log messages are appended to the existing content of the log file.
#'
#' @return The function does not return a value. It writes log messages to the specified log file.
#'
#' @noRd
log_validation_issue <- function(msg, log_file = "val_issues.txt") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  con <- tryCatch(
    {
      file(log_file, open = "a")
    },
    error = function(e) {
      stop("Unable to open log file: ", log_file)
    }
  )
  on.exit(close(con))

  if (is.character(msg)) {
    message <- paste(timestamp, "-", msg)
    writeLines(message, con = con)
  } else if (is.list(msg)) {
    lapply(unlist(msg), function(m) {
      message <- paste(timestamp, "-", gsub("\n", "\\n", m))
      writeLines(message, con = con)
    })
  } else {
    warning("Message to log is neither character nor list.")
  }
}
