#' Handle Text Files
#'
#' This function reads a text file, skipping the specified number of rows.
#' It is designed to handle tab-separated text files, with the first few rows optionally skipped.
#' In case of an error or if the file is unreadable, it logs the issue and skips further processing of the file.
#'
#' @param file A character string representing the path to the text file to be read.
#' @param skip_rows Integer. The number of rows to skip when reading the text file. Default is 4.
#' @param env A list-like object where messages, issues, or warnings can be logged for later inspection or reporting.
#'
#' @return A list containing:
#'         - `data`: A named list with the data frame from the text file (name is the basename of the file).
#'         - `files`: A list mapping the file name to the file path.
#'
#' @details This function reads a tab-separated text file and skips the first `skip_rows` rows.
#' If an error occurs during reading (e.g., malformed file), it logs the error message and returns
#' a list with the file path and `NULL` as the data. It also logs a message in the provided environment.
#'
#' @importFrom utils read.table
#' @noRd
handle_text_file <- function(file, skip_rows = 4, env) {
  data <- list()
  files <- list()

  tryCatch(
    {
      sheet_data <- read.table(file, header = TRUE, sep = "\t", skip = 4)
      data[[basename(file)]] <- sheet_data
      files[[basename(file)]] <- file
    },
    error = function(e) {
      data[[basename(file)]] <- NULL
      files[[basename(file)]] <- file
      if (is.null(data[[basename(file)]])) {
        add_message(env, "The data frame does not exist or is NULL.", "issues")
      }
      add_message(env, paste("Error reading file:", file, "-", e$message), "issues")
    }
  )

  return(list(data = data, files = files))
}
