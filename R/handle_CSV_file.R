#' Handle CSV Files
#'
#' This function reads a CSV file with a specified separator and loads the data into a list.
#'
#' @param file A character string representing the path to the CSV file to be read.
#'
#' @return A list containing:
#'         - `data`: A named list where the key is the base name of the file, and the value is the data frame
#'                   from the CSV file.
#'         - `files`: A named list where the key is the base name of the file, and the value is the file path.
#'
#' @importFrom utils read.csv
#' @noRd
handle_CSV_file <- function(file) {
  data <- list()
  files <- list()
  sheets <- read.csv(file, sep = ";", header = TRUE, encoding = "UTF-8", check.names = FALSE)
  data[[basename(file)]] <- sheets
  files[[basename(file)]] <- file

  return(list(data = data, files = files))
}
