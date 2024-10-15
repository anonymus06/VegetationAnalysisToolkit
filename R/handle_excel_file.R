#' Handle Excel Files
#'
#' This function reads all the sheets of an Excel file into a list of data frames.
#' It also maps the sheet names to the file path for reference.
#'
#' @param file A character string representing the path to the Excel file to be read.
#'
#' @details This function loads an Excel workbook, reads each sheet, and returns the data along with a mapping between
#' sheet names and the file path. It starts reading from the first row of each sheet.
#' The function supports reading multiple sheets from the same Excel file and handles each one independently.
#'
#' @return A list containing:
#'         - `data`: A named list of data frames for each sheet in the Excel file.
#'                   Each sheet is stored under its corresponding sheet name.
#'         - `files`: A named list mapping sheet names to the file path.
#'
#' @importFrom openxlsx getSheetNames loadWorkbook read.xlsx
#' @noRd
handle_excel_file <- function(file) {
 sheets <- getSheetNames(file)
 wb <- loadWorkbook(file)
 sheet_data <- lapply(sheets, function(sheet) {
  read.xlsx(wb, sheet = sheet, startRow = 1)
 })
 names(sheet_data) <- sheets

 return(list(data = sheet_data, files = setNames(rep(file, length(sheets)), sheets)))
}
