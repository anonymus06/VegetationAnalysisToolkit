#' Add a Date Column to a Data Frame Based on a Date String
#'
#' @param df A data frame to which the date column will be added.
#' @param date_str A string representing a date, extracted from the names of data files.
#'        The function expects this string to be in one of two formats:
#'        - 'yymmddhh' (year, month, day, hour)
#'        - 'yymmdd' (year, month, day)
#'        The function attempts to detect the format automatically based on the string length
#'        and pattern.
#'
#' @return The input data frame with an added 'Date' column. If the date string matches
#'         one of the expected formats, this column will contain date values corresponding
#'         to the string. If the format is unrecognized, the column will be filled with NA,
#'         and a warning will be issued.
#'
#' @examples
#' df <- data.frame(Value = c(1, 2, 3))
#' date_str <- "231001"
#' add_date_column(df, date_str)
#'
#' @noRd
add_date_column <- function(df, date_str) {
 if (grepl("\\d{6}\\d{2}", date_str)) {
  df$Date <- as.Date(substr(date_str, 1, 6), format = "%y%m%d")
 } else if (grepl("\\d{6}", date_str)) {
  df$Date <- as.Date(date_str, format = "%y%m%d")
 } else {
  df$Date <- NA
  warning(
   "Unable to assign dates to all data entries! Date string format unrecognized for: ",
   date_str, ". Date column set to NA.")
  # warning("Unable to assign dates to all data entries! Ensure your input file names conform to the expected formats and try to run the code again!")
 }

 return(df)
}
