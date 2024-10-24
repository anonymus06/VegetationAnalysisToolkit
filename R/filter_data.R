#' Filter Data Frames by Removing Junk Columns in PlantPen NDVI & PRI
#'
#' This function removes any unnecessary columns that appear after the variable (e.g. NDVI) column
#' in the last data frame of the input list.
#'
#' @param df A list of data frames where the last data frame contains the measurement data with the variable column.
#' @param variable A string representing the variable being processed (e.g., NDVI, PRI, or CCI).
#'
#' @return A modified list of data frames where the last data frame in the list has columns only up
#' to and including the variable column.
#'
#' @details The function identifies the position of the variable column in the last data frame of
#' the input list. Based on this position, it truncates the last data frame to keep only columns
#' up to and including the variable column, discarding any columns that follow.
#'
#' @note This function assumes the last data frame contains a variable column. If the assumption is incorrect,
#' the function will return `NULL`. Additionally, make sure that the column structure doesn't change unexpectedly
#' during data pre-processing to avoid issues with the column indexing.
#'
#' @noRd
filter_data <- function(df, variable) {
  var_col_index <- which(names(df[[length(df)]]) == variable)

  if (length(var_col_index) == 0) {
    message(paste0(variable, "column not found in the dataset. Returning NULL."))
    return(NULL)
  }

  df[[length(df)]] <- df[[length(df)]][, 1:var_col_index]
  return(df)
}
