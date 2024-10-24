#' Filtering out rows that might inadvertently contain column names as data
#'
#' This function filters out rows in the data frame that contain only values
#' matching the column names, which can sometimes appear as data due to
#' incorrect formatting or parsing.
#'
#' @param df The data frame to filter.
#' @param required_columns Character, a vector of required column names.
#'
#' @return A filtered data frame with rows containing only column names removed.
#'         Rows where all values match a column name will be excluded.
#'
#' @noRd
filter_rows_not_in_columns <- function(df, required_columns) {
  rows_to_keep <- apply(df, 1, function(row) !all(row %in% required_columns))
  filtered_df <- df[rows_to_keep, ]
  return(filtered_df)
}
