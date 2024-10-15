#' Split a Column into Multiple Columns Based on Comma Separation
#'
#' This function takes the first column of a data frame, splits its contents based on commas,
#' and expands the data into multiple columns. Each new column is named according to the split parts.
#'
#' @param input_data A data frame where the first column contains multiple data points separated by commas.
#'
#' @return A data frame with the first column split into multiple columns based on comma separation,
#'         with each new column named according to the split parts.
#'
#' @importFrom dplyr rename_with everything %>%
#' @noRd
split_columns <- function(input_data) {
 split_columns <- lapply(strsplit(input_data[, 1], ","), function(x) trimws(x))
 split_df <- data.frame(do.call(rbind, split_columns), stringsAsFactors = FALSE)

 input_vector <- colnames(input_data)
 parts <- unlist(strsplit(input_vector, ","))[unlist(strsplit(input_vector, ",")) != ""]

 split_df <- split_df %>%
  rename_with(~ parts, everything())

 return(split_df)
}
