#' Convert Data Types of Specific Columns in a Data Frame
#'
#' @param df A data frame whose columns need data type conversion.
#'
#' @return A data frame with the first two columns converted to numeric, the third column to character,
#'         and the fourth column to date format. If date conversion fails, NA is inserted for those values.
#'
#' @note It's important to ensure the input data frame has at least four columns and that the data
#'       in each column is appropriate for the intended conversion. In the specific context where this function
#'       is utilized within the code, these prerequisites are ensured by the logical structure and sequence
#'       of operations leading up to this point. Therefore, while general caution is advised, the unique
#'       circumstances of this function's application mitigate the need for additional checks within the function itself.
#'
#' @noRd
convert_data <- function(df) {

 if (ncol(df) < 4) {
  stop("The input data frame must have at least four columns.")
 }

 df[, c(1, 2)] <- lapply(df[, c(1, 2)], function(x) {
  as.numeric(gsub(",", ".", x))
 })
 df[, 3] <- as.character(df[, 3])
 df[, 4] <- tryCatch(
  as.Date(df[, 4], format = "%Y-%m-%d"),
  error = function(e) {
   warning("Failed to convert the fourth column to Date. NAs have been inserted.")
   return(rep(NA, nrow(df)))
  }
 )

 return(df)
}
