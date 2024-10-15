#' Process and Clean DataFrame
#'
#' @param df A data frame to be processed.
#'
#' @details
#' This function processes a data frame by performing the following steps:
#' - If the data frame has only one column, it assumes it's in a raw format (e.g., MC-100 Chlorophyll data)
#'   and splits the column into multiple columns.
#' - The function filters out any rows where the values match the column names, as they might
#'   be erroneously interpreted as data.
#'
#' @return A list containing:
#'         - `current_df`: The processed data frame.
#'         - `current_df_old`: The original data frame before processing.
#' @noRd
process_dataframe <- function(df) {
 if (ncol(df) == 1) { # (= MC-100 Chlorophyll data)
  df <- split_columns(df)
 }
 df_old <- df
 df <- filter_rows_not_in_columns(df, names(df))

  return(list(current_df = df, current_df_old = df_old))
}
