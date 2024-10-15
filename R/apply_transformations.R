#' Apply Transformations to Merged Data Frames
#'
#' This function applies a series of transformations to a list of merged data frames.
#' The transformations include filtering columns, adding a date column, renaming columns,
#' and converting data types.
#'
#' @param merged_data_frames A list of data frames that have been merged with their corresponding index data.
#' @param variable A string representing the variable being processed (e.g., NDVI, PRI, or CCI).
#'
#' @details The function iterates over each data frame in the input list, performing the following steps:
#'          1. Filters the data frame to retain only specified columns: "index", "Reading", and "description".
#'          2. Adds a date column derived from the data frame's name.
#'          3. Renames the columns based on predefined new names.
#'          4. Converts data types of the columns to ensure consistency (numeric for indices and readings, character for codes, and date for dates).
#'
#' @return A list of named data frames, with the same names as the input list.
#'
#' @importFrom dplyr %>%
#'
#' @noRd
apply_transformations <- function(merged_data_frames, variable) {

 transformed_data_frames <- lapply(names(merged_data_frames), function(name) {
  df <- merged_data_frames[[name]]

  columns_to_keep <- c("index", "Reading")

  if ("description" %in% names(df)) {
   columns_to_keep <- c(columns_to_keep, "description")
  } else {
   stop("There were no description' columns found in the index files!")
  }

  if(all(columns_to_keep %in% names(df))) {
   df_filtered <- df[columns_to_keep]
   date_str <- gsub("[^0-9]", "", name)
   new_names <- c("index", variable, "Code", "Date")

   df_transormed <- df_filtered %>%
    add_date_column(date_str) %>%
    rename_columns_by_position(new_names) %>%
    convert_data

   return(df_transormed)

  } else {
   stop("Data frame does not contain all required columns: ", paste(columns_to_keep, collapse=", "))
  }
 })

 names(transformed_data_frames) <- names(merged_data_frames) # maybe it is totally redundant, if so, skip it!
 return(transformed_data_frames)
}
