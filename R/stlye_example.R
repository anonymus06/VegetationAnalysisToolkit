#' Preprocess All Data Frames with Segmentation
#'
#' This function preprocesses both data and index data frames by segmenting them
#' and adding unique identifiers. It is designed to handle data frames where
#' multiple segments exist within a single data frame, often demarcated by specific
#' header rows. The function differentiates between data and index files based on
#' their naming conventions.
#'
#' @param data A list containing data frames to be preprocessed.
#'
#' @return A list with two elements: `data_files_processed` and `index_files_processed`,
#'         each containing preprocessed data frames with added unique identifiers.
#'
#' @importFrom dplyr %>%
#' @noRd
preprocess_all_data_frames <- function(data) {

 # ---- initial part of procedure ----
 data_file_names <- names(data)[grepl("\\.CSV$", names(data))]
 index_file_names <- names(data)[!grepl("\\.CSV$", names(data))]

 # Function to preprocess data frames, adding unique identifiers based on segments
 preprocess_data_df_with_segments <- function(data_df) {
  segment_counter <- 1 # Initialize a segment counter
  unique_ids <- vector("character", nrow(data_df)) # Initialize unique IDs vector
  header_rows <- data_df$Sample == "Sample" # Identify header rows to skip

  # Iterate through rows to identify segments and assign unique identifiers
  for (i in 1:nrow(data_df)) {
   if (header_rows[i]) {
    segment_counter <- segment_counter + 1
    next
   }

   # Create a unique identifier for the current row
   # Here, we're assuming 'Sample' column is already processed to a consistent format (e.g., as integer)
   sample_num <- sprintf("%03d", as.integer(data_df$Sample[i]))
   unique_ids[i] <- paste0(segment_counter, "_", sample_num)
  }

  # Assign unique IDs, excluding rows with empty 'Sample'
  data_df$UniqueID <- unique_ids[data_df$Sample != ""]

  # Remove header rows from the final data frame
  data_df <- data_df[!header_rows, ]

  return(data_df)
 }

 # Function to preprocess index frames by adding unique identifiers based on segments
 preprocess_index_df_with_segments <- function(data_df) {
  segment_counter <- 1
  unique_ids <- vector("character", nrow(data_df))
  header_rows <- data_df$index == "index"

  for (i in 1:nrow(data_df)) {
   if (header_rows[i]) {
    segment_counter <- segment_counter + 1
    next
   }

   # Create unique identifier using segment counter and formatted index number
   sample_num <- sprintf("%03d", as.integer(data_df$index[i])) # Ensure 'Sample' is a string with leading zeros
   unique_ids[i] <- paste0(segment_counter, "_", sample_num)
  }

  # Assign unique IDs, excluding rows with 'NA' index
  data_df$UniqueID <- unique_ids[data_df$index != "NA"]

  # Remove header rows from the final data frame #todo: eddig miért mukodott enelkul?
  data_df <- data_df[!header_rows, ]

  return(data_df)
 }


 # ---- main part of procedure ----
 data_files_processed <- lapply(data[data_file_names], function(df) {
  if (is.data.frame(df) && nrow(df) > 0) {
   preprocess_data_df_with_segments(split_columns(df))
  } else {
   return(df)
  }
 })

 index_files_processed <- lapply(data[index_file_names], function(df) {
  if (is.data.frame(df) && nrow(df) > 0) {
   preprocess_index_df_with_segments(df)
  } else {
   return(df)
  }
 })

 return(list(data_files_processed = data_files_processed,
             index_files_processed = index_files_processed))
}
