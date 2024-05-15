#'

#' Define a function to split a column with multiple data into separate columns (as many as data we have separated by commas)!

#' @param input_data A data frame where the first column contains multiple data points separated by commas.
#'
#' @return A data frame with the first column split into multiple columns based on comma separation,
#'         with each new column named according to the split parts.
#'
#' @importFrom dplyr rename_with everything %>%
splitColumns <- function(input_data) {

  # Split the first column at commas and apply trimws to each split part to remove leading/trailing whitespace.
  # Note: This operation assumes that the first column of the input data contains the data points to be split.
  split_columns <- lapply(strsplit(input_data[, 1], ","), function(x) trimws(x))

  # Convert the list of split elements into a data frame
  # This is done by binding the rows of the list into a single data frame.
  split_df <- data.frame(do.call(rbind, split_columns), stringsAsFactors = FALSE)

  # Extract the column names from the input data frame.
  # This step prepares for renaming the split columns in the resulting data frame.
  input_vector <- colnames(input_data)

  # Split the column names at commas, removing any empty strings that might arise.
  # This ensures that only meaningful names are used for the newly created columns.
  parts <- unlist(strsplit(input_vector, ","))[unlist(strsplit(input_vector, ",")) != ""]

  # Rename the columns of the split data frame using the parts extracted from the column names.
  # This step applies the meaningful names to the newly created columns from the split operation.
  split_df <- split_df %>%
    rename_with(~ parts, everything())

  return(split_df)
}

#' Preprocess All Data Frames with Segmentation

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
preprocess_all_data_frames <- function(data) {

  # ---- initial part of procedure ----

  # Identify data and index file names based on their naming conventions
  data_file_names <- names(data)[grepl("\\.CSV$", names(data))]
  index_file_names <- names(data)[!grepl("\\.CSV$", names(data))]

  # Function to preprocess data frames, adding unique identifiers based on segments
  preprocess_data_df_with_segments <- function(data_df) {
    # Initialize a segment counter
    segment_counter <- 1
    # Initialize an empty vector to store unique identifiers
    unique_ids <- vector("character", nrow(data_df))

    # Iterate through rows to identify segments and assign unique identifiers
    for (i in 1:nrow(data_df)) {
      if (data_df$Sample[i] == "Sample") { # Identify segment headers
        segment_counter <- segment_counter + 1 # Increment segment counter
        next # Skip this row in the final data
      }

      # Create a unique identifier for the current row
      # Here, we're assuming 'Sample' column is already processed to a consistent format (e.g., as integer)
      sample_num <- sprintf("%03d", as.integer(data_df$Sample[i]))
      unique_ids[i] <- paste0(segment_counter, "_", sample_num)
    }

    # Add unique identifiers to the data frame, excluding NA sample rows
    data_df$UniqueID <- unique_ids[data_df$Sample != "NA"]

    return(data_df)
  }

  # Similar preprocessing function for index data frames
  preprocess_index_df_with_segments <- function(data_df) {
    segment_counter <- 1
    unique_ids <- vector("character", nrow(data_df))
    for (i in 1:nrow(data_df)) {
      if (data_df$index[i] == "index") {
        segment_counter <- segment_counter + 1
        next # Skip this row in the final data
      }

      sample_num <- sprintf("%03d", as.integer(data_df$index[i])) # Ensure 'Sample' is a string with leading zeros
      unique_ids[i] <- paste0(segment_counter, "_", sample_num)
    }

    data_df$UniqueID <- unique_ids[data_df$index != "NA"]

    return(data_df)
  }


  # ---- main part of procedure ----

  # Apply preprocessing to data files, ensuring they are split into columns if necessary
  data_files_processed <- lapply(data[data_file_names], function(df) {
    if (is.data.frame(df) && nrow(df) > 0) {
      preprocess_data_df_with_segments(splitColumns(df))
    } else {
      return(df)  # Return unchanged if not a data frame or empty
    }
  })

  # Apply preprocessing to index files
  index_files_processed <- lapply(data[index_file_names], function(df) {
    if (is.data.frame(df) && nrow(df) > 0) {
      preprocess_index_df_with_segments(df)
    } else {
      return(df)  # Return unchanged if not a data frame or empty
    }
  })

  return(list(data_files_processed = data_files_processed,
              index_files_processed = index_files_processed))
}

#' A function that gets rid of every junk column that goes after the NDVI column

#' @param df A list of data frames where the last data frame contains the NDVI column.
#'
#' @return A modified list of data frames where the last data frame in the list having columns only up
#' to and including the NDVI column.
#'
#' @details The function identifies the position of the NDVI column in the last data frame of
#' the input list. Based on this position, the function then truncates the last
#' data frame in the list to include only columns up to and including the NDVI column.
#'
#' @note This function assumes that there is an `NDVI` column in the last data frame of the list.
#' If these assumption does not hold, the function may not perform as expected.
filter_data <- function(df){



  # Identify the position of the NDVI column in the last data frame of the list
  ndvi_col_index <- which(names(df[[length(df)]]) == "NDVI")

  # Update the last data frame in the list to retain only columns up to and including the NDVI column
  # This operation ensures that all columns following the NDVI column are removed
  df[[length(df)]] <- df[[length(df)]][, 1:ndvi_col_index]

  return(df)
}
