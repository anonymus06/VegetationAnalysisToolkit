#' General-purpose helper functions.


#' Check if a file is a directory
#'
#' @param file The file path to check.
#'
#' @return TRUE if the file is a directory, FALSE otherwise.
is_directory <- function(file) {
 file.info(file)$isdir
}

map_all_NDVI <- function(all_data, last_df, split_code, env) {
mapply(
 function(data, data_name) map_NDVI(data, data_name, last_df, split_code, env),
 all_data[-length(all_data)],
 names(all_data)[-length(all_data)],
 SIMPLIFY = FALSE
)
}

#' Function to determine if a data frame is from an index file
#'
#' @param df_name The name of the data frame.
#' @param data_frame_files A list mapping data frame names to file paths.
#'
#' @return TRUE if the data frame is from an index file, FALSE otherwise.
is_index_file <- function(file_path) { # todo: variable - documentum javit , clean column-nál, filter_data-nál, handle_text_file-nál is! /meg mindenhol ellenoriz
 grepl("index.xlsx", file_path, ignore.case = TRUE)
}


#' Helper function to check if a sequence is sequential
#'
#' @param df A data frame containing the sequence.
#' @param verbose Logical, if TRUE prints detailed messages.
#'
#' @return TRUE if the sequence is sequential, FALSE otherwise.
check_sequential_index <- function(df, verbose = FALSE) {
  # Identifying segments with "Sample" headers for separate sequentiality checks
  split_indices <- which(df$Sample == "Sample")
  # Append an additional index to include the last data segment
  split_indices <- c(split_indices, nrow(df) + 1)

  index0 <- 1
  for (i in seq_along(split_indices)) {
    index <- split_indices[i]
    # Adjust the segment range to handle the last chunk correctly
    if (i == length(split_indices)) {
      seq_df <- df[index0:(nrow(df)), ]
    } else {
      seq_df <- df[index0:(index - 1), ]
    }
    index0 <- index + 1 # Prepare for the next segment

    # Exclude "Sample" header rows and convert to numeric for sequentiality check
    numeric_samples <- suppressWarnings(as.numeric(seq_df$Sample[seq_df$Sample != "Sample"]))

    # Verify sequentiality within the segment
    if (any(diff(numeric_samples) != 1)) {
      if (verbose) {
        print(sprintf("Non-sequential values found in 'Sample' column within rows %d to %d.", index0, index - 1))
      }
      return(FALSE)
    }
    if (verbose) {
      print(sprintf("Checked sequentiality up to row %d: OK", index - 1))
    }
  }

  return(TRUE) # Return TRUE if all segments pass the sequentiality check
}

#' Process and Clean DataFrame
#' @param df A data frame to be processed.
#'
#' @return A list containing:
#'         - `current_df`: The processed data frame.
#'         - `current_df_old`: The original data frame before processing.
process_dataframe <- function(df) {
  # Check if the number of columns is 1 (= Chlorofile data) and split the columns if true
  if (ncol(df) == 1) {
    df <- splitColumns(df)
  }

  # Save an old copy for sequential index check
  df_old <- df

  # Filter out rows that match column names
  df <- filter_rows_not_in_columns(df, names(df))

  # Return the processed dataframe and its old copy
  return(list(current_df = df, current_df_old = df_old))
}

#' Helper function to add identified issues to the list
#'
#' @param issues A list containing named vectors of issue messages.
#' @param name The category name under which the issue message should be added.
#' @param issue_message The message describing the issue.
#'
#' @return The updated list of issues with the new issue message appended.
add_issue <- function(issues, name, issue_message) {
  issues[[name]] <- c(issues[[name]], issue_message)
  return(issues)
}

with_error_handling <- function(action, ...) {
 result <- action()
 if (is.null(result)) {
  return(invisible(NULL))
  # stop("An error occurred during processing. See error log for details.")
 }
 return(result)
}

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

#' Function to get rid of junk from column names
#'
#' @param col_names A vector of column names to clean.
#'
#' @return A vector of cleaned column names.
clean_column_names <- function(col_names) {
 sapply(col_names, function(name) {
  name <-
   gsub("[^[:alnum:]]", "", name) # Remove non-alphanumeric characters
  name <- gsub("nm", "", name) # Remove the "nm" suffix
  name
 })
}

#' Define ANSI escape codes for colors
red <- make_style("red")
black <- make_style("black")

#' Function to add messages to a list
add_message <- function(env, message, type = "info") {
 env$messages <- append(env$messages, list(list(message = message, type = type)))

}

# Function to generate project header
generate_project_header <- function(file_name, file_content) {

 # Header information
 project_name <- tools::toTitleCase(basename(getwd()))
 script_name <- "data_processing.R"
 software_version <- paste0("VegetationAnalysisToolkit ", as.character(utils::packageVersion("VegetationAnalysisToolkit")))
 generated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

 capture.output({
  cat("* Project:          ", project_name, "\n")
  cat("* File content:     ", file_content, "\n")
  cat("* File name:        ", file_name, "\n")
  cat("* Script name:      ", script_name, "\n")
  cat("* Software version: ", software_version, "\n")
  cat("* Generated at:     ", generated_at, "\n\n")
 })
}
