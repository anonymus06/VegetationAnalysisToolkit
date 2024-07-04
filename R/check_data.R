#' Diagnostic tool for data validation and checks.


#' Comprehensive Data Check and Issue Logging Function
#'
#' @param df A list containing data frames to be checked.
#' @param variable The name of the variable being checked, used to tailor the checks for specific data types.
#'
#' @details The function operates in several steps:
#'          1. It applies two sets of checks: `check_df1` for general data frame issues and `check_df2` for issues
#'             in data frames excluding index data.
#'          2. Issues are collected and aggregated across all data frames.
#'          3. The function returns a list indicating whether the data passed the checks without issues and the collected validation warnings.
#'
#'          This process ensures a comprehensive review of the data, aiding in the identification and correction
#'          of potential data quality issues before further analysis.
#'
#' @return A list containing:
#'         - `is_valid`: Logical value indicating whether the data passed the checks without issues (TRUE) or if issues were found (FALSE).
#'         - `validation_warnings`: A list of warnings encountered during validation.
check_data <- function(df, data_type, data_frame_files, variable) {

 # ---- initial part of procedure ----

 # Define expected structures based on the data type
 if (data_type == "NDVI") {
  data_columns <-
   c("index", "time", "id", "X760", "X635", "NDVI")
  index_columns <- c("index", "description")
  data_types <-
   c("integer", # integer
     "character",
     "character",
     "integer", # integer
     "integer", # integer
     "numeric") # numeric
  index_types <- c("numeric", "character")
  # variable <- "NDVI"

 } else if (data_type == "Chlorophyll") {
  data_columns <-
   c("Sample",
     "Time/Date",
     "Units",
     "Reading",
     "Lat",
     "Lon",
     "DOP",
     "# Sat")
  index_columns <- c("index", "Position")
  data_types <- c("character")
  index_types <- c("numeric", "character")
  # variable <- "CCI"

 } else {
  stop("Unknown data type in 'check_data'")
 }

 issues <- list() # Initialize an empty list to collect potential issues
 #non_index_dfs <- df[sapply(df, function(x) !is.null(x) && ncol(x) != 2)]  # Get df data frames without the index files

 index_files <- sapply(data_frame_files, is_index_file)
 # non_index_dfs <- names(df)[!index_files]
 non_index_dfs <- df[!index_files]

 # index_files <- sapply(names(df), is_index_file, data_frame_files)
 # non_index_dfs <- df[!index_files]

 result <- list(is_valid = TRUE, validation_warnings = list())

 # ---- main part of procedure ----

 # Perform all checks and aggregate issues
 # issues <- c(issues, perform_all_checks(df, non_index_dfs, data_columns, index_columns, data_types,
 #                                                    index_types, variable))
 issues <- perform_all_checks(df, non_index_dfs, data_columns, index_columns, data_types, data_type,
                              index_types, variable, data_frame_files)


 if (length(issues) > 0) {
  result$is_valid <- FALSE
  result$validation_warnings <- issues
 }


 return(result)
}

# Define a function to perform content consistency and integrity checks
perform_non_index_checks <- function(df, name, variable, data_type, data_frame_files, local_issues) {

 current_df <- df[[name]]

 # Get the file name from the data_frame_files list
 file_name <- data_frame_files[[name]]

 # Process the DataFrame
 result <- process_dataframe(as.data.frame(current_df)) # as.data.frame redundant? # todo: ncol(df) == 1 -> chlorofily -> more robust solution, like data_type variable use to avoid errors when it wants to split a column when it is actually is a bad ndvi file
 current_df <- result$current_df
 current_df_old <- result$current_df_old

 # Content Consistency and Integrity Checks
 # local_issues <- c(local_issues, check_index_sequence(current_df, current_df_old, name, local_issues))
 local_issues <- check_index_sequence(current_df, name, data_type, file_name, local_issues)
 local_issues <- check_datetime_format(current_df, name, file_name, local_issues)
 local_issues <- check_id_unit_consistency(current_df, name, variable, file_name, local_issues)

 # local_issues <- c(local_issues, check_datetime_format(current_df, name, local_issues))
 # local_issues <- c(local_issues, check_id_unit_consistency(current_df, name, variable, local_issues))
 # local_issues <- c(local_issues, check_missing_values(current_df, name, local_issues))
 # local_issues <- c(local_issues, check_duplicate_rows(current_df, name, local_issues))

 return(local_issues)
}

perform_global_checks <- function(df, name, data_columns, index_columns, data_type, data_types, index_types, data_frame_files, local_issues){

 current_df <- df[[name]]

 # Get the file name from the data_frame_files list
 file_name <- data_frame_files[[name]]

 # Structure and Data Type Checks
 local_issues <- check_data_existence(
  current_df,
  name,
  data_columns,
  file_name,
  local_issues
 )

 local_issues <- check_column_names(current_df, name, data_type, data_columns, index_columns, file_name, local_issues)
 local_issues <- check_data_types(current_df, name, data_types, index_types, file_name, local_issues)
 local_issues <- check_missing_values(current_df, name, file_name, local_issues)
 local_issues <- check_duplicate_rows(current_df, name, file_name, local_issues)

 # local_issues <-
 #  c(
 #   local_issues,
 #   check_data_existence(
 #    current_df,
 #    name,
 #    data_columns,
 #    index_columns,
 #    data_types,
 #    index_types,
 #    local_issues
 #   )
 #  )
 # local_issues <-
 #  c(local_issues,
 #    check_column_names(current_df, name, data_columns, index_columns, local_issues))
 # local_issues <-
 #  c(local_issues,
 #    check_data_types(current_df, name, data_types, index_types, local_issues))

 return(local_issues)
}

# Function to perform all checks
perform_all_checks <- function(df, non_index_dfs, data_columns, index_columns, data_types, data_type, index_types, variable, data_frame_files) {
 local_issues <- list()

 # Structure and Data Type Checks
 structure_issues <- lapply(names(df), function(name) {
  perform_global_checks(df, name, data_columns, index_columns, data_type, data_types, index_types, data_frame_files, list())
 })

 # Flatten the list of issues
 # structure_issues <- unlist(structure_issues, recursive = FALSE)
 structure_issues <- do.call(c, structure_issues)
 if(length(structure_issues) > 0) {
  local_issues <- c(local_issues, structure_issues)
 }

 # Content Consistency and Integrity Checks
 content_issues <- lapply(names(non_index_dfs), function(name) {
  # perform_non_index_checks(non_index_dfs, name, variable, data_type, data_frame_files, list())
  perform_non_index_checks(non_index_dfs, name, variable, data_type, data_frame_files, local_issues)
 })

 # Flatten the list of issues
 # content_issues <- unlist(content_issues, recursive = FALSE)
 content_issues <- do.call(c, content_issues)
 if(length(content_issues) > 0) {
  local_issues <- c(local_issues, content_issues)
 }

 return(local_issues)
}


# Structure and Data Type Checks

check_data_existence <-
 function(current_df,
          name,
          data_columns,
          file_name,
          local_issues) {

  # First, check if the data frame itself is NULL
  # if (is.null(current_df)) {
  #  local_issues <-
  #   add_issue(local_issues,
  #             name,
  #             paste0("The data frame '", name, "' does not exist or is NULL. Source file: '", file_name, "'"))
  #  return(local_issues)  # Return the list of issues even when exiting early
  # }

  # Check if the data frame is empty
  if (nrow(current_df) == 0) {
   local_issues <-
    add_issue(local_issues, name, paste0("The data frame '", name, "' is empty. Source file: '", file_name, "'"))
   return(local_issues)
  }

  # Check the number of columns against expected structures
  if (ncol(current_df) == length(data_columns)) {
   # Handle NDVI data
  } else if (ncol(current_df) == 2) {
   # Handle Index file
  } else {
   local_issues <- add_issue(
    local_issues,
    name,
    paste0(
     "The data frame '",
     name,
     "' does not match any expected number of columns. ",
     "Detected dimensions: ",
     ncol(current_df),
     " columns. ",
     "Expected dimensions for data file: ",
     length(data_columns), ". ",
     "Expected dimensions for Index file: ",
     2, ". Source file: '", file_name, "'"
    )
   )
   return(local_issues)
  }

  return(local_issues)

 }

check_column_names <-
 function(current_df,
          name,
          data_type, # todo: TÖBBI HELYEN IS HOZZÁÍRNI H ÚJ VARIABLE HERE!
          data_columns,
          index_columns,
          file_name,
          local_issues) {

  mapped_names <- clean_column_names(names(current_df))
  # cat(mapped_names)

  # Define the new expected columns string if data_columns starts with "index"
  if (data_type == "NDVI") {
   expected_data_columns <- c("index", "time", "id", "760 [nm]", "635 [nm]", "NDVI")
   expected_index_columns <- c("index", "description")
  } else if (data_type == "Chlorofil"){ #todo: Chlorofil - jól írtam?
   expected_data_columns <- c("Sample") # todo: befejezni a colnames-t!
   expected_index_columns <- c("index", "Position") # todo: description legyen v Position?
  }

  # Check against the first set of expected columns
  if (all(data_columns %in% mapped_names)) {
   # If the data columns match, no issue is added
  } else if (all(index_columns %in% mapped_names)) {
   # If the index columns match, no issue is added
  } else {
   # If neither set of columns match, add an issue
   local_issues <- add_issue(
    local_issues,
    name,
    paste0(
     "Your data frame '", name, "' does not match the expected column names! ",
     # "Detected columns: '", paste0(mapped_names, collapse = ", "), "'. ", # it can be misleading for the user as it gives X760, X635 for the normally formatted 760 [nm], 635 [nm] colnames, so for better understanding, i leave it out!
     "Expected columns for data file: '", paste0(expected_data_columns, collapse = ", "), "'. ",
     "Expected columns for Index file: '", paste0(index_columns, collapse = ", "), "'. Source file: '", file_name, "'"
    )
   )

  }
  return(local_issues)
 }

check_data_types <- function(current_df, name, data_types, index_types, file_name, local_issues) {
 actual_types <- sapply(current_df, class) %>% unname()

 if (all(actual_types == "character")) {
  local_issues <- add_issue(
   local_issues,
   name,
   paste0( # e.g. adding new columns to it can disrupt its original data types
    "All columns in '", name, "' are read as character type. ",
    "This may indicate an issue with the data file format. ",
    "If you modified the input file prior to processing, it is possible that this caused the issue. ",
    "Please use the original, unmodified file and try again. Source file: '", file_name, "'"
   )
  )
 } else if (all(actual_types == data_types)) {
  # Data types match the expected data types for the main data file
 } else if (all(actual_types == index_types)) {
  # Data types match the expected data types for the index file
 } else {
  local_issues <- add_issue(
   local_issues,
   name,
   paste0(
    "Data types in '", name, "' data frame do not match! ",
    "Expected data types for data file: '", paste0(data_types, collapse = ", "), "'. ",
    "Expected data types for Index file: '", paste0(index_types, collapse = ", "), "'. ",
    "Found: '", paste0(actual_types, collapse = ", "), "'. Source file: '", file_name, "'"
   )
  )
 }

 return(local_issues)
}



# Content Consistency and Integrity Checks

check_index_sequence <- function(current_df, name, data_type, file_name, local_issues) {

 # Verify the sequentiality of 'index' or 'Sample' columns, depending on data type
 # Since index appears to be a sequential identifier, we should ensure it's continuous without unexpected gaps!

 if (data_type == "Chlorophyll") {

  if (!check_sequential_index(current_df_old)) {

   # Check if values are NOT sequential
   local_issues <-
    add_issue(
     local_issues,
     name,
     paste0(
      "Non-sequential values detected in column 'Sample', dataset: '",
      name,
      "'. Source file: '", file_name, "'"
     )
    )
  }

 } else if (data_type == "NDVI") {


  if (any(diff(sort(as.numeric(current_df[["index"]]))) != 1)) {
   # NDVI check for non-sequential index
   local_issues <-
    add_issue(
     local_issues,
     name,
     paste0(
      "Non-sequential values detected in column 'index', dataset: '",
      name,
      "' . Source file: '", file_name, "'"
     )
    )
  }
 }

 return(local_issues)

}

check_datetime_format <- function(current_df, name, file_name, local_issues) {
 # Split datetime strings into separate date and time components
 split_datetime <-
  strsplit(as.character(current_df[, 2]), " ")  # Assuming the datetime string is in the 2nd column of 'df'
 # Extract time and date parts for validation
 time_part <- sapply(split_datetime, function(x)
  x[1])

 if (names(current_df)[1] == "Sample") {
  # chlorophyl
  date_part <- sapply(split_datetime, function(x)
   x[2])
  combined_datetime <-
   paste(date_part, time_part) # Combining date and time
  posix_time <-
   as.POSIXct(combined_datetime, format = "%m/%d/%Y %H:%M:%S")

 } else if (names(current_df)[1] == "index") {
  # ndvi
  date_part <-
   sapply(split_datetime, function(x) {
    # Assuming date is the last part
    if (length(x) >= 3 && nchar(x[3]) > 0) {
     return(x[3])  # Return the third element if it's not empty
    } else {
     return(x[which(nchar(x) > 0)[1]]) # Otherwise, return the first non-empty element
    }
   })

  combined_datetime <-
   paste(date_part, time_part) # Combining date and time
  posix_time <- as.POSIXct(combined_datetime, format = "%d.%m.%Y %H:%M:%S")
 }

 # Checking for incorrect formats by identifying NA values in the conversion result
 if (any(is.na(posix_time))) {
  local_issues <-
   add_issue(
    local_issues,
    name,
    paste0("Some entries in the Time/Date column have an incorrect format. Expected formats: 'HH:MM:SS DD.MM.YYYY' [NDVI] or 'HH:MM:SS MM/DD/YYYY' [Chlorophyl]. Source file: '", file_name, "'"
   ))
 }
 return(local_issues)


}

check_id_unit_consistency <- function(current_df, name, variable, file_name, local_issues) {
 if (names(current_df)[1] == "index" &&
     (any(current_df$id != variable))) {
  local_issues <-
   add_issue(local_issues,
             name,
             paste0("ID column contains values other than '", variable, "'. Source file: '", file_name, "'")
   )
 } else if (names(current_df)[1] == "Sample" &&
            (any(current_df$Units != variable))) {
  local_issues <-
   add_issue(local_issues,
             name,
             paste0("Units column contains values other than '", variable, "'. Source file: '", file_name, "'")
   )
 }
 return(local_issues)

}

check_missing_values <- function(current_df, name, file_name, local_issues) {
 # Convert empty strings to NA
 current_df[current_df == ""] <- NA

 # Check for missing values
 if (any(colSums(is.na(current_df)) > 0)) {
  local_issues <-
   add_issue(local_issues,
             name,
             paste0("Missing values found in one or more columns. Source file: '", file_name, "'"))
 }
 return(local_issues)

}

check_duplicate_rows <- function(current_df, name, file_name, local_issues) {
 if (any(duplicated(current_df))) {
  local_issues <-
   add_issue(local_issues, name, paste0("Duplicate rows found in the data. Source file: '", file_name, "'"))
 }
 return(local_issues)

}
