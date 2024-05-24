#'

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
check_data <- function(df, variable) {

 issues <- list()  # Initialize an empty list to collect potential issues
 df_null <- df[sapply(df, function(x) !is.null(x) && ncol(x) != 2)]  # Get df data frames without the index files

 # Perform checks with tryCatch
 issues_df1 <- lapply(names(df), function(name) {
  tryCatch(
   check_df1(df[[name]], name),
   warning = function(w) {
    msg <- paste("Warning in check_df1 for", name, ":", conditionMessage(w))
    log_general_issue(msg)
    warning(msg)
    NULL
   },
   error = function(e) {
    msg <- paste("Error in check_df1 for", name, ":", conditionMessage(e))
    log_general_issue(msg)
    message(msg)
    NULL
   }
  )
 }) %>% Filter(function(x) !is.null(x) && length(x) > 0, .)

 issues_df2 <- lapply(names(df_null), function(name) {
  tryCatch(
   check_df2(df_null[[name]], name, variable),
   warning = function(w) {
    msg <- paste("Warning in check_df2 for", name, ":", conditionMessage(w))
    log_general_issue(msg)
    warning(msg)
    NULL
   },
   error = function(e) {
    msg <- paste("Error in check_df2 for", name, ":", conditionMessage(e))
    log_general_issue(msg)
    message(msg)
    NULL
   }
  )
 }) %>% Filter(function(x) !is.null(x) && length(x) > 0, .)

 # Aggregate Issues
 if (length(issues_df1) > 0) {
  for (i in seq_along(issues_df1)) {
   df_issues <- issues_df1[[i]]
   for (name in names(df_issues)) {
    issues[[name]] <- c(issues[[name]], df_issues[[name]])
   }
  }
 }
 if (length(issues_df2) > 0) {
  for (i in seq_along(issues_df2)) {
   df_issues <- issues_df2[[i]]
   for (name in names(df_issues)) {
    issues[[name]] <- c(issues[[name]], df_issues[[name]])
   }
  }
 }
 validation_warnings <- list()
 if (length(issues) > 0) {
  for (name in names(issues)) {
   issue_details <- paste(issues[[name]], collapse = "\n")
   warning_message <- paste0("Issues in ", name, ":\n", issue_details)
   validation_warnings <- c(validation_warnings, warning_message)
   #warning(warning_message)
  }
 }

 return(list(is_valid = (length(issues) == 0),
             validation_warnings = validation_warnings))
}

#' Data Frame Structure and Content Validation Function

#' @param current_df The data frame to be checked.
#' @param name The name of the data frame, used for identifying it in the issue log.
#'
#' @return A list of identified issues with the data frame structure and content.
#'
#' @details The function first checks if the data frame exists and is not empty.
#'          It then compares the data frame's structure against expected structures for different data types.
#'          This includes checking the number of columns, the names of the columns, and the data types of columns.
#'          Identified issues are added to a list which is returned by the function.
check_df1 <- function(current_df, name) {

  # ---- initial part of procedure ----

  # Initial checks and setup
  expected_columns1 <- c("index", "time", "id", "760", "635", "NDVI") # Expected columns for NDVI data
  expected_columns2 <- c("Sample,Time/Date,Units,Reading,Lat,Lon,DOP,# Sat") # Expected for Chlorophyll data
  expected_columns3 <- c("index", "Position") # Expected for index data
  expected_types1 <- c("character") # Expected data types for Chlorophyll data
  expected_types2 <- c("integer", "character", "character", "integer", "integer", "numeric") # For NDVI data
  expected_types3 <- c("numeric", "character") # For index data
  local_issues <- list() # To store issues found


  # ---- main part of procedure ----

  # [0] Preliminary checks for data existence and structure:
  # First, check if the data frame itself is NULL
  if (is.null(current_df)) {
    local_issues <- add_issue(local_issues, name, paste("The data frame", name, "does not exist or is NULL."))
    return(local_issues)  # Return the list of issues even when exiting early
  }
  # Check if the data frame is empty
  if (nrow(current_df) == 0) {
    local_issues <- add_issue(local_issues, name, paste("The data frame", name, "is empty."))
    return(local_issues)
  }
  # Check the number of columns against expected structures
  if (ncol(current_df) == 1) {
    # Handle Chlorophyll data
  } else if (ncol(current_df) == 2) {
    # Handle Index file
  } else if (ncol(current_df) == 6) { # Adjust according to your needs
    # Handle NDVI data
  } else {
    local_issues <- add_issue(local_issues, name,
                              paste0("The data frame '", name, "' does not match any expected column structure.",
                                     "\nDetected dimensions: ", ncol(current_df), " columns.",
                                     "\nExpected dimensions for NDVI data: 6 columns.",
                                     "\nExpected dimensions for Chlorophyll data: 1 column.",
                                     "\nExpected dimensions for Index file: 2 columns."))
    return(local_issues)
  }

  # [1] Column name checks:
  mapped_names <- names(current_df)   # Extract column names
  # Check against the first set of expected columns
  if (!all(expected_columns1 %in% mapped_names)) {
    # If the first check fails, try the second set of expected columns
    if (!all(expected_columns2 %in% mapped_names)) {
      # ...
      if (!all(expected_columns3 %in% mapped_names)){
        local_issues <- add_issue(local_issues, name,
                                  paste0("Your data frame '", name, "' does not match the expected column names!",
                                         "\nDetected columns: '", mapped_names, "'",
                                         "\nExpected columns for NDVI data: '", paste0(expected_columns1, collapse=", "), "'",
                                         "\nExpected columns for Chlorophyll data: '", paste0(expected_columns2, collapse=", "), "'",
                                         "\nExpected columns for Index file: '", paste0(expected_columns3, collapse=", "), "'",
                                         sep="\n"))
      }
    }
  }

  # [2] Data type checks:
  actual_types <- sapply(current_df, class) %>% unname()
  if (!all(actual_types == expected_types1)) {
    if (!all(actual_types == expected_types2)) {
      if (!all(actual_types == expected_types3)) {
        local_issues <- add_issue(local_issues, name,
                                  paste0("Data types in '", name, "' data frame do not match!",
                                         "\nExpected data types for NDVI data: '", paste0(expected_types1, collapse=", "), "'",

                                         "\nExpected data types for Chlorophyll data: '", paste0(expected_types2, collapse=", "), "'",

                                         "\nExpected data types for Index file: '", paste0(expected_types3, collapse=", "), "'",
                                         "\nFound: ", paste0(actual_types, collapse=", ")
                                  ))

      }}}


  return(local_issues)
}

#' Performs Consistency and Data Integrity Checks on a Data Frame

#' @param current_df The data frame to be validated.
#' @param name A string representing the name of the data frame, used for identification in issue logging.
#' @param variable A string indicating the type of data being checked, such as 'NDVI' or 'CCI'.
#'
#' @return A list of identified issues, if any, within the data frame.
#'
#' @details The function checks for:
#' - Sequentiality in the 'index' or 'Sample' column, depending on the data type.
#' - Correct time and date format in datetime columns.
#' - Consistency in the 'id' or 'Units' column with the expected variable type.
#' - Presence of missing values in any of the columns.
#' - Duplicate rows within the data frame.
#'
#' It employs a helper function to filter out rows that might contain column names
#' instead of actual data, ensuring that checks are performed on valid data entries only.
#' Additionally, it dynamically adjusts checks based on the column structure, catering
#' specifically to the requirements of NDVI and Chlorophyll data.
#'
#' @note This function is part of a larger data validation process and assumes that
#' the data frame has already undergone preliminary checks for existence and basic structure.
#' It is designed to be flexible and can be adapted or extended to include additional
#' checks as needed.
check_df2 <- function(current_df, name, variable) {

  # ---- initial part of procedure ----

  #Filtering out rows that might inadvertently contain column names as data
  filter_rows_not_in_columns <- function(df, required_columns) {
    rows_to_keep <- apply(df, 1, function(row) !all(row %in% required_columns))
    filtered_df <- df[rows_to_keep, ]
    return(filtered_df)
  }

  # Split the columns into a 1 element/1 cell format!
  # Only call the function if there is one column in the data frame! - marking chlorophyl data!
  if (ncol(current_df) == 1){current_df <- splitColumns(current_df)}

  # Filter out rows that match column names and save an old copy for sequential index check
  current_df_old <- current_df
  current_df <- filter_rows_not_in_columns(current_df, names(current_df))

  local_issues <- list()    # Initialize a list to hold any issues found during checks


  # ---- main part of procedure ----

  # [1] Consistency Checks:
  # Verify the sequentiality of 'index' or 'Sample' columns, depending on data type
  # Since index appears to be a sequential identifier, we should ensure it's continuous without unexpected gaps!
  if (names(current_df)[1] == "Sample") { # For chlorophyll data
    if (!check_sequential_index(current_df_old)) {  # Check if values are NOT sequential
      local_issues <- add_issue(local_issues, name, paste0("Non-sequential values detected in column 'Sample', dataset: '", name,"'."))
    }
  } else if (any(diff(sort(current_df[["index"]])) != 1)) {  # NDVI check for non-sequential index
    local_issues <- add_issue(local_issues, name, paste0("Non-sequential values detected in column 'index', dataset: '",name,"' ."))
  }

  # [2] Validate 'time' format by ensuring it matches expected patterns:
  # Split datetime strings into separate date and time components
  split_datetime <- strsplit(as.character(current_df[,2]), " ")  # Assuming the datetime string is in the 2nd column of 'df'
  # Extract time and date parts for validation
  time_part <- sapply(split_datetime, function(x) x[1])
  if (names(current_df)[1] == "Sample") { # chlorophyl
    date_part <- sapply(split_datetime, function(x) x[2])
    combined_datetime <- paste(date_part, time_part) # Combining date and time
    posix_time <- as.POSIXct(combined_datetime, format ="%m/%d/%Y %H:%M:%S")
  } else if (names(current_df)[1] == "index") { # ndvi
    date_part <- sapply(split_datetime, function(x) { # Assuming date is the last part
      if (length(x) >= 3 && nchar(x[3]) > 0) {
        return(x[3])  # Return the third element if it's not empty
      } else {
        return(x[which(nchar(x) > 0)[1]]) # Otherwise, return the first non-empty element
      }
    })
    combined_datetime <- paste(date_part, time_part) # Combining date and time
    posix_time <- as.POSIXct(combined_datetime, format="%d.%m.%Y %H:%M:%S")
  }
  # Checking for incorrect formats by identifying NA values in the conversion result
  if (any(is.na(posix_time))) {
    local_issues <- add_issue(local_issues, name, "Some entries in the Time/Date column have an incorrect format. Expected formats: 'HH:MM:SS DD.MM.YYYY' [NDVI] or 'HH:MM:SS MM/DD/YYYY' [Chlorophyl].")
  }

  # [3] Ensure consistency in 'id' or 'Units' columns with the expected data type:
  if (names(current_df)[1] == "index" && (any(current_df$id != variable))) {
    local_issues <- add_issue(local_issues, name, "ID column contains values other than 'NDVI'.")
  } else if (names(current_df)[1] == "Sample" && (any(current_df$Units != variable))) {
    local_issues <- add_issue(local_issues, name, "Units column contains values other than 'CCI'.")
  }

  # [4] Identify any missing values across all columns:
  if (any(colSums(is.na(current_df)) > 0)) {
    local_issues <- add_issue(local_issues, name, "Missing values found in one or more columns.")
  }

  # [5] Detect duplicate rows in the data frame:
  if (any(duplicated(current_df))) {
    local_issues <- add_issue(local_issues, name, "Duplicate rows found in the data.")
  }


  return(local_issues)  # Return a list of issues found, if any
}

#' Function to Verify Sequentiality in Chlorophyll Data Sample Column

#' @param df A dataframe containing chlorophyll data, including a 'Sample' column that needs verification.
#' @param verbose A logical parameter that, when set to TRUE, prints a message for each checked segment indicating
#'        successful verification of sequentiality.
#'
#' @details The function iterates through the dataframe, identifying segments of data delineated by rows
#'          with the value "Sample" in the 'Sample' column. Each segment is checked to ensure that the 'Sample'
#'          numbers are sequential (i.e., increase by exactly 1 between each row). The function can handle
#'          multiple segments within a single dataframe, making it suitable for datasets that include
#'          multiple measurement sessions concatenated into one file.
#'
#'          If any non-sequential 'Sample' values are found within a segment (excluding the "Sample" header rows),
#'          the function returns FALSE, indicating an issue with the sequentiality of sample numbering.
#'          Otherwise, it returns TRUE, confirming that all 'Sample' numbers within each segment are sequential.
#'
#'          This verification is crucial for ensuring data integrity, especially when analyzing time-series
#'          measurements or when the order of samples is significant for the analysis.
#'
#' @return A logical value: TRUE if the 'Sample' numbers are sequential in all segments, and FALSE otherwise.
#'
#' @note The function uses the 'Sample' column to identify new data segments and to check the sequentiality
#'       of sample numbers. It is designed to skip over and not count the header rows marked as "Sample".
#'       Ensure that your data is formatted correctly, with "Sample" used exclusively to denote headers
#'       for new segments.
#'
#'       The function suppresses warnings when converting 'Sample' values to numeric, as the presence of
#'       "Sample" headers will cause conversion warnings. This suppression ensures that the function focuses
#'       on checking sequentiality without being hindered by expected non-numeric values.
check_sequential_index <- function(df, verbose=FALSE) {

  # Identifying segments with "Sample" headers for separate sequentiality checks
  split_indices <- which(df$Sample == "Sample")
  # Append an additional index to include the last data segment
  split_indices <- c(split_indices, nrow(df) + 1)

  index0 <- 1
  for (i in seq_along(split_indices)) {
    index <- split_indices[i]
    # Adjust the segment range to handle the last chunk correctly
    if (i == length(split_indices)) {
      seq_df <- df[index0:(nrow(df)),]
    } else {
      seq_df <- df[index0:(index - 1),]
    }
    index0 <- index + 1 # Prepare for the next segment

    # Exclude "Sample" header rows and convert to numeric for sequentiality check
    numeric_samples <- suppressWarnings(as.numeric(seq_df$Sample[seq_df$Sample != "Sample"]))

    # Verify sequentiality within the segment
    if (any(diff(numeric_samples) != 1)) {
      if(verbose){ print(sprintf("Non-sequential values found in 'Sample' column within rows %d to %d.", index0, index - 1))}
      return(FALSE)
    }
    if(verbose){ print(sprintf("Checked sequentiality up to row %d: OK", index - 1))}
  }

  return(TRUE)  # Return TRUE if all segments pass the sequentiality check
}
