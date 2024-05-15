#'

#' Merge Data and Index Frames

#' This function merges data frames from csv_elements with their corresponding index frames from index_elements.
#' The matching is based on a common 'UniqueID' column present in both sets of data frames. It is designed to work
#' with data frames named such that the base name of a CSV file matches the name of its corresponding index frame.
#'
#' @param csv_elements A named list of data frames originating from CSV files.
#' @param index_elements A named list of index data frames.
#'
#' @return A list of merged data frames, named by the base name of the original CSV files, without the .CSV extension.
merge_data_and_index_frames <- function(csv_elements, index_elements) {

  # Initialize an empty list to store the results of merging data frames.
  merged_data_frames <- list()

  # Iterate over csv_elements
  for (csv_name in names(csv_elements)) {
    # Extract the base name by removing the .CSV extension, used for matching with index names.
    base_name <- gsub("\\.CSV$", "", csv_name)

    # Check if there's a corresponding index frame for the current CSV file.
    if(base_name %in% names(index_elements)) {
      # Retrieve the data frame and its corresponding index frame.
      data_df <- csv_elements[[csv_name]]
      index_df <- index_elements[[base_name]]

      # Merge by 'UniqueID' if it exists in both data frames
      if("UniqueID" %in% colnames(data_df) && "UniqueID" %in% colnames(index_df)) {
        merged_df <- merge(data_df, index_df, by = "UniqueID", all = TRUE)
        merged_data_frames[[base_name]] <- merged_df
      } else {
        # Issue a warning if 'UniqueID' is missing in either the data or index frame.
        warning(paste("Missing 'UniqueID' in either data or index data frame for base name:", base_name))
      }
    } else {
      # Issue a warning if no corresponding index frame is found for the base name.
      warning(paste("Base name not found in index files:", base_name))
    }
  }

  return(merged_data_frames)
}

#' Apply Transformations to Merged Data Frames

#' @param merged_data_frames A list of data frames that have been merged with their corresponding index data.
#'
#' @return A list of transformed data frames, with the same names as the input merged data frames.
#'
#' @details The function iterates over each data frame in the input list, performing the following steps:
#'          1. Filters the data frame to retain only specified columns.
#'          2. Adds a date column derived from the data frame's name.
#'          3. Renames the columns based on predefined new names.
#'          4. Converts data types of the columns to ensure consistency.
#'          5. Optionally splits one of the columns into multiple columns for detailed categorization.
apply_transformations <- function(merged_data_frames) {

  # Iterate over each merged data frame to apply transformations
  transformed_data_frames <- lapply(names(merged_data_frames), function(name) {
    df <- merged_data_frames[[name]]
    columns_to_keep <- c("index", "Reading", "Position") # Define columns to keep from the merged data frame

    # Check if all required columns are present in the data frame
    if(all(columns_to_keep %in% names(df))) {
      # Keep only specified columns. This ensures that the number of columns in 'df_filtered'
      # will always match the number of 'new_names' defined later for renaming.
      df_filtered <- df[columns_to_keep] # Keep only specified columns

      # Extract date string from name, assuming name contains numeric date info
      date_str <- gsub("[^0-9]", "", name)

      # Define new names for the columns
      new_names <- c("Index", "CCI", "Code", "Date")

      # Sequentially apply transformations to the filtered data frame
      # Transformations applied in sequence:
      # 1. Add a date column derived from the base name.
      # 2. Rename columns according to 'new_names'.
      # 3. Convert data types, if necessary.
      # 4. Optionally, split columns for further detailed categorization.

      df_filtered <- addDateColumn(df_filtered, date_str) %>% # We add date columns to work!
        renameColumnsByPosition(new_names) %>%  # Rename the selected columns!
        convertData %>% # Change data type: [Sample NDVI Code Date -> numeric-numeric-character-date] - supresswarnings!
        splitCodeColumn # Apply splitCodeColumn to get Landuse, Position and InterRow columns!

      return(df_filtered) # Return the transformed dataframe
    } else {
      # If the data frame does not contain all required columns, halt execution.
      # This check is crucial for ensuring that subsequent transformations have a valid starting point.
      stop("Data frame does not contain all required columns: ", paste(columns_to_keep, collapse=", "))
    }
  })

  # Re-assign names to keep track of which transformed data frame corresponds to which original name
  names(transformed_data_frames) <- names(merged_data_frames) # maybe it is totally redundant, if so, skip it!

  return(transformed_data_frames)
}

#' Adds a date column to a data frame based on a date string extracted from file names.

#' @param df A data frame to which the date column will be added.
#' @param date_str A string representing a date, extracted from the names of data files.
#'        The function expects this string to be in one of two formats:
#'        - 'yymmddhh' (year, month, day, hour)
#'        - 'yymmdd' (year, month, day)
#'        The function attempts to detect the format automatically based on the string length
#'        and pattern.
#'
#' @return The input data frame with an added 'Date' column. If the date string matches
#'         one of the expected formats, this column will contain date values corresponding
#'         to the string. If the format is unrecognized, the column will be filled with NA,
#'         and a warning will be issued.
addDateColumn <- function(df, date_str) { # date_str will come from the index files!

  # Check for 'yymmddhh' format, indicating a date with year, month, day, and hour.
  if (grepl("\\d{6}\\d{2}", date_str)) { # e.g. "23081709"
    # If the string matches, extract and convert it to a date, ignoring the hour.
    df$Date <- as.Date(substr(date_str, 1, 6), format = "%y%m%d")

    # Check for 'yymmdd' format, indicating a date with year, month, and day.
  } else if (grepl("\\d{6}", date_str)) { # e.g. "230817" or plantpen_NDVI_230620
    # If the string matches, convert it directly to a date.
    df$Date <- as.Date(date_str, format = "%y%m%d")

  } else {
    # Handle other cases or provide a default value
    df$Date <- NA
    warning("Unable to assign dates to all data entries! Ensure your input file names conform to the expected formats and try to run the code again!\n")
  }

  return(df)
}

#' Renames columns of a data frame based on the provided list of new names.

#' @param df A data frame whose columns are to be renamed.
#' @param new_names A character vector of new names for the columns of the data frame.
#'                  The length of this vector must match the number of columns in the data frame.
#'
#' @return The data frame with its columns renamed if the lengths match,
#'         otherwise returns NULL as an indication of a mismatch.
renameColumnsByPosition <- function(df, new_names) {

  # Calculates the number of columns in the input data frame
  num_columns <- length(df)

  # Check if the number of columns matches the length of the new names provided.
  if (num_columns == length(new_names)) {
    # If true, rename the columns of the data frame with the new names.
    names(df)[1:length(new_names)] <- new_names
  } else {

    # If the numbers do not match, it indicates a potential error in input.
    # Return NULL to indicate the mismatch and stop the function execution.
    # This is a safeguard to prevent incorrect data manipulation.
    # [Though it is a little bit of a redundant step as the length of "num_columns"
    # will always be equal with the length of "new_names"!]

    return(NULL)
  }

  return(df)
}

#' Converts the data types of specific columns in a data frame.

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
convertData <- function(df) {

  # Convert the first two columns to numeric, handling European-style decimal separators.
  df[, c(1, 2)] <- lapply(df[, c(1, 2)], function(x) {
    as.numeric(gsub(",", ".", x))
  })

  # Convert the third column to character data type.
  df[, 3] <- as.character(df[, 3])

  # Convert the fourth column to Date format, using error handling to manage conversion failures.
  df[, 4] <- tryCatch(
    as.Date(df[, 4], format = "%Y-%m-%d"),
    error = function(e) {
      # Handle the error here, e.g., replace with NAs
    }
  )

  return(df)
}

#' Splits the "Code" column into three new columns based on specific patterns.

#' @param df A data frame with a column named "Code" that contains strings to be split.
#'
#' @return A data frame with three new columns ("Position", "Landuse", "InterRow") extracted from the "Code" column.
#'         If a pattern does not match exactly, the resulting column value will be NA.
#'
#' @details This function is designed to accurately parse and categorize components of a code string
#'          into distinct categories based on predefined patterns. The function prioritizes exact matches
#'          to ensure that only fully matching codes are extracted. In cases where a code component is
#'          expected but not present in the exact form (e.g., "GERGELY1" should not match any pattern),
#'          the function will not extract a partial match, aiming to maintain the integrity of the categorized data.
#'
#' @importFrom stringr str_extract
splitCodeColumn <- function(df) {

  df <- df %>%
    mutate(
      Position = str_extract(Code, "F|A|HP|CS|U|2|3|4"),
      Landuse = str_extract(Code, "CSU|CSR|CST|SZ|E|R|Ts1|T1|Cc1|Ts4|T2_new"),
      InterRow = ifelse(
        grepl("CSU|CST|CSR", Code) & !grepl("SK", Code),
        "S",
        str_extract(Code, "SK")
      )
      # Conditional columns!
      # e.g.: CSUFSK -> InterRow: SK | CSUF -> InterRow: S | CSUFS -> InterRow: S | CSUF{whatever} -> InterRow: S
      # <=> in the interrow column, everything is coded as "S" (=Sor) what is not wrote as "SK" (=Sorkoz) in the excel sheets code columns!
    )

  return(df)
}
