#' Merge Data and Index Frames

#' This function merges data frames from csv_elements with their corresponding index frames from index_elements.
#'
#' @param csv_elements A named list of data frames originating from CSV files.
#' @param index_elements A named list of index data frames.
#'
#' @return A list of merged data frames, named by the base name of the original CSV files, without the .CSV extension.
#' @noRd
merge_data_and_index_frames <- function(csv_elements, index_elements) {
  merged_data_frames <- list()

  for (csv_name in names(csv_elements)) {
    base_name <- gsub("\\.CSV$", "", csv_name)

    if (base_name %in% names(index_elements)) {
      data_df <- csv_elements[[csv_name]]
      index_df <- index_elements[[base_name]]

      if ("UniqueID" %in% colnames(data_df) && "UniqueID" %in% colnames(index_df)) {
        merged_df <- merge(data_df, index_df, by = "UniqueID", all = TRUE)
        merged_data_frames[[base_name]] <- merged_df
      } else {
        warning(paste("Missing 'UniqueID' in either data or index data frame for:", base_name))
      }
    } else {
      warning(paste("No matching index file found for base name:", base_name))
    }
  }

  return(merged_data_frames)
}
