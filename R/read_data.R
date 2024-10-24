#' General Function to Read Data for Both PPNP and MC100 Devices
#'
#' This function reads data for either the PlantPen NDVI & PRI (PPNP) or the MC100 Chlorophyll Concentration Meter.
#' It processes text or CSV files and associated index files based on the device type.
#'
#' @param folder_pathway A character string specifying the path to the folder containing data and index files.
#' @param device_type A character string specifying the device type ("PPNP" for PlantPen or "MC100" for MC-100).
#' @param skip_rows An integer specifying the number of rows to skip when reading the data files. Default is 4.
#' @param valid_patterns A character vector of valid filename patterns to match. Default patterns are adjusted based on device type.
#' @param env An environment object that stores issues and messages generated during the process.
#'
#' @return A list containing:
#'         - `all_data`: A list of data frames containing data and index files, organized by file name or sheet name.
#'         - `data_frame_files`: A list mapping data frame names to their respective file paths.
#' @noRd
read_data <- function(folder_pathway, device_type, env,
                      skip_rows = 4,
                      valid_patterns = NULL) {
  if (is.null(valid_patterns)) {
    if (device_type == "PPNP") {
      valid_patterns <- c("(?i)plantpen_(NDVI|PRI)_\\d{6}\\.txt$", "(?i)index\\.xlsx$")
    } else if (device_type == "MC100") {
      valid_patterns <- c("(?i)^\\d{6}(\\d{2})?\\.csv$", "(?i)index\\.xlsx$")
    } else {
      stop("Invalid device_type provided")
    }
  }

  files <- list.files(folder_pathway, full.names = TRUE)

  all_data <- list()
  data_frame_files <- list()

  process_file <- function(file) {
    file_extension <- tools::file_ext(file)

    if (is_directory(file)) {
      log_skipped_file(file, "folder", env)
      return(NULL)
    }

    if (!check_naming_pattern(file, valid_patterns)) {
      log_issue(file, "Invalid naming pattern", env)
      return(NULL)
    }

    if (tolower(file_extension) == "xlsx" && !startsWith(basename(file), "~$")) {
      return(handle_excel_file(file))
    } else if (device_type == "PPNP" && tolower(file_extension) == "txt" && !startsWith(basename(file), "~$")) {
      return(handle_text_file(file, skip_rows, env))
    } else if (device_type == "MC100" && tolower(file_extension) == "csv" && !startsWith(basename(file), "~$")) {
      return(handle_CSV_file(file))
    } else if (startsWith(basename(file), "~$")) {
      log_skipped_file(file, "temporary file", env)
      return(NULL)
    } else {
      log_skipped_file(file, "element", env)
      return(NULL)
    }
  }

  results <- lapply(files, process_file)
  for (result in results) {
    if (!is.null(result)) {
      all_data <- c(all_data, result$data)
      data_frame_files <- c(data_frame_files, result$files)
    }
  }

  return(list(all_data = all_data, data_frame_files = data_frame_files))
}
