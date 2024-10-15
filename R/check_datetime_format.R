#' Check Date and Time Format in a Data Frame
#'
#' This function validates the format of the datetime strings in a data frame for both PlantPen and MC-100 data.
#' It splits the datetime string into date and time components, checks the format, and reports any incorrect entries.
#'
#' @param current_df The data frame to be checked for datetime format.
#' @param name A character string representing the name of the data frame (used in reporting issues).
#' @param file_name A character string representing the name of the source file (used in reporting issues).
#' @param local_issues A list of issues encountered during validation, to which new issues will be appended.
#'
#' @details
#' - For "MC-100" data, the function expects the datetime format to be "HH:MM:SS MM/DD/YYYY".
#' - For "PlantPen" data, the function expects the datetime format to be "HH:MM:SS DD.MM.YYYY".
#' - Any incorrect datetime formats are reported as an issue.
#'
#' @return A list containing any validation issues found, appended to `local_issues`.
#'         If no issues are found, `local_issues` is returned unchanged.
#'
#' @noRd
check_datetime_format <- function(current_df, name, file_name, local_issues) {
 split_datetime <- strsplit(as.character(current_df[, 2]), " ")
 time_part <- sapply(split_datetime, function(x) x[1])

 if (names(current_df)[1] == "Sample") {
  # For MC-100 data
  date_part <- sapply(split_datetime, function(x) x[2])
  combined_datetime <- paste(date_part, time_part)
  posix_time <- as.POSIXct(combined_datetime, format = "%m/%d/%Y %H:%M:%S")

 } else if (names(current_df)[1] == "index") {
  # For PlantPen data
  date_part <-
   sapply(split_datetime, function(x) {
    if (length(x) >= 3 && nchar(x[3]) > 0) {
     return(x[3])
    } else {
     return(x[which(nchar(x) > 0)[1]])
    }
   })
  combined_datetime <- paste(date_part, time_part)
  posix_time <- as.POSIXct(combined_datetime, format = "%d.%m.%Y %H:%M:%S")
 }

 if (any(is.na(posix_time))) {
  local_issues <-
   add_issue(
    local_issues,
    name,
    paste0("Some entries in the Time/Date column have an incorrect format. Expected formats:
           'HH:MM:SS DD.MM.YYYY' [PlantPen] or 'HH:MM:SS MM/DD/YYYY' [MC-100]. Source file: '",
           file_name, "'"
    ))
 }

 return(local_issues)
}
