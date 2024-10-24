#' Check for Sequential Index Values in a Data Frame
#'
#' This function checks whether the index or sample column values are sequential and continuous
#' ,ensuring that there are no unexpected gaps.
#'
#' @param current_df The data frame to be checked for index sequentiality.
#' @param currect_df_old description
#' @param name A character string representing the name of the data frame (used in reporting issues).
#' @param device_type A character string specifying the type of device (e.g., "PPNP", "MC100").
#' @param file_name A character string representing the name of the source file (used in reporting issues).
#' @param local_issues A list of issues encountered during validation, to which new issues will be appended.
#'
#' @return A list containing any validation issues found, appended to `local_issues`. If the sequence is valid,
#'         `local_issues` is returned unchanged.
#'
#' @noRd
check_index_sequence <- function(current_df, current_df_old, name, device_type, file_name, local_issues) {
  if (device_type == "MC100") {
    if (!check_sequential_index(current_df_old)) {
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
  } else if (device_type == "PPNP") {
    if (any(diff(sort(as.numeric(current_df[["index"]]))) != 1)) {
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
