#' Validate ID/Unit Consistency in a Data Frame
#'
#' This function checks whether the `id` or `Units` column in a data frame contains values consistent with the
#' expected variable.
#'
#' @param current_df The data frame to be checked.
#' @param name A character string representing the name of the data frame (used in reporting issues).
#' @param variable The expected value for the `id` or `Units` column (e.g., "NDVI", "CCI").
#' @param file_name A character string representing the name of the source file (used in reporting issues).
#' @param local_issues A list of issues encountered during validation, to which new issues will be appended.
#'
#' @return A list containing any validation issues found, appended to `local_issues`.
#'         If no issues are found, `local_issues` is returned unchanged.
#'
#' @noRd
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
