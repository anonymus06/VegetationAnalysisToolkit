#' Helper function to add identified issues to the list
#'
#' This helper function appends a new issue message to the appropriate category in a list of issues.
#' Each category is represented by a named vector within the list.
#'
#' @param issues A list containing named vectors of issue messages.
#' @param name A character string the category name under which the issue message should be added.
#' @param issue_message A character string, the message describing the issue.
#'
#' @return The updated list of issues with the new issue message appended.
#'
#' @examples
#' issues <- list()
#' issues <- add_issue(issues, "File Issues", "File format not recognized")
#' issues <- add_issue(issues, "Data Issues", "NA values detected in data")
#'
#' @noRd
add_issue <- function(issues, name, issue_message) {
 issues[[name]] <- c(issues[[name]], issue_message)
 return(issues)
}
