#' Read Simple Configuration File
#'
#' This function reads a simple configuration file containing patterns for position, landuse,
#' and optional inter-row conditions. The configuration file is expected to have each line correspond
#' to a specific setting, which is used to configure pattern matching in other functions.
#'
#' @param file A character string representing the path to the configuration file.
#'
#' @details The configuration file should have the following structure:
#'          - Line 1: The regular expression for `positionPattern`.
#'          - Line 2: The regular expression for `landusePattern`.
#'          - Line 3: Patterns for inter-row conditions (separated by `|`), or "NA" if no inter-row conditions are present.
#'          - Line 4: The default value to use for the inter-row conditions.
#'          - Line 5: The pattern used for extracting inter-row conditions.
#'
#'          If "NA" is present on Line 3, the function will set `interRowConditions` to `NULL`.
#'
#' @return A list containing the patterns and conditions parsed from the configuration file:
#'         - `positionPattern`: A regular expression pattern for matching positions.
#'         - `landusePattern`: A regular expression pattern for matching landuse.
#'         - `interRowConditions`: A list of conditions for extracting inter-row patterns.
#'         This list will contain patterns for matching, a default value, and an extraction pattern if provided.
#'
#' @noRd
read_simple_config <- function(file) {
 lines <- readLines(file)
 config <- list(
  positionPattern = lines[1],
  landusePattern = lines[2]
 )

 if (tolower(lines[3]) != "na") {
  landuse_patterns <- unlist(strsplit(lines[3], "|", fixed = TRUE))
  interRowConditions <- lapply(landuse_patterns, function(pat) {
   list(
    pattern = pat,
    default = ifelse(is.na(lines[4]), NA, lines[4]),
    extract = ifelse(is.na(lines[5]), NA, lines[5]))
  })
  config$interRowConditions <- interRowConditions
 } else {
  config$interRowConditions <- NULL
 }

 return(config)
}
