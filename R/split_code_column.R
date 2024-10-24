#' Split Code Column into Position, Landuse, and InterRow Columns
#'
#' Splits the "Code" column into three new columns based on user-specified patterns.
#'
#' @param df A data frame with a column named "Code" that contains strings to be split.
#' @param config A list containing the regex patterns for extracting the "Position", "Landuse", and "InterRow" columns.
#' @param env An environment object used for logging messages.
#'
#' @return A data frame with three new columns ("Position", "Landuse", "InterRow")
#'         extracted from the "Code" column based on the provided patterns.
#'
#' @details This function allows for flexible parsing of the "Code" column into distinct categories (Position, Landuse, InterRow).
#'          The regex patterns and conditions are user-defined, allowing customization for various code formats.
#'          It handles invalid codes by logging them into the provided environment.
#'
#' @importFrom dplyr %>% mutate rowwise ungroup first
#' @importFrom purrr map_chr
#' @noRd
split_code_column <- function(df, config, env) {
  positionPattern <- config$positionPattern
  landusePattern <- config$landusePattern
  interRowConditions <- config$interRowConditions

  invalid_codes <- character()

  df <- df %>%
    rowwise() %>%
    mutate(
      ValidCode = is_valid_code(Code, positionPattern, landusePattern, interRowConditions),
      Position = if (ValidCode) extract_with_sorted_patterns(Code, positionPattern) else NA_character_,
      Landuse = if (ValidCode) extract_with_sorted_patterns(Code, landusePattern) else NA_character_
    ) %>%
    ungroup() %>%
    select(-ValidCode) %>%
    rowwise() %>%
    mutate(
      InterRow = if (!is.null(interRowConditions)) {
        map_chr(Code, function(code) {
          if (!is_valid_code(code, positionPattern, landusePattern, interRowConditions)) {
            invalid_codes <<- unique(c(invalid_codes, code))
            return(NA_character_)
          }
          for (condition in interRowConditions) {
            if (grepl(condition$pattern, code) && !grepl(condition$extract, code)) {
              return(condition$default)
            } else if (grepl(condition$extract, code)) {
              return(condition$extract)
            }
          }
          return(NA_character_) # Return NA if none of the conditions match
        }) %>% first()
      } else {
        NA_character_
      }
    ) %>%
    ungroup()

  if (length(invalid_codes) > 0) {
    add_message(env, paste0("Invalid code detected: ", paste(invalid_codes, collapse = ", ")), "info")
  }

  return(df)
}
