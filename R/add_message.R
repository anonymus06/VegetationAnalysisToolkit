#' Add Messages to a List
#'
#' This function adds messages to a list within the specified environment (`env`) with a designated message type.
#'
#' @param env An environment containing a list `messages` where the message will be stored.
#' @param message A character string representing the message to be added.
#' @param type A character string indicating the type of message (e.g., "info", "warning", "error"). The default is "info".
#'
#' @return The function does not return a value, but it appends the message to the `messages` list within `env`.
#'
#' @examples
#' env <- new.env()
#' env$messages <- list()
#' add_message(env, "Data loaded successfully", type = "info")
#' add_message(env, "File not found", type = "error")
#'
#' @noRd
add_message <- function(env, message, type = "info") {
  env$messages <- append(env$messages, list(list(message = message, type = type)))
}
