#' Print Collected Messages and Configuration
#'
#' This function prints the collected messages stored in the environment, along with a summary of the configuration
#' parameters used in the process. It organizes the output into sections, showing configuration details and activity logs.
#'
#' @param env An environment object where messages (both informational and issues) are stored.
#' @param lower_limit The lower limit for filtering, to be displayed in the configuration section.
#' @param upper_limit The upper limit for filtering, to be displayed in the configuration section.
#' @param variable A character string representing the variable being processed.
#' @param device_name A character string representing the device name.
#' @param split_code A logical value indicating whether the split code columns are enabled (`TRUE`) or disabled (`FALSE`).
#' @param validate A logical value indicating whether data validation is enabled (`TRUE`) or disabled (`FALSE`).
#'
#' @return This function does not return a value. It prints messages and configuration details to the console.
#'
#' @importFrom crayon green red black
#' @noRd
print_collected_messages <- function(env, lower_limit, upper_limit, variable,
                                     device_name = "Your Device Name", split_code, validate) {
 cat(green("===== Configuration =====\n"))
 cat(sprintf("Device: %s\n", device_name))
 cat(sprintf("Selected variable: %s\n", variable))
 cat(sprintf("Lower limit: %s\n", lower_limit))
 cat(sprintf("Upper limit: %s\n", upper_limit))
 cat(sprintf("Split Code Columns: %s\n", ifelse(split_code, "ON", "OFF")))
 cat(sprintf("Data validation: %s\n", ifelse(validate, "ON", "OFF")))
 cat(green("=========================\n"))

 cat(green("===== Activity Log =====\n"))
 for (msg in env$messages) {
  if (msg$type == "info") {
   cat(black(" *", msg$message), "\n")
  } else if (msg$type == "issues") {
   cat(red(" *", msg$message), "\n")
  }
 }
 cat(green("=========================\n"))
}
