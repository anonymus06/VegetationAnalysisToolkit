#' Open the HTML Vignette for VegetationAnalysisToolkit
#'
#' This function opens the HTML vignette for the VegetationAnalysisToolkit package in your default web browser.
#' The vignette provides an introduction and examples to help you get started with the package.
#'
#' @return Opens the HTML vignette in the default browser.
#' @export
#' @examples
#' open_vignette()
open_vignette <- function() {
 html_vignette_path <- system.file("doc/introduction.html", package = "VegetationAnalysisToolkit")
 if (file.exists(html_vignette_path)) {
  browseURL(html_vignette_path)
 } else {
  message("Vignette not found. Please ensure the package is correctly installed.")
 }
}
