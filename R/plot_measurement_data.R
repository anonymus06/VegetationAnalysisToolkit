#' Plot Measurement Data
#'
#' This function generates various types of plots for a single measurement variable (e.g., NDVI, CCI)
#' over time or as a distribution. It allows for filtering by measurement location codes and date ranges.
#'
#' @param output_path A character string specifying the path to the directory containing the processed data files.
#' @param variable The name of the variable to plot (e.g., 'NDVI', 'CCI').
#' @param code The specific code to filter by (optional).
#' @param tstart The start date for filtering (optional).
#' @param tend The end date for filtering (optional).
#' @param plot_type The type of plot to generate: "time_series", "boxplot", or "violin" (default is "time_series").
#'
#' @details
#' The function plots the selected variable for the specified code and date range, or the entire dataset if no
#' filters are provided.
#' It allows generating a time series plot, box plot, or violin plot for the data values.
#' The date range is set using \code{tstart} and \code{tend}.
#'
#' @return A plotly object of the specified plot.
#'
#' @importFrom dplyr filter group_by summarise %>%
#' @importFrom ggplot2 ggplot geom_line geom_point geom_boxplot geom_violin labs theme_minimal aes
#' @importFrom lubridate as_date
#' @importFrom plotly ggplotly
#'
#' @export
#'
#' @examples
#' \dontrun{
#' output <- "out/"
#'
#' # Plot time series for all locations and date range
#' plot_measurement_data(output, variable = "NDVI")
#'
#' # Plot boxplot for specified dates and location
#' plot_measurement_data(output, variable = "CCI", code = "CSTA", tstart = "2023-05-12", tend = "2023-08-18", plot_type = "boxplot")
#' }
plot_measurement_data <- function(output_path, variable, code = NULL, tstart = NULL, tend = NULL,
                                  plot_type = "time_series") {
 df <- load_filtered_excel(variable, output_path)

 # check used arguments
 if (!is.null(tstart)) tstart <- as_date(tstart)
 if (!is.null(tend)) tend <- as_date(tend)
 #if (is.null(ylim)) ylim <- c(NA_real_, NA_real_)
 #if (is.null(language)) language <- "EN"
 if (is.null(plot_type)) plot_type <- "time_series"
 if (is.null(code)) filtered_df <- df
 #if (is.null(quiet)) quiet <- TRUE

 # validate arguments
 plot_type <- match.arg(plot_type, choices = c("time_series", "boxplot", "violin"))
 if (!is.null(code) && !(code %in% unique(df$Code))) {
  stop("Provided code does not exist in the dataset!")
 }

 df$Date <- as.Date(df$Date, origin = "1899-12-30")
 df$Date <- as.Date(df$Date, format="%Y-%m-%d")
 df[[variable]] <- as.numeric(df[[variable]])

 date_range <- range(df$Date, na.rm = TRUE)
 if (!is.null(tstart)) {
  tstart <- as_date(tstart)
  if (is.na(tstart) || tstart < date_range[1]) stop("'tstart' is out of the dataset's date range.")
 }

 if (!is.null(tend)) {
  tend <- as_date(tend)
  if (is.na(tend) || tend > date_range[2]) stop("'tend' is out of the dataset's date range.")
 }

 if (!is.null(tstart) && !is.null(tend) && tstart > tend) {
  stop("'tstart' must be before 'tend'.")
 }


 # ---- main part of procedure ----

 if (!is.null(code)){
  filtered_df <- df %>% filter(Code == code)
 }
 if (!is.null(tstart)) {
  filtered_df <- filtered_df %>% filter(Date >= as.Date(tstart))
 }
 if (!is.null(tend)) {
  filtered_df <- filtered_df %>% filter(Date <= as.Date(tend))
 }

 if (plot_type == "time_series") {
  grouped_df <- if (!is.null(code)) {
   filtered_df %>% group_by(Date) %>% summarise(Avg_VAR = mean(get(variable), na.rm = TRUE), .groups = 'drop')

  } else {
   filtered_df %>% group_by(Code, Date) %>% summarise(Avg_VAR = mean(get(variable), na.rm = TRUE), .groups = 'drop')

  }

  p <- ggplot(grouped_df, aes(x = Date, y = Avg_VAR, group = code, color = code)) +
   geom_line() + geom_point() + theme_minimal() +
   labs(title = ifelse(!is.null(code), paste0(variable, "Over Time for Code: ", code), paste0(variable, "Over Time by Code")), # todo: címbe bele kell rakni ha jelmagyarázatba már látszik a code?
        x = "Date", y = variable)

 } else if (plot_type == "boxplot") {
  p <- ggplot(filtered_df, aes(x = Code, y = variable)) +
   geom_boxplot() + theme_minimal() +
   labs(title = paste0(variable, "Variability by Code", x = "Code", y = variable))

 } else if (plot_type == "violin") {
  p <- ggplot(filtered_df, aes(x = Code, y = variable)) +
   geom_violin(trim = FALSE) +
   geom_boxplot(width = 0.1, fill = "white") + theme_minimal() +
   labs(title = paste0(variable, "Distribution by Code", x = "Code", y = variable))
 }

 ggplotly(p)
}
