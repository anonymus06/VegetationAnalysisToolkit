#'

#' Plot NDVI Data
#'
#' @param df A dataframe with columns 'Date', 'Code', and 'NDVI'.
#' @param code The specific code to filter by (optional).
#' @param tstart The start date for filtering (optional).
#' @param tend The end date for filtering (optional).
#' @param plot_type The type of plot to generate: "time_series", "boxplot", or "violin".
#'
#' @importFrom dplyr filter group_by summarise %>%
#' @importFrom ggplot2 ggplot geom_line geom_point geom_boxplot geom_violin labs theme_minimal aes
#' @importFrom lubridate as_date
#' @importFrom plotly ggplotly
#' @importFrom readxl read_excel
#'
#' @export
#'
#' @return A plotly object of the specified NDVI plot.
#'
#' @description
#' This function generates various types of plots for NDVI data including time series, box plots, and violin plots.
#' It allows for filtering by location codes and date range.
#'
#' @details
#' Based on the input data (\code{df}) and variable (\code{code}) and plot settings (\code{plot_type}), a plot is created.
#' The time range can be adjusted by \code{tstart} and \code{tend}.
#'
#' @examples
#' \dontrun{
#' df <- readxl::read_excel("path/to/your/data/ndvi_2023_filtered.xlsx")
#'
#' # plot timeseries for all locations and date range
#' plot_NDVI(df)
#'
#' # plot boxplot for specified dates and location
#' plot_NDVI(df, code = "CSTA", tstart = "2023-05-12", tend = "2023-08-18", plot_type = "boxplot")
#' }
plot_NDVI <- function(df, code=NULL, tstart=NULL, tend=NULL, plot_type=NULL) {

  # ---- initial part of procedure ----

  # set optional arguments
  # opt_param <- c("code", "tstart", "tend", "plot_type")
  # code <- tstart <- tend <- plot_type <- NULL

  # load additional arguments
  # param <- list(...)
  # for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  # unused_param <- setdiff(names(param), opt_param)
  # if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (!is.null(tstart)) tstart <- as_date(tstart)
  if (!is.null(tend)) tend <- as_date(tend)
  #if (is.null(ylim)) ylim <- c(NA_real_, NA_real_)
  #if (is.null(language)) language <- "EN"
  if (is.null(plot_type)) plot_type <- "time_series"
  if (is.null(code)) filtered_df <- df
  #if (is.null(quiet)) quiet <- TRUE

  # validate 'plot_type' argument
  plot_type <- match.arg(plot_type, choices = c("time_series", "boxplot", "violin"))

  # validate 'code' argument
  if (!is.null(code) && !(code %in% unique(df$Code))) {
    stop("Provided code does not exist in the dataset!")
  }

  # ensure NDVI is numeric and 'Date' is in the correct format
  df$Date <- as.Date(df$Date, origin = "1899-12-30")
  df$Date <- as.Date(df$Date, format="%Y-%m-%d")
  df$NDVI <- as.numeric(as.character(df$NDVI))

  # convert and validate 'tstart' and 'tend'
  date_range <- range(df$Date, na.rm = TRUE)
  if (!is.null(tstart)) {
    tstart <- as_date(tstart)
    if (is.na(tstart) || tstart < date_range[1]) stop("'tstart' is out of the dataset's date range.")
  } #todo: date_range - adott code-ra nézzük a tartományát ne az egész adatsornak!
  if (!is.null(tend)) {
    tend <- as_date(tend)
    if (is.na(tend) || tend > date_range[2]) stop("'tend' is out of the dataset's date range.")
  }
  if (!is.null(tstart) && !is.null(tend) && tstart > tend) {
    stop("'tstart' must be before 'tend'.")
  }


  # ---- main part of procedure ----

  # start with filtering by code if specified
  if (!is.null(code)){
    filtered_df <- df %>% filter(Code == code)
  }
  # apply date range filter if specified
  if (!is.null(tstart)) {
    filtered_df <- filtered_df %>% filter(Date >= as.Date(tstart))
  }
  if (!is.null(tend)) {
    filtered_df <- filtered_df %>% filter(Date <= as.Date(tend))
  }

  # conditional plotting based on plot_type
  if (plot_type == "time_series") {
    grouped_df <- if (!is.null(code)) {
      filtered_df %>% group_by(Date) %>% summarise(Avg_NDVI = mean(NDVI, na.rm = TRUE), .groups = 'drop')

    } else {
      filtered_df %>% group_by(Code, Date) %>% summarise(Avg_NDVI = mean(NDVI, na.rm = TRUE), .groups = 'drop')

    }

    p <- ggplot(grouped_df, aes(x = Date, y = Avg_NDVI, group = code, color = code)) +
      geom_line() + geom_point() + theme_minimal() +
      labs(title = ifelse(!is.null(code), paste0("NDVI Over Time for Code: ", code), "NDVI Over Time by Code"), # todo: címbe bele kell rakni ha jelmagyarázatba már látszik a code?
           x = "Date", y = "NDVI")

  } else if (plot_type == "boxplot") {

    # Box plots (or box-and-whisker plots) for NDVI values grouped by code would help you compare the
    # distribution of NDVI values across different codes. This could highlight variations in vegetation
    # health between different locations or conditions, showing median values, quartiles, and outliers.

    p <- ggplot(filtered_df, aes(x = Code, y = NDVI)) +
      geom_boxplot() + theme_minimal() +
      labs(title = "NDVI Variability by Code", x = "Code", y = "NDVI")

  } else if (plot_type == "violin") {

    p <- ggplot(filtered_df, aes(x = Code, y = NDVI)) +
      geom_violin(trim = FALSE) +
      geom_boxplot(width = 0.1, fill = "white") + theme_minimal() +
      labs(title = "NDVI Distribution by Code", x = "Code", y = "NDVI")
  }

  # Return the plot in a plotly object
  ggplotly(p)

}
