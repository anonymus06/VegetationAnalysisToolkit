% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_processing.R
\name{filter_data}
\alias{filter_data}
\title{A function that gets rid of every junk column that goes after the NDVI column}
\usage{
filter_data(df)
}
\arguments{
\item{df}{A list of data frames where the last data frame contains the NDVI column.}
}
\value{
A modified list of data frames where the last data frame in the list having columns only up
to and including the NDVI column.
}
\description{
A function that gets rid of every junk column that goes after the NDVI column
}
\details{
The function identifies the position of the NDVI column in the last data frame of
the input list. Based on this position, the function then truncates the last
data frame in the list to include only columns up to and including the NDVI column.
}
\note{
This function assumes that there is an `NDVI` column in the last data frame of the list.
If these assumption does not hold, the function may not perform as expected.
}
