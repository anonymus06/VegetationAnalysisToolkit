% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_processing.R
\name{convertData}
\alias{convertData}
\title{Converts the data types of specific columns in a data frame.}
\usage{
convertData(df)
}
\arguments{
\item{df}{A data frame whose columns need data type conversion.}
}
\value{
A data frame with the first two columns converted to numeric, the third column to character,
        and the fourth column to date format. If date conversion fails, NA is inserted for those values.
}
\description{
Converts the data types of specific columns in a data frame.
}
\note{
It's important to ensure the input data frame has at least four columns and that the data
      in each column is appropriate for the intended conversion. In the specific context where this function
      is utilized within the code, these prerequisites are ensured by the logical structure and sequence
      of operations leading up to this point. Therefore, while general caution is advised, the unique
      circumstances of this function's application mitigate the need for additional checks within the function itself.
}
