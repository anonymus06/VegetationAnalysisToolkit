% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/file_handling.R
\name{read_Chlorophyll}
\alias{read_Chlorophyll}
\title{Read Chlorophyll Data with Index files}
\usage{
read_Chlorophyll(
  folder_pathway,
  valid_patterns = c("^\\\\d{6}(\\\\d{8})?\\\\.csv$", "index\\\\.xlsx$")
)
}
\arguments{
\item{folder_pathway}{A character string specifying the path to the folder containing the chlorophyll data files.}

\item{valid_patterns}{A character vector of valid filename patterns to match. Default is c("^\\d{6}(\\d{8})?\\.csv$", "index\\.xlsx$").}
}
\value{
A list containing:
        - `all_data`: A list of data frames containing chlorophyll data and index files, organized by file name or sheet name.
        - `data_frame_files`: A list mapping data frame names to their respective file paths.
}
\description{
Read Chlorophyll Data with Index files
}
\details{
This function was optimized to read chlorophyll raw CSV files generated by instrument X (version Y)
and user-defined Excel files that record measurements. For sample file, see: .
}
