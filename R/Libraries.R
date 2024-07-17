
# Libraries
library(openxlsx)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(readxl)
library(plotly)
library(crayon)
library(yaml)
library(purrr)

utils::globalVariables(c("Code", "Filtered", "Total", "Removed", "Date", "Index", "Landuse", "CCI", "NDVI"))
if (getRversion() >= "2.15.1") utils::globalVariables(c(".", "InterRow", "index", "n"))
if (getRversion() >= "2.15.1") utils::globalVariables("Avg_NDVI")
