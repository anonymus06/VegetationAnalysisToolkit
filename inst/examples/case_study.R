# -----------------------------------------------------------------------------
# Case Study: Investigating Soil–Plant–Water Interactions in Sloping Vineyards
# -----------------------------------------------------------------------------

# This script demonstrates the use of the VegetationAnalysisToolkit package to
# replicate some of the methods used in the research described by Horel and
# Zsigmond (2023). In the original study, soil management strategies were
# investigated for their effects on soil water content (SWC), leaf chlorophyll,
# NDVI, and other plant parameters in sloping vineyards.
#
# While the package is currently in development, its goal is to provide a unified
# R environment to fully support the replication and facilitation of similar
# studies. This script serves as a demonstration of the package’s current
# capabilities, with future versions planned to offer more comprehensive support
# for complex data analysis and research.
#
# Reference: Horel, Á., & Zsigmond, T. (2023). Plant growth and soil water content
# changes under different inter-row soil management methods in a sloping vineyard.
# Plants, 12(7), 1549. https://doi.org/10.3390/plants12071549


# 1. Load the VegetationAnalysisToolkit package
library(VegetationAnalysisToolkit)

# 2. Define Parameters
# Set the directory paths and filename for processing
folder_path <- system.file("extdata", "plantpen_ndvi_pri_data", package = "VegetationAnalysisToolkit")
# folder_path <- system.file("extdata", "mc100_chlorophyll_data", package = "VegetationAnalysisToolkit")
output_dir <- "out/"

# 3. Process NDVI Data
# The NDVI data reflects the plant's vigor and canopy structure.
process_PlantPen_NDVI_PRI(folder_path, output,
                          lower_limit = 0.0, upper_limit = 1.0,
                          variable = "NDVI",
                          validate = FALSE,
                          split_code = TRUE)

# 4. Process Chlorophyll Data
# Leaf chlorophyll content was used to evaluate grapevine health.
# process_MC100_Chlorophyll(folder_path, output,
#                           lower_limit = 0.0, upper_limit = 10.0,
#                           variable="CCI",
#                           validate=FALSE,
#                           split_code=TRUE)

# 5. Generate Basic Statistics
generate_statistics(output, variable = "CCI")
generate_statistics(output, variable = "NDVI")

# 6.Generate Time Series Plots
plot_measurement_data(output,
                      variable = "CCI",
                      code = "CSTA",
                      tstart = "2023-07-10",
                      tend = "2023-08-18",
                      plot_type = "time_series")

# -----------------------------------------------------------------------------
# End of Case Study Script
# -----------------------------------------------------------------------------
# This case study provides a simplified demonstration of the data processing and
# analysis techniques inspired by Horel and Zsigmond (2023). As the VegetationAnalysisToolkit
# develops, additional functionality will allow users to fully replicate this and
# similar studies within a unified R environment.
