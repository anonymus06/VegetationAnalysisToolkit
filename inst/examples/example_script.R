# inst/examples/example_script.R

# Load the package
library(VegetationAnalysisToolkit)

# Define parameters
folder_path <- "inst/extdata/chlorophyl/"
output_dir <- "out/"
filename <- "20240626_test-run.xlsx"

# Run the process functions
process_NDVI(folder_path, output_dir, filename,
             lower_limit = 0.0, upper_limit = 1.0,
             variable = "NDVI",
             validate = FALSE,
             split_code = FALSE)

process_Chlorophyll(folder_path, output_dir, filename,
                    lower_limit = 0.0, upper_limit = 10.0,
                    variable = "CCI",
                    validate = FALSE,
                    split_code = TRUE)

# Generate statistics for the filtered data
generate_statistics(output_dir)
