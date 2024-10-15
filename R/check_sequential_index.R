#' Helper function to check if a sequence is sequential
#'
#' @param df A data frame containing the sequence in a column named 'Sample'.
#' @param verbose Logical, if TRUE prints detailed messages.
#'
#' @return Logical, TRUE if the sequence is sequential, FALSE otherwise.
#'
#' @noRd
check_sequential_index <- function(df, verbose = FALSE) {
 split_indices <- which(df$Sample == "Sample")
 split_indices <- c(split_indices, nrow(df) + 1)
 index0 <- 1

 for (i in seq_along(split_indices)) {

  index <- split_indices[i]
  if (i == length(split_indices)) {
   seq_df <- df[index0:(nrow(df)), ]
  } else {
   seq_df <- df[index0:(index - 1), ]
  }

  index0 <- index + 1
  numeric_samples <- suppressWarnings(as.numeric(seq_df$Sample[seq_df$Sample != "Sample"]))
  if (any(diff(numeric_samples) != 1)) {
   if (verbose) {
    print(sprintf("Non-sequential values found in 'Sample' column within rows %d to %d.", index0, index - 1))
   }
   return(FALSE)
  }

  if (verbose) {
   print(sprintf("Checked sequentiality up to row %d: OK", index - 1))
  }

 }

 return(TRUE)
}
