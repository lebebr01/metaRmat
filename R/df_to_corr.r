#' Helper function to read in data
#'
#' Description here
#'
#' @param data Data frame input used to convert to correlation matrices.
#' @param variables A character vector of variable names representing the columns
#'  to convert to a pairwise correlation matrix.
#'
#' @importFrom corpcor vec2sm
#'
#' @export
df_to_corr <- function(data, variables) {

  data_subset <- data[variables]

  lapply(seq_len(nrow(data_subset)), function(xx)
      row_to_matrix(data_subset[xx, ])
    )

}

row_to_matrix <- function(data) {

  corr_matrix <- corpcor::vec2sm(as.matrix(data), diag = FALSE)
  diag(corr_matrix) <- 1

  corr_matrix
}


