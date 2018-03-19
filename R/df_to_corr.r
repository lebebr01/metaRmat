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

  column_row_names <- add_variable_names(data_subset)

  lapply(seq_len(nrow(data_subset)), function(xx)
      row_to_matrix(data_subset[xx, ], column_row_names)
    )
}

row_to_matrix <- function(data, var_names) {

  corr_matrix <- corpcor::vec2sm(as.matrix(data), diag = FALSE)
  diag(corr_matrix) <- 1

  colnames(corr_matrix) <- rownames(corr_matrix) <- var_names

  corr_matrix
}

add_variable_names <- function(data) {

  var_names <- names(data)

  var_names <- strsplit(var_names, split = "_")

  after_ <- unique(unlist(lapply(seq_along(var_names), function(xx)
    var_names[[xx]][2])))

  before_ <- unique(unlist(lapply(seq_along(var_names), function(xx)
    var_names[[xx]][1])))

  c(after_, before_[!(before_ %in% after_)])

}


