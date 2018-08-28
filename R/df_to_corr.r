#' Helper function to read in data
#'
#' This function builds a list of correlation matrices from data that are in
#' flat files where a correlation matrix is coded into a single row, a
#' common format for coding correlation matrices.
#'
#' @param data Data frame input used to convert to correlation matrices.
#' @param variables A character vector of variable names representing the columns
#'  to convert to a pairwise correlation matrix. Variable names take the following format:
#'  "columnname_rowname" where the name prior to the underscore is the column name and the
#'  name after the underscore is the row name.
#' @param ID A variable name, as a character string, to use as names for the list elements.
#'
#' @return A list of correlation matrices.
#'
#' @importFrom corpcor vec2sm
#'
#' @examples
#' becker09 <- read.csv(paste0(system.file('raw_data', package = 'metaRmat'),
#'    '/Becker09.csv'))
#' becker09_list <- df_to_corr(becker09,
#'    variables = c('Cognitive_Performance',
#'                  'Somatic_Performance',
#'                  'Selfconfidence_Performance',
#'                  'Somatic_Cognitive',
#'                  'Selfconfidence_Cognitive',
#'                  'Selfconfidence_Somatic'),
#'     ID = 'ID')
#'
#' @export
df_to_corr <- function(data, variables, ID = NULL) {

  data_subset <- data[variables]

  column_row_names <- add_variable_names(data_subset)

  data_list <- lapply(seq_len(nrow(data_subset)), function(xx)
      row_to_matrix(data_subset[xx, ], column_row_names)
    )

  names(data_list) <- data[, ID]

  data_list

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

extract_var_names <- function(var_names) {

  var_names <- strsplit(var_names, split = "_")

  after_ <- unique(unlist(lapply(seq_along(var_names), function(xx)
    var_names[[xx]][2])))

  before_ <- unique(unlist(lapply(seq_along(var_names), function(xx)
    var_names[[xx]][1])))

  c(after_, before_[!(before_ %in% after_)])

}


