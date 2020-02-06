var_cov <- function(data) {
  as.matrix(Matrix::bdiag(data))
}

missing_data <- function(data) {

  -1 * which(is.na(data))

}

tau_matrix <- function(rho, tau2) {

  rho_matrix <-  corpcor::vec2sm(rho)
  diag(rho_matrix) <- 1

  tau_mat <- diag(sqrt(tau2))

  tau_mat %*% rho_matrix %*% tau_mat

}

# Identify and subset matrix
matrix_subset <- function(matrix, variables) {

  match_column <- colnames(matrix) %in% variables
  match_row <- rownames(matrix) %in% variables

  matrix[match_row, match_column]

}

# format model coefficients
format_coefficients <- function(coefficients) {

  coef_names <- sapply(coefficients, names)
  coef_names <- lapply(coef_names, format_coef_names)

  coefs <- lapply(seq_along(coefficients), function(xx)
    cbind(coef_names[[xx]], estimate = coefficients[[xx]]))

  coefs
}

format_coef_names <- function(names) {

  names_coef <- data.frame(trimws(
    do.call('rbind', strsplit(names, "->"))),
    stringsAsFactors = FALSE)

  names(names_coef) <- c("predictor", "outcome")

  names_coef
}

unique_names <- function(coefficients) {
  unique(c(coefficients[['predictor']], coefficients[['outcome']]))
}

# Horrible hack to keep CRAN happy and suppress NOTES about
# parts of the code that use non-standard evaluation.
# See:
# http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
# https://github.com/smbache/magrittr/issues/29
utils::globalVariables(c('lhs', 'model', 'model_out_random',
                         'op', 'rhs'))
