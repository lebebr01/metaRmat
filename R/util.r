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
