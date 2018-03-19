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
