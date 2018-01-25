var_cov <- function(data) {
  as.matrix(Matrix::bdiag(data))
}

missing_data <- function(data) {

  -1 * which(is.na(data))

}
