#' Jacobian Standard Slopes
#'
#' @param R Matrix of estimates
#'
#' @importFrom Matrix sparseVector
#' @importFrom Matrix sparseMatrix
#'
#' @export
jacobian_stdslopes <- function(R) {

  n <-ncol(R)
  JacMat <- matrix(nrow=n-1, ncol=0.5*n*(n-1))

  #Partition of R for standardized slopes
  R01 <- R[2:n,1]
  R11 <- R[2:n,2:n]

  R11inv <- solve(R11)

  for (i in 1:(n-1)) {
    Ji <- as.vector(Matrix::sparseVector(1, i, n-1))
    JacMat[,i] <- R11inv %*% Ji
  }

  k <- n

  for (i in 1:(n-2)){
    for (j in (i+1):(n-1)){

      Jij <- as.matrix(Matrix::sparseMatrix(c(i,j), c(j,i),
                                    x=1, dims=c(n-1,n-1)))
      JacMat[,k] <- -R11inv %*% Jij %*% R11inv %*% R01

      k <- k+1
    }
  }
  JacMat
}


#' Variance paths
#'
#' @param mean_r
#' @param var_cov
#'
#' @export
var_path <- function(mean_r, var_cov)  {

  A <- jacobian_stdslopes(mean_r) # Jacobian matrix
  A %*% var_cov %*% t(A)          # var-cov matrix
}
