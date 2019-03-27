#' Jacobian Standard Slopes
#'
#' @param R Matrix of estimates
#'
#' @importFrom Matrix sparseVector sparseMatrix
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

#' Variance-covariance Matrix of Partials
#'
#' @param R Matrix of estimates
#'
#' @importFrom corpcor vec2sm
#' @importFrom matrixcalc elimination.matrix svd.inverse vec
#' @importFrom Matrix sparseMatrix
#'
#' @export
jacobian_pcor <- function(R) {
  n <- nrow(R)
  Rinv <- solve(R)

  nvp <- 0.5*n*(n-1)
  Rgen <- corpcor::vec2sm(2*seq(nvp),diag=FALSE)
  diag(Rgen) <- 1

  ## Generates the Transition Matrix KcT ##
  vecR1 <- matrixcalc::vec(Rgen)
  vecRgen <- ifelse(vecR1==1, 0, vecR1)

  if (length(Rgen)==4){
    vecpRgen <- matrix(Rgen[2,1])
  } else{
    R1 <- Rgen[ ,-ncol(Rgen)]
    R1 <- R1[-1, ]
    vecpRgen <- matrixcalc::elimination.matrix(nrow(R1)) %*%
      matrixcalc::vec(R1)
  }

  for (i in 1:length(vecpRgen)){
    K1 <- ifelse(vecRgen == vecpRgen[i, 1], 1, 0)
    if(i > 1) {
      KcInvT <- cbind(KcInvT,K1)
      } else {
        KcInvT <- K1
      }
  }
  KcT <- matrixcalc::svd.inverse(KcInvT)

  # Creates diagonal matrix with entries equal to the square root of the
  # reciprocals of the diagonal entries of Rinv

  D1 <- diag(Rinv)
  D <- diag(D1, n, n)
  Dinv <- solve(D)
  SqDinv <- Dinv^(1/2)

  ## Creates the Jacobian matrix
  ## k is the number of non-redundant correlations

  k <- n * (n - 1) / 2
  Jac <- matrix(nrow=k, ncol=k)

  iter <- 1

  for(i in 1:(n - 1)){
    for(j in (i + 1):n){
      A <- Matrix::sparseMatrix(c(i, j), c(j, i),
                                    x=1, dims=c(n, n))
      dR <- as.matrix(-Rinv %*% A %*% Rinv)
      diagRinv <- diag(dR)
      diagMatRinv <- diag(diagRinv, n, n)

      dD <- -0.5*Dinv^(3/2) %*% diagMatRinv

      dP <- dD %*% Rinv %*% SqDinv +
        SqDinv %*% dR %*% SqDinv +
        SqDinv %*% Rinv %*% dD

      vecp_dP <- KcT %*% matrixcalc::vec(dP)
      Jac[, iter] <- -vecp_dP
      iter <- iter + 1
    }
  }
  Jac
}


#' Variance paths
#'
#' @param mean_r Average correlation matrices that are synthesized from metafor
#' @param var_cov Variance covariance matrix coming from metafor.
#' @param type Type of Jacobian to return, default value is 'stdslopes' can also
#'   specify 'pcor' for the partial correlations.
#'
#' @export
var_path <- function(mean_r, var_cov, type = 'stdslopes')  {

  if(type == 'stdslopes') {
    A <- jacobian_stdslopes(mean_r) # Jacobian matrix
  }
  if(type == 'pcor') {
    A <- jacobian_pcor(mean_r) # Jacobian matrix
  }

  A %*% var_cov %*% t(A)          # var-cov matrix
}
