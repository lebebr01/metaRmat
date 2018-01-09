#' Olkin & Siotani 1976 variance-covariance matrix
#'
#' Description here
#'
#' @param data
#'
#' @export
olkin_siotani <- function(data) {

  nvar <- ncol(data)

  pp <- nvar * (nvar - 1) / 2 #number of correlations

  covmtx<-matrix(0, nrow = pp, ncol = pp)

  x <- array(0, dim = c(nvar, nvar, nvar, nvar))
  y <- array(0, dim = c(nvar, nvar, nvar, nvar))

  covm <- array(0, dim = c(nvar, nvar, nvar, nvar))
  for (s in 1:(nvar-1)){
    for (t in (s+1):nvar){
      for (u in s:(nvar-1)){
        for (v in (u+1):nvar){
          if (s!=u | t<=v) {
            x[s,t,u,v] <- 0.5*data[s,t]*data[u,v]*((data[s,u])^2+(data[s,v])^2+(data[t,u])^2+(data[t,v])^2)+data[s,u]*data[t,v]+data[s,v]*data[t,u]
            y[s,t,u,v] <- data[s,t]*data[s,u]*data[s,v]+data[t,s]*data[t,u]*data[t,v]+data[u,s]*data[u,t]*data[u,v]+data[v,s]*data[v,t]*data[v,u]
            covm[s,t,u,v] <- (x[s,t,u,v] - y[s,t,u,v])
          } else {
            x[s,t,u,v] <- y[s,t,u,v] <- covm[s,t,u,v] <- 0
          }
        }
      }
    }
  }
  covmt <- matrix(NA, nrow = pp, ncol = pp)
  for (s in 1:(nvar-1)){
    for (t in (s+1):nvar){
      for (u in s:(nvar-1)){
        for (v in (u+1):nvar){
          if (nvar>3){
            if (u<(nvar-1) && s<(nvar-1)){
              if (((s-1)*(nvar-s+1)+t-s)<((u-1)*(nvar-1)+v-u)) {
                covmt[((s-1)*(nvar-1)+t-s),((u-1)*(nvar-1)+v-u)] <- covmt[((u-1)*(nvar-1)+v-u),((s-1)*(nvar-1)+t-s)] <- covm[s,t,u,v]
              }
              if (((s-1)*(nvar-1)+t-s)==((u-1)*(nvar-1)+v-u)) {
                covmt[((s-1)*(nvar-1)+t-s),((u-1)*(nvar-1)+v-u)] <- covm[s,t,u,v]
              }
            }
            if (u==(nvar-1) && s<(nvar-1)) {
              covmt[((s-1)*(nvar-1)+t-s),(2*nvar-3+v-u)] <- covmt[(2*nvar-3+v-u),((s-1)*(nvar-1)+t-s)] <- covm[s,t,u,v]
            }
            if (u==(nvar-1) && s==(nvar-1)) {
              covmt[(2*nvar-3+t-s),(2*nvar-3+v-u)] <- covm[s,t,u,v]
            }
          }
          if (nvar==3) {
            if (((s-1)*(nvar-s+1)+t-s)<((u-1)*(nvar-1)+v-u)) {
              covmt[((s-1)*(nvar-1)+t-s),((u-1)*(nvar-1)+v-u)] <- covmt[((u-1)*(nvar-1)+v-u),((s-1)*(nvar-1)+t-s)] <- covm[s,t,u,v]
            }
            if (((s-1)*(nvar-1)+t-s)==((u-1)*(nvar-1)+v-u)) {
              covmt[((s-1)*(nvar-1)+t-s),((u-1)*(nvar-1)+v-u)] <- covm[s,t,u,v]
            }
          }
        }
      }
    }
  }
  covmt
}
