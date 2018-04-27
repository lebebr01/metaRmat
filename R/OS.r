#' Olkin & Siotani 1976 variance-covariance numerator
#'
#' Description here
#'
#' @param data
#'
#' @export
OS <- function(data) {

  nvar <- ncol(data)

  pp <- nvar * (nvar - 1) / 2 #number of correlations

  covmtx <- matrix(0, nrow = pp, ncol = pp)

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


#' Olkin & Siotani 1976 variance-covariance matrix
#'
#' Description here
#'
#' @param data
#' @param n Sample size
#' @param type
#'
#' @export
olkin_siotani <- function(data, n, type = c('average','weighted', 'simple')) {

  if(type == 'average') {
    numerator <- average_OS(data)
  } else {
    if(type == 'weighted') {
      numerator <- weighted_OS(data, n)
    } else {
      numerator <- simple_OS(data)
    }
  }

  mapply("/", numerator, n, SIMPLIFY = FALSE)

}

simple_OS <- function(data) {

  data_nomissing <- lapply(data, function(x) replace(x, is.na(x), 0)) # replacing NA by 0

  lapply(data_nomissing, OS)

}


average_OS <- function(data) {

  data_nomissing <- lapply(data, function(x) replace(x, is.na(x), 0)) # replacing NA by 0
  data_average <- Reduce('+', data_nomissing) / length(data_nomissing)

  num_elements <- diag(data_average) * length(data_nomissing) # number of elements in each mean

  diag(data_average) <- 1

  numerator <- OS(data_average)

  do.call("list", rep(list(numerator), length(data_nomissing)))
}


weighted_OS <- function(data, n) {

  data_nomissing <- lapply(data, function(x) replace(x, is.na(x), 0)) # replacing NA by 0

  data_list <- lapply(data_nomissing, corpcor::sm2vec)  # vectorize
  data_list_n <- mapply("*", data_list, n, SIMPLIFY = FALSE)  # multiply by n
  data_sum <- Reduce("+", data_list_n)

  data_withmissing <- lapply(data, corpcor::sm2vec)  # vectorize corr matrix with NA
  missing_loc <-  lapply(data_withmissing, is.na)     # identify elements with NA
  missing_sampsize <- lapply(seq_along(missing_loc), function(i)
    ifelse(missing_loc[[i]] == FALSE, n[[i]], NA)) # replace FALSE by sample size

  sample_size <-  colSums(do.call(rbind, missing_sampsize), na.rm = TRUE)      # sum columns across list

  data_average <- data_sum / sample_size

  data_matrix <- corpcor::vec2sm(data_average, diag = FALSE, order = NULL)
  diag(data_matrix) <- 1

  numerator <- OS(data_matrix)                               # OS for one

  do.call("list", rep(list(numerator), length(data_nomissing)))

}

