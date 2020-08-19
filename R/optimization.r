#' Find regression coefficients
#'
#' @param model_input Model input as a character string. Multiple models need
#'   to be on their own line. Model syntax uses lavann like syntax, see details
#'   for more details about this syntax.
#' @param R A correlation matrix, most likely this will be the average
#'   correlation matrix outputted from the metafor package.
#' @param ...Currently not used
#'
#' @return A vector of regression coefficient estimates.
#' @importFrom stats as.formula
#' @export
find_reg_coef = function(model_input, R, ...) {

  if(length(grep("^\\s*#", model_input, value = TRUE)) == 0) {
    model_input <- trimws(unlist(strsplit(model_input, ".#")))[1]

    if(length(grep("~", unlist(strsplit(model_input, "")))) != 1) {
      stop("Please check the model again")
    }
    try(as.formula(model_input))

    names_r1122 <- trimws(unlist(strsplit(model_input, "~")))

    names_r22 <- names_r1122[1]
    if(length(grep("+", names_r1122[2]))>0){
      names_r11 =  trimws(unlist(strsplit(names_r1122[2], "[+]")))
    }else{
      names_r11 = names_r1122[2]
    }

    if(any(!(names_r11 %in% colnames(R))) | any(!(names_r22 %in% colnames(R)))) {
      stop("A variable in the model does not match with the variables in the correlation matrix")
    }

    R_new = R[c(names_r11, names_r22), c(names_r11, names_r22)]

    R11 = R_new[names_r11, names_r11]
    R12 = R_new[names_r11, names_r22]

    b = as.numeric(solve(R11) %*% R12) # might need  another function
    names(b) <- paste(names_r11, "->", names_r22)

    list(b)

  } else {
    NULL
  }
}



#' Mul R2
#'
#' @param model_input Model input as a character string. Multiple models need
#'   to be on their own line. Model syntax uses lavann like syntax, see details
#'   for more details about this syntax.
#' @param R A correlation matrix, most likely this will be the average
#'   correlation matrix outputted from the metafor package.
#'
#' @return A vector of names
#' @importFrom stats as.formula
#' @export
Mul_R2 = function(model_input, R) {

  if(length(grep("^\\s*#", model_input, value=TRUE)) == 0){
    model_input = trimws(unlist(strsplit(model_input, ".#")))[1]

    if(length(grep("~", unlist(strsplit(model_input, "")))) != 1) {
      stop("Please check the model again")
    }
    try(as.formula(model_input))

    names_r1122 = trimws(unlist(strsplit(model_input, "~")))

    names_r22 = names_r1122[1]
    if(length(grep("+", names_r1122[2]))>0){
      names_r11 =  trimws(unlist(strsplit(names_r1122[2], "[+]")))
    }else{
      names_r11 = names_r1122[2]
    }

    if(any(!(names_r11 %in% colnames(R))) | any(!(names_r22 %in% colnames(R)))) {
      stop("A variable in the model does not match with the variables in the correlation matrix")
    }

    R_new = R[c(names_r11, names_r22), c(names_r11, names_r22)]

    R11 = R_new[names_r11, names_r11]
    R12 = R_new[names_r11, names_r22]

    mul_R2 = as.numeric(t(R12) %*% solve(R11) %*% R12) # might need  another function
    names(mul_R2) <- paste(names_r22)

    mul_R2

  } else {
    NULL
  }
}

#' Estimate regression coefficients
#'
#' @param model_input Model input as a character string. Multiple models need
#'   to be on their own line. Model syntax uses lavann like syntax, see details
#'   for more details about this syntax.
#' @param R A correlation matrix, most likely this will be the average
#'   correlation matrix outputted from the metafor package.
#'
#' @return A list of parameter estimates
#' @details Detailed examples of lavaan like syntax can be found:
#'   http://lavaan.ugent.be/tutorial/syntax1.html.
#'
#'   The output will be the same length as the number of regression equations
#'   specified in the model_input argument.
#' @export
#'
#' @examples
#' Br <-  matrix(c(1.00000000, -0.09773331, -0.1755029,  0.3186775,
#' -0.09773331,  1.00000000,  0.5271873, -0.4175596,
#' -0.17550292,  0.5271872,  1.0000000, -0.4006848,
#' 0.31867753, -0.41755963, -0.4006848,  1.0000000),
#' nrow = 4, byrow = TRUE)
#'
#' colnames(Br) <- c("Performance",  "Self_confidence",  "Cognitive", "Somatic" )
#'
#' rownames(Br) <- colnames(Br)
#'
#' ## Proposed path model
#'model <- "## Regression paths
#' Performance ~  Self_confidence  + Cognitive  + Somatic
#' Self_confidence ~ Cognitive + Somatic "
#'
#' find_B(model, Br)
find_B <- function(model_input, R) {
  model_line <- trimws(unlist(strsplit(model_input, "\n")))

  model_line <- model_line[which(model_line != "")]

  Result <- sapply(seq_along(model_line), function(xx) {
    find_reg_coef(model_line[xx], R)
    })

  if(sum(sapply(Result, is.null)) > 0) {
    Result <- Result[-which(sapply(Result, is.null))]

    lapply(Result, unlist)

  } else {

    Result
  }

}


#' c mat ft
#'
#' @param model_input Model input as a character string. Multiple models need
#'   to be on their own line. Model syntax uses lavann like syntax, see details
#'   for more details about this syntax.
#' @param R A correlation matrix, most likely this will be the average
#'   correlation matrix outputted from the metafor package.
#'
#' @return A list.
#' @export
#'
#' @examples
#' Br <-  matrix(c(1.00000000, -0.09773331, -0.1755029,  0.3186775,
#' -0.09773331,  1.00000000,  0.5271873, -0.4175596,
#' -0.17550292,  0.5271872,  1.0000000, -0.4006848,
#' 0.31867753, -0.41755963, -0.4006848,  1.0000000),
#' nrow = 4, byrow = TRUE)
#'
#' colnames(Br) <- c("Performance",  "Self_confidence",  "Cognitive", "Somatic" )
#'
#' rownames(Br) <- colnames(Br)
#'
#' ## Proposed path model
#'model <- "## Regression paths
#' Performance ~  Self_confidence  + Cognitive  + Somatic
#' Self_confidence ~ Cognitive + Somatic "
#'
#' c_mat_ft(model, Br)
c_mat_ft <- function(model_input, R) {
  A_mat <- matrix(0, nrow = nrow(R), ncol = ncol(R))
  colnames(A_mat) <- colnames(R)
  rownames(A_mat) <- rownames(R)

  path_coef <- find_B(model_input, R)

  for(j in seq_along(path_coef)) {
    for(i in seq_along(path_coef[[j]])) {
      A_names <- unlist(strsplit(names(path_coef[[j]][i]), ".->."))
      A_mat[A_names[2], A_names[1]] <- path_coef[[j]][i]
    }
  }

  S_mat <- matrix(0, nrow = nrow(R), ncol = ncol(R))
  colnames(S_mat) <- colnames(R)
  rownames(S_mat) <- rownames(R)
  diag(S_mat) <- 1

  exo <- unique(unlist(lapply(strsplit(unlist(lapply(path_coef, function(x) names(x))),
                                      ".->."), function(y) y[2])))

  ind <- setdiff(colnames(R),exo)

  for(i in 1:length(ind)) {
    for(j in 1:length(ind)) {
      S_mat[ind[i], ind[j]] <- R[ind[i], ind[j]]
    }
  }


  model_line <- trimws(unlist(strsplit(model_input, "\n")))

  model_line <- model_line[which(model_line != "")]

  R2 <- sapply(1:length(model_line), function(x) Mul_R2(model_line[x], R))

  if(is.list(R2)) {
    R2 <- unlist(R2)
  }

  for(h in 1:length(exo)) {
    S_mat[exo[h], exo[h]] <- 1 - R2[exo[h]]
  }

  names(R2) <- exo

  F_mat <- diag(nrow(R))
  colnames(F_mat) <- colnames(R)
  rownames(F_mat) <- rownames(R)

  I_R <- diag(nrow(R))

  C_mat <- F_mat %*% solve(I_R-A_mat) %*% S_mat %*% t(solve(I_R-A_mat)) %*% t(F_mat)


  num_freePar <- length(which(A_mat != 0))+
    length(unique(S_mat[(which(S_mat != 0 & S_mat != 1))]))
  df <- ncol(R)*(ncol(R)+1)/2 - num_freePar

  list(model_input = model_input,
       A_mat = A_mat,
       S_mat= S_mat,
       F_mat = F_mat,
       C_mat = C_mat,
       Mul_R2 = R2,
       num_freePar= num_freePar,
       df = df)
}

# R: observed correlation matrix
# method_mat: "lavaan" or "Loehlin"
# method_null: "sem" or "sem_QA"
# N: Number of observation


#' Model fitting function
#'
#' @param model_input Model input as a character string. Multiple models need
#'   to be on their own line. Model syntax uses lavann like syntax, see details
#'   for more details about this syntax.
#' @param R A correlation matrix, most likely this will be the average
#'   correlation matrix outputted from the metafor package.
#' @param method_mat Method of estimation, can either be "loehlin" or "lavaan".
#'   Default is "loehlin"
#' @param method_null Unsure
#' @param N Sample size
#'
#' @return A list of fit indices.
#' @export
#' @importFrom stats pchisq optimize
#' @examples
#' Br <-  matrix(c(1.00000000, -0.09773331, -0.1755029,  0.3186775,
#' -0.09773331,  1.00000000,  0.5271873, -0.4175596,
#' -0.17550292,  0.5271872,  1.0000000, -0.4006848,
#' 0.31867753, -0.41755963, -0.4006848,  1.0000000),
#' nrow = 4, byrow = TRUE)
#'
#' colnames(Br) <- c("Performance",  "Self_confidence",  "Cognitive", "Somatic" )
#'
#' rownames(Br) <- colnames(Br)
#'
#' ## Proposed path model
#'model <- "## Regression paths
#' Performance ~  Self_confidence  + Cognitive  + Somatic
#' Self_confidence ~ Cognitive + Somatic "
#'
#' N <- 573
#' model_fit(model_input = model, R = Br, method_mat  = "lavaan",
#'          method_null = "sem", N)
#' model_fit(model_input = model, R = Br, method_mat  = "loehlin",
#'          method_null = "sem", N )
model_fit <- function(model_input, R,
                     method_mat  = "loehlin",
                     method_null = "sem",
                     N) {

  model_result <- c_mat_ft(model_input, R)

  # Implied correlation matrix
  if(method_mat == "lavaan") {
    SigmaHat <-  model_result[['S_mat']]
  }
  if(method_mat == "loehlin") {
    SigmaHat <-  model_result[['C_mat']]
  }

  path_coef <- find_B(model_result[['model_input']], R)

  # MLdisc
  MLdisc <- .5*(sum(diag(( (R - SigmaHat) %*% solve(SigmaHat) )^2)))

  # GLSdisc
  GLSdisc <-  .5*(sum(diag( ( diag(ncol(R)) - solve(R) %*% SigmaHat)^2 )))


  Chi2 <- GLSdisc * (N - 1)
  pvalue <- pchisq(Chi2, model_result[['df']], ncp = 0,
                   lower.tail = F, log.p = FALSE)

  if (method_null == "sem"){
    # sem pacakge chi2Null method
    CC <- diag(diag(R))
    SS <- solve(R) %*% (R - CC)
    Chi2Null <- (N - 1)*0.5*sum(diag(SS %*% SS))
  }

  #Chi2Null <- (N - 1) * (sum(diag(S %*% diag(1/diag(S)))) +  log(prod(diag(S)))) # Q & A not sure about GLS Or ML

  dfNull <- ncol(R)*(ncol(R)+1)/2 - ncol(R) # Not sure about it

  lambda <- Chi2 - model_result[['df']]
  lambdaNULL <- Chi2Null - dfNull

  # CFI & TLI
  CFI <- 1 - lambda/lambdaNULL
  TLI <- 1 - (lambda/model_result[['df']])/(lambdaNULL/dfNull)

  # RMSEA
  if( lambda < 0 ){
    RMSEA <- "RMSEA is negative"
    RMSEA_CI <- NULL
  }else{
    RMSEA <- sqrt(lambda/((N-1)*model_result[['df']])) # when Chi2 - df is negative, it does not
    df <- model_result[['df']]
    max <- N
    tail <- (1 - 0.95)/2

    while (max > 1){
      res <- optimize(function(lam) (tail - pchisq( Chi2, df, ncp = lam))^2,
                      interval=c(0, max))

      if (is.na(res$objective) || res$objective < 0) {
        max <- 0
        warning("cannot find upper bound of RMSEA")
        break
      }
      if (sqrt(res$objective) < tail/100) {
        break
        }
      max <- max/2
    }

    lam_U <- if (max <= 1) NA else res$minimum
    max <- max(max, 1)
    while (max > 1){
      res <- optimize(function(lam) (1 - tail - pchisq(Chi2, df, ncp=lam))^2, interval=c(0, max))
      if (sqrt(res$objective) < tail/100) break
      max <- max/2
      if (is.na(res$objective) || res$objective < 0){
        max <- 0
        warning("cannot find lower bound of RMSEA")
        break
      }
    }
    lam_L <- if (max <= 1) NA else res$minimum
    RMSEA_U <- sqrt(lam_U/((N - (1))*df))
    RMSEA_L <- sqrt(lam_L/((N - (1))*df))

    RMSEA_CI = c(RMSEA_L, RMSEA_U)
  }

  #RMSEA_CI = c(sqrt(N/qchisq(0.975, N)),  sqrt(N/qchisq(0.025, N)))*RMSEA

  # SRMR
  diff <- R - SigmaHat

  uppder_diff <- diff[upper.tri(diff)]

  S_S <- matrix(nrow = nrow(R), ncol = ncol(R))

  for(i in 1:nrow(R)) {
    for(j in i:ncol(R)) {
      S_S[i,j] <- R[i,i]*R[j,j]
    }
  }
  uppder_S_S <- S_S[upper.tri(S_S)]

  SRMR <- sqrt((sum((diag(diff)/ diag(R)^2)^2) +
                  sum((uppder_diff / uppder_S_S)^2)) / (ncol(R)*(ncol(R)+1)/2))

  list(path_coefficients = path_coef,
       Model = round(c(Chi2 = Chi2 , df = model_result$df, pvalue =pvalue),4),
       NullModel = c(Chi2Null = Chi2Null, dfNull= dfNull),
       CFI = CFI,
       TLI= TLI,
       RMSEA = RMSEA,
       RMSEA_CI = RMSEA_CI,
       SRMR = SRMR)

}
