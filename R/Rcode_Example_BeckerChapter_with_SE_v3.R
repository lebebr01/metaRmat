# #---------------------------------------------------------------------
# # Example Becker Chapter
# #---------------------------------------------------------------------
# # Load packages
# #---------------------------------------------------------------------
#   # library(gdata)
#   # library(corpcor)
#   # library(metaSEM)
#   # library(metafor)
#   # library(lavaan)
#   # library(RAMpath)
# #---------------------------------------------------------------------
# # Create a data set
# #---------------------------------------------------------------------
#   dat <-  Becker09
#
#   R <- dat[[1]]
#   n <- unlist(dat[[2]])
# #------------------------------------------------------------------
# # Compute O & S for each study indicidually
# #------------------------------------------------------------------
#      OL <- lapply(R,OS)
#    OL <- mapply("/",OL,n,SIMPLIFY = FALSE)
# #------------------------------------------------------------------
# # Compute  mean O & S
# #------------------------------------------------------------------
# # first replace NA by zeros
#     RR <- R                           # redifine list
#     RR <- lapply(RR,function(x) replace(x,is.na(x),0)) # replacing NA by 0
#     Rm <- Reduce('+', RR)/length(RR)
#        nElem <- diag(Rm) * length(RR) # number of elements in each mean
#           diag(Rm) <- 1
#      Rm1 <- OS(Rm)
#      Rm2 <-  do.call("list", rep(list(Rm1), length(RR)))
#      OLm <- mapply("/",Rm2 ,n,SIMPLIFY = FALSE)
# #---------------------------------------------------------------------
# # Compute sample size weighted mean O & S
# #------------------------------------------------------------------
#     Rw1 <- lapply(RR, sm2vec)  # vectorize
#     Rw2 <- mapply("*",Rw1,n,SIMPLIFY = FALSE)  # multiply by n
#     Rw3 <- Reduce("+", Rw2)
#     Rw1b <- lapply(R, sm2vec)  # vectorize corr matrix with NA
#     a <-  lapply(Rw1b, is.na)                                           # identify elements with NA
#     a2 <- lapply(seq_along(a), function(i) ifelse(a[[i]] == FALSE, n[[i]], NA)) # replace FALSE by sample size
#     nS <-  colSums(do.call(rbind, a2), na.rm = TRUE)                    # sum columns across list
#
#
#        # nS <- Reduce('+', n)                       # sum all n
#    # Rw4 <- Rw3/nS                         # create an indicator for missing values to remove sampe size
#    # res <- c(0,  0, 16, 45, 16 + 45, 16 + 45)
#   #  nS_2 <- nS - res
#   #  Rw4 <- Rw3/nS_2
#     Rw4 <- Rw3/nS
#     Rw5 <- vec2sm(Rw4, diag = FALSE, order = NULL)
#         diag(Rw5) <- 1
#       OSw <- OS(Rw5)                               # OS for one
#     OSw1 <-  do.call("list", rep(list(OSw), length(RR)))
#     OLw <- mapply("/",OSw1 ,n,SIMPLIFY = FALSE)
# #-------------------------------------------------------------------
# # Analyses
# #------------------------------------------------------------------
#   rr <- lapply(R,  sm2vec)  # vectorize
#   r <-   matrix(unlist(rr)) # vector effect sizes
#  # miss_ind <- 1*(is.na(r))  # creats indicator variable for missing data
#   miss_loc <- -1 * which(is.na(r)) #creats indicator variable for missing data
#   # S <- bdiag(OL)            # var_cov within
#   #  S <- bdiag(OLm)          # var-cov mean
#    S <- bdiag(OLw)          # var-cov sample size weighted mean
#   S <- as.matrix(S)
# #-------------------------------------------------------------------
# # Removing manually missing values
# #-------------------------------------------------------------------
#   # r <- r[c(-15, -17, -18, -28, -29, -30)]                  # Need to work this to make it outomatic
# #  r <- na.omit(r)
# #   S <- S[-c(15, 17, 18, 28, 29, 30), -c(15, 17, 18, 28, 29, 30)]
# # outcome <- rep(1:length(rr[[1]]), length(R))
# #  outcome <- as.factor(outcome)
# #  study <- sort(rep(1:length(R),length(rr[[1]])))
# #  outcome <- outcome[c(-15, -17, -18, -28, -29, -30)]
# #  study <- study[c(-15, -17, -18, -28, -29, -30)]
#  r <- r[miss_loc]                  # Need to work this to make it outomatic
#   # r <- na.omit(r)
#    S <- S[c(miss_loc), c(miss_loc)]
#  outcome <- rep(1:length(rr[[1]]), length(R))
#   outcome <- as.factor(outcome)
#   study <- sort(rep(1:length(R),length(rr[[1]])))
#   outcome <- outcome[miss_loc]
#   study <- study[miss_loc]
# #-------------------------------------------------------------------
# # Random Effects Results
# #-------------------------------------------------------------------
#   res.mR <- rma.mv(r,S, mods = ~ factor(outcome) -1 , random = ~ factor(outcome) | factor(study),
#                    struct="UN", method="REML")
#  summary(res.mR)
#
#
#
#  # with intercept for Q Model
#  res.mR_Qb <- rma.mv(r,S, mods = ~ factor(outcome) , random = ~ factor(outcome)  | factor(study),
#                    struct="UN", method="REML")
# # summary(res.mR_Qb)
# #-------------------------------------------------------------------
# # Fixed Effects Results
# #-------------------------------------------------------------------
#   res.mF <- rma.mv(r,S, mods = ~ factor(outcome) - 1)
#   # summary(res.mF)
# #-------------------------------------------------------------------
# # Extracting elements from output
# #-------------------------------------------------------------------
#     bf <- res.mF$b
#     rvecf <- as.matrix(bf)
#     Bf <- vec2sm(rvecf, diag = FALSE, order = NULL)
#     diag <- 1
#     diag(Bf) <- diag
#     Vf <-  res.mF$vb
#     Bf   # fixed- mean corr
#     Vf   # var-cov fixed mean corr
# #-------------------------------------------------------------------
# # Extracting elements from  Random-effect output
# #-------------------------------------------------------------------
#     br <- res.mR$b
#     rvecr <- as.matrix(br)
#     Br <- vec2sm(rvecr, diag = FALSE, order = NULL)
#     diag <- 1
#     diag(Br) <- diag
#     Vr <-  res.mR$vb
#     Tau2 <-  res.mR$tau2
#     Rho <-  res.mR$rho
#     Br # random - mean corr
#     Vr  # var-cov random mean corr
#     round(Vr, 5)
#     Tau2
#     Rho
#  #------------------
#  # tau matrix
#  #------------------
#   rho <-  vec2sm(Rho, diag = FALSE)
#   diag(rho) <- diag
#   tdiag <- diag(sqrt(Tau2))
#   taumat <- tdiag %*% rho %*% tdiag
#   round(taumat, 4)
#   round(cov2cor(taumat),3)
# ####################################################################
# ##################################################################
#   # order of elements in the Br matrix and var-cov
#   #-----------------------------------------------------------------
#     cl_a <- substr(colnames(R[[1]]), 1,7)
#   cl_names <- colnames(R[[1]])
#   library(combinat)
#     # combos <- combn(cl_a, 2)
#     combos <- combn(cl_names, 2)
#     cl_b <-  paste(combos[1,], combos[2,], sep = "_")
#
#     tmp <-  vec2sm(cl_b, diag = FALSE)
#     # diag(tmp) <- cl_a
#    diag(tmp) <- cl_names
#     combos_2 <- combn(cl_b, 2)
#     cl_v <-  paste(combos_2[1,], combos_2[2,], sep = "_")
#     tmp_2 <-  vec2sm(cl_v, diag = FALSE)
#     diag(tmp_2) <- cl_b
#    cl_v3 <-  sm2vec(tmp_2, diag = TRUE)
#
#     # removing performance
#     cl_b2 <- br[ -1*grep(cl_a[1], cl_b) ]  # removing performance from mean r matrix
#       cl_b2 <- vec2sm(cl_b2, diag = FALSE, order = NULL)
#       diag(cl_b2) <- diag
#       cl_b2 # R_xx
#      # outcome column
#       cl_b2_y <- br[ grep(cl_a[1], cl_b) ]  # Rxy
#      # paths
#       solve(cl_b2) %*% cl_b2_y
#       # removing performance
#
#    ## fix cl_v REDUCE MODEL VAT COV
#
#     vr <-  sm2vec(Vr, diag = TRUE) # vector of mean var-cov
#        vr_2 <- vr[ -1*grep(cl_a[1], cl_v3) ]  # removing performance from mean var-cov
#      #  A <- matrix(vr_2,ncol(cl_b2),ncol(cl_b2) )
#      #  B <- B <- matrix(NA,dim(A)[1],dim(A)[2])
#       # lowerTriangle(B) <- lowerTriangle(A)
#       # upperTriangle(B) <- lowerTriangle(A)
#       # diag(B) <- diag(A)
#       # vr_2 <- B
#        vr_2 <- vec2sm(vr_2, diag = TRUE)    # var cov of reduce model
# ###################################################################
# # SE using Jac matrix
# #####################################################################
# # library(Matrix)
#
# # outcome first row
#
# Jac.StdSlopes <- function(R){
#
# n <-ncol(R)
# JacMat <- matrix(nrow=n-1,ncol=0.5*n*(n-1))
#
# #Partition of R for standardized slopes
# R01 <- R[2:n,1]
# R11 <- R[2:n,2:n]
#
# R11inv <- solve(R11)
#
# for (i in 1:(n-1)) {
# Ji <- as.vector(sparseVector(1,i,n-1))
# JacMat[,i] <- R11inv%*%Ji
# }
#
# k <- n
#
# for (i in 1:(n-2)){
# for (j in (i+1):(n-1)){
#
# Jij <-as.matrix(sparseMatrix(c(i,j),c(j,i),x=1,dims=c(n-1,n-1)))
# JacMat[,k] <- -R11inv%*%Jij%*%R11inv%*%R01
#
# k <- k+1
# }
# }
# JacMat
# }
#
#  # var function
#    var_path <- function (mean_r, var_cov)  {
#
#      A <- Jac.StdSlopes(mean_r) # Jacobian matrix
#      A %*% var_cov %*% t(A)          # var-cov matrix
#   } # end SE function
#    var_path(mean_r = Br, var_cov = Vr) # Var of the path
#    var_path(Br[-1,-1], vr_2)
# #########################################################
#
#
#
#
# #-------------------------------------------------------------------
# # Computing Stage 2
# #-------------------------------------------------------------------
# # using first column as outcome
#   Rxx <- Br[-1, -1]
#   Rxy <- Br[1, 2:4]
#   B <- solve(Rxx) %*% Rxy
#   B # slopes as cog, somatic and self; outcome perf
#   t(B)%*%Rxy   # R2 for peformance
#    (vpath <- var_path(mean_r = Br, var_cov = Vr)) # Var of the path
#    (z_test <- B/ sqrt(diag(vpath))) # z test
#     2*pnorm(-abs(z_test))   # p value
# #-----------------------------------------------------
# # reducing the matrix for the intermediate outcome
# #-----------------------------------------------------
#   Br_2 <- Br[-1, -1]
#   Rxx_2 <- Br_2[-3, -3]
#   Rxy_2 <- Br_2[3, 1:2]
#   B_2 <- solve(Rxx_2) %*% Rxy_2
#   B_2  # slopes cog and somatic; outcome self
#    t(B_2)%*%Rxy_2   # R2 for self
#           vr <-  sm2vec(Vr, diag = TRUE) # vector of mean var-cov
#           vr_2 <- vr[ -1*grep(cl_a[1], cl_v3) ]  # removing performance from mean var-cov
#           vr_2 <- vec2sm(vr_2, diag = TRUE)    # var cov of reduce model
#     (vpath <- var_path(mean_r = Br[-1,-1], var_cov = vr_2)) # Var of the reduce path
#     (z_test <- B_2/ sqrt(diag(vpath))) # z test
#     2*pnorm(-abs(z_test))   # p value
# ####################################################################
# ##################################################################
# #-------------------------------------------------------------------
# #
# #-------------------------------------------------------------------
# # Fitting seprate analysis by sport type
#
# # TEAM sport
# #-------------------------------------------------------------------
#   dat_team <- dat
#
#
#    dat_team[[2]] <- dat_team[[2]][c(-1, -2, -4, -5, -6, -10)]
#    dat_team[[3]] <- dat_team[[3]][c(-1, -2, -4, -5, -6, -10)]
#
#    dat_team[[1]] <- dat_team[[1]][c(-1, -2, -4, -5, -6, -10)]
#   dat_team[[1]]
#
# #------------------------------------------------------------------
#   R <- dat_team[[1]]
#   n <- unlist(dat_team[[2]])
# #------------------------------------------------------------------
# # Compute O & S for each study indicidually
# #------------------------------------------------------------------
#      OL <- lapply(R,OS)
#    OL <- mapply("/",OL,n,SIMPLIFY = FALSE)
# #------------------------------------------------------------------
# # Compute  mean O & S
# #------------------------------------------------------------------
# # first replace NA by zeros
#     RR <- R                           # redifine list
#     RR <- lapply(RR,function(x) replace(x,is.na(x),0)) # replacing NA by 0
#     Rm <- Reduce('+', RR)/length(RR)
#        nElem <- diag(Rm) * length(RR) # number of elements in each mean
#           diag(Rm) <- 1
#      Rm1 <- OS(Rm)
#      Rm2 <-  do.call("list", rep(list(Rm1), length(RR)))
#      OLm <- mapply("/",Rm2 ,n,SIMPLIFY = FALSE)
# #---------------------------------------------------------------------
# # Compute sample size weighted mean O & S
# #------------------------------------------------------------------
#     Rw1 <- lapply(RR, sm2vec)  # vectorize
#     Rw2 <- mapply("*",Rw1,n,SIMPLIFY = FALSE)  # multiply by n
#     Rw3 <- Reduce("+", Rw2)
#     Rw1b <- lapply(R, sm2vec)  # vectorize corr matrix with NA
#     a <-  lapply(Rw1b, is.na)                                           # identify elements with NA
#     a2 <- lapply(seq_along(a), function(i) ifelse(a[[i]] == FALSE, n[[i]], NA)) # replace FALSE by sample size
#     nS <-  colSums(do.call(rbind, a2), na.rm = TRUE)                    # sum columns across list
#
#
#        # nS <- Reduce('+', n)                       # sum all n
#    # Rw4 <- Rw3/nS                         # create an indicator for missing values to remove sampe size
#    # res <- c(0,  0, 16, 45, 16 + 45, 16 + 45)
#   #  nS_2 <- nS - res
#   #  Rw4 <- Rw3/nS_2
#     Rw4 <- Rw3/nS
#     Rw5 <- vec2sm(Rw4, diag = FALSE, order = NULL)
#         diag(Rw5) <- 1
#       OSw <- OS(Rw5)                               # OS for one
#     OSw1 <-  do.call("list", rep(list(OSw), length(RR)))
#     OLw <- mapply("/",OSw1 ,n,SIMPLIFY = FALSE)
# #-------------------------------------------------------------------
# # Analyses
# #------------------------------------------------------------------
#   rr <- lapply(R,  sm2vec)  # vectorize
#   r <-   matrix(unlist(rr)) # vector effect sizes
#  # miss_ind <- 1*(is.na(r))  # creats indicator variable for missing data
#   miss_loc <- -1 * which(is.na(r)) # specific location of values
#    # S <- bdiag(OL)            # var_cov within
#   #  S <- bdiag(OLm)          # var-cov mean
#    S <- bdiag(OLw)          # var-cov sample size weighted mean
#   S <- as.matrix(S)
# #-------------------------------------------------------------------
# # Removing manually missing values
# #-------------------------------------------------------------------
#    r <- r[miss_loc]                  # Need to work this to make it outomatic
#   # r <- na.omit(r)
#    S <- S[c(miss_loc), c(miss_loc)]
#  outcome <- rep(1:length(rr[[1]]), length(R))
#   outcome <- as.factor(outcome)
#   study <- sort(rep(1:length(R),length(rr[[1]])))
#   outcome <- outcome[miss_loc]
#   study <- study[miss_loc]
# #-------------------------------------------------------------------
# # Random Effects Results
# #-------------------------------------------------------------------
#   res.mR <- rma.mv(r,S, mods = ~ factor(outcome) -1 , random = ~ factor(outcome) | factor(study),
#                    struct="UN", method="REML")
# # summary(res.mR)
#  # with intercept for Q Model
# # res.mR_Qb <- rma.mv(r,S, mods = ~ factor(outcome) , random = ~ factor(outcome)  | factor(study),
#   #                 struct="UN", method="REML")
# # summary(res.mR_Qb)
# #-------------------------------------------------------------------
# # Fixed Effects Results
# #-------------------------------------------------------------------
#   res.mF <- rma.mv(r,S, mods = ~ factor(outcome) - 1)
#   # summary(res.mF)
# #-------------------------------------------------------------------
# # Extracting elements from output
# #-------------------------------------------------------------------
#     bf <- res.mF$b
#     rvecf <- as.matrix(bf)
#     Bf <- vec2sm(rvecf, diag = FALSE, order = NULL)
#     diag <- 1
#     diag(Bf) <- diag
#     Vf <-  res.mF$vb
#     Bf   # fixed- mean corr
#     Vf   # var-cov fixed mean corr
# #-------------------------------------------------------------------
# # Extracting elements from  Random-effect output
# #-------------------------------------------------------------------
#     br <- res.mR$b
#     rvecr <- as.matrix(br)
#     Br <- vec2sm(rvecr, diag = FALSE, order = NULL)
#     diag <- 1
#     diag(Br) <- diag
#     Vr <-  res.mR$vb
#     Tau2 <-  res.mR$tau2
#     Rho <-  res.mR$rho
#     Br # random - mean corr
#     Vr  # var-cov random mean corr
#     Tau2
#     Rho
#  #------------------
#  # tau matrix
#  #------------------
#   rho <-  vec2sm(Rho, diag = FALSE)
#   diag(rho) <- diag
#   tdiag <- diag(sqrt(Tau2))
#   taumat <- tdiag %*% rho %*% tdiag
#   round(taumat, 4)
#
# #-------------------------------------------------------------------
# # Computing Stage 2
# #-------------------------------------------------------------------
# # using first column as outcome
#   Rxx <- Br[-1, -1]
#   Rxy <- Br[1, 2:4]
#   B <- solve(Rxx) %*% Rxy
#   B # slopes as cog, somatic and self; outcome perf
#   t(B)%*%Rxy   # R2 for peformance
#    (vpath <- var_path(mean_r = Br, var_cov = Vr)) # Var of the path
#    (z_test <- B/ sqrt(diag(vpath))) # z test
#     2*pnorm(-abs(z_test))   # p value
#
# # reducing the matrix for the intermediate outcome
#   Br_2 <- Br[-1, -1]
#   Rxx_2 <- Br_2[-3, -3]
#   Rxy_2 <- Br_2[3, 1:2]
#   B_2 <- solve(Rxx_2) %*% Rxy_2
#   B_2  # slopes cog and somatic; outcome self
#   t(B_2)%*%Rxy_2   # R2 for self
#
#   # var of path reduce model
#  vr <-  sm2vec(Vr, diag = TRUE) # vector of mean var-cov
#        vr_2 <- vr[ -1*grep(cl_a[1], cl_v3) ]  # removing performance from mean var-cov
#        vr_2 <- vec2sm(vr_2, diag = TRUE)    # var cov of reduce model
#      (vpath <- var_path(mean_r = Br[-1,-1], var_cov = vr_2)) # Var of the reduce path
#    (z_test <- B_2/ sqrt(diag(vpath))) # z test
#     2*pnorm(-abs(z_test))   # p value
# ####################################################################
# ##################################################################
#
# ####################################################################
# #-------------------------------------------------------------------
# # Ind sport
# #-------------------------------------------------------------------
#   dat_indv <- dat
#
#    dat_indv[[2]] <- dat_indv[[2]][c(-3, -7, -8, -9)]
#    dat_indv[[3]] <- dat_indv[[3]][c(-3, -7, -8, -9)]
#
#    dat_indv[[1]] <- dat_indv[[1]][c(-3, -7, -8, -9)]
#   dat_indv[[1]]
#
#
# #------------------------------------------------------------------
#   R <- dat_indv[[1]]
#   n <- unlist(dat_indv[[2]])
# #------------------------------------------------------------------
# # Compute O & S for each study indicidually
# #------------------------------------------------------------------
#      OL <- lapply(R,OS)
#    OL <- mapply("/",OL,n,SIMPLIFY = FALSE)
# #------------------------------------------------------------------
# # Compute  mean O & S
# #------------------------------------------------------------------
# # first replace NA by zeros
#     RR <- R                           # redifine list
#     RR <- lapply(RR,function(x) replace(x,is.na(x),0)) # replacing NA by 0
#     Rm <- Reduce('+', RR)/length(RR)
#        nElem <- diag(Rm) * length(RR) # number of elements in each mean
#           diag(Rm) <- 1
#      Rm1 <- OS(Rm)
#      Rm2 <-  do.call("list", rep(list(Rm1), length(RR)))
#      OLm <- mapply("/",Rm2 ,n,SIMPLIFY = FALSE)
# #---------------------------------------------------------------------
# # Compute sample size weighted mean O & S
# #------------------------------------------------------------------
#     Rw1 <- lapply(RR, sm2vec)  # vectorize
#     Rw2 <- mapply("*",Rw1,n,SIMPLIFY = FALSE)  # multiply by n
#     Rw3 <- Reduce("+", Rw2)
#     Rw1b <- lapply(R, sm2vec)  # vectorize corr matrix with NA
#     a <-  lapply(Rw1b, is.na)                                           # identify elements with NA
#     a2 <- lapply(seq_along(a), function(i) ifelse(a[[i]] == FALSE, n[[i]], NA)) # replace FALSE by sample size
#     nS <-  colSums(do.call(rbind, a2), na.rm = TRUE)                    # sum columns across list
#
#
#        # nS <- Reduce('+', n)                       # sum all n
#    # Rw4 <- Rw3/nS                         # create an indicator for missing values to remove sampe size
#    # res <- c(0,  0, 16, 45, 16 + 45, 16 + 45)
#   #  nS_2 <- nS - res
#   #  Rw4 <- Rw3/nS_2
#     Rw4 <- Rw3/nS
#     Rw5 <- vec2sm(Rw4, diag = FALSE, order = NULL)
#         diag(Rw5) <- 1
#       OSw <- OS(Rw5)                               # OS for one
#     OSw1 <-  do.call("list", rep(list(OSw), length(RR)))
#     OLw <- mapply("/",OSw1 ,n,SIMPLIFY = FALSE)
# #-------------------------------------------------------------------
# # Analyses
# #------------------------------------------------------------------
#   rr <- lapply(R,  sm2vec)  # vectorize
#   r <-   matrix(unlist(rr)) # vector effect sizes
#   miss_loc <- -1 * which(is.na(r)) # creats indicator variable for missing data specific location of values
#   # S <- bdiag(OL)            # var_cov within
#   #  S <- bdiag(OLm)          # var-cov mean
#    S <- bdiag(OLw)          # var-cov sample size weighted mean
#   S <- as.matrix(S)
# #-------------------------------------------------------------------
# # Removing manually missing values
# #-------------------------------------------------------------------
#    r <- r[miss_loc]                  # Need to work this to make it automatic
#   # r <- na.omit(r)
#    S <- S[c(miss_loc), c(miss_loc)]
#  outcome <- rep(1:length(rr[[1]]), length(R))
#   outcome <- as.factor(outcome)
#   study <- sort(rep(1:length(R),length(rr[[1]])))
#   outcome <- outcome[miss_loc]
#   study <- study[miss_loc]
# #-------------------------------------------------------------------
# # Random Effects Results
# #-------------------------------------------------------------------
#   res.mR <- rma.mv(r,S, mods = ~ factor(outcome) -1 , random = ~ factor(outcome) | factor(study),
#                    struct="UN", method="REML")
# # summary(res.mR)
#  # with intercept for Q Model
#  res.mR_Qb <- rma.mv(r,S, mods = ~ factor(outcome) , random = ~ factor(outcome)  | factor(study),
#                    struct="UN", method="REML")
# # summary(res.mR_Qb)
# #-------------------------------------------------------------------
# # Fixed Effects Results
# #-------------------------------------------------------------------
#   res.mF <- rma.mv(r,S, mods = ~ factor(outcome) - 1)
#   # summary(res.mF)
# #-------------------------------------------------------------------
# # Extracting elements from output
# #-------------------------------------------------------------------
#     bf <- res.mF$b
#     rvecf <- as.matrix(bf)
#     Bf <- vec2sm(rvecf, diag = FALSE, order = NULL)
#     diag <- 1
#     diag(Bf) <- diag
#     Vf <-  res.mF$vb
#     Bf   # fixed- mean corr
#     Vf   # var-cov fixed mean corr
# #-------------------------------------------------------------------
# # Extracting elements from  Random-effect output
# #-------------------------------------------------------------------
#     br <- res.mR$b
#     rvecr <- as.matrix(br)
#     Br <- vec2sm(rvecr, diag = FALSE, order = NULL)
#     diag <- 1
#     diag(Br) <- diag
#     Vr <-  res.mR$vb
#     Tau2 <-  res.mR$tau2
#     Rho <-  res.mR$rho
#     Br # random - mean corr
#     Vr  # var-cov random mean corr
#     Tau2
#     Rho
#  #------------------
#  # tau matrix
#  #------------------
#   rho <-  vec2sm(Rho, diag = FALSE)
#   diag(rho) <- diag
#   tdiag <- diag(sqrt(Tau2))
#   taumat <- tdiag %*% rho %*% tdiag
#   round(taumat, 4)
# #-------------------------------------------------------------------
# # Computing Stage 2
# #-------------------------------------------------------------------
# # using first column as outcome
#   Rxx <- Br[-1, -1]
#   Rxy <- Br[1, 2:4]
#   B <- solve(Rxx) %*% Rxy
#   B # slopes as cog, somatic and self; outcome perf
#   t(B)%*%Rxy   # R2 for peformance
#    (vpath <- var_path(mean_r = Br, var_cov = Vr)) # Var of the path
#     (z_test <- B/ sqrt(diag(vpath))) # z test
#     2*pnorm(-abs(z_test))   # p value
# # reducing the matrix for the intermediate outcome
#   Br_2 <- Br[-1, -1]
#   Rxx_2 <- Br_2[-3, -3]
#   Rxy_2 <- Br_2[3, 1:2]
#   B_2 <- solve(Rxx_2) %*% Rxy_2
#   B_2  # slopes cog and somatic; outcome self
#   t(B_2)%*%Rxy_2   # R2 for peformance
#   # var of path reduce model
#  vr <-  sm2vec(Vr, diag = TRUE) # vector of mean var-cov
#        vr_2 <- vr[ -1*grep(cl_a[1], cl_v3) ]  # removing performance from mean var-cov
#        vr_2 <- vec2sm(vr_2, diag = TRUE)    # var cov of reduce model
#      (vpath <-  var_path(mean_r = Br[-1,-1], var_cov = vr_2)) # Var of the reduce path
#        (z_test <- B_2/ sqrt(diag(vpath))) # z test
#     2*pnorm(-abs(z_test))   # p value
#
# ####################################################################
# ##################################################################
#
# ####################################################################
#
#
# #-------------------------------------------------------------------
# # Computing Stage 2
# #-------------------------------------------------------------------
#
#   substr(Becker09$Type_of_sport, Team)
#
#
#
#  Becker09$Type_of_sport
#    plyr::llply(Becker09, function(x) as.matrix(unlist(lapply(x, '[[', "Team"))))
#
#   mapply(function(x, y) x[x[, 2] %in% y,], mylist, hislist, SIMPLIFY=FALSE)
#
#
# ####################################################################
# ##################################################################
# #-------------------------------------------------------------------
# # Computing Stage 2 ATTEMPT WITH LAVAAN
# #-------------------------------------------------------------------
#     # library(lavaan)
#     # library(lavaan.survey)
#     # library(survey)
#
#  inv.wei <- solve(Vr )
#  wi.survey <- svydesign(id = ~1, weights = ~inv.wei )
#
#   lower <- '
#   1.00000000
#   -0.03390687  1.00000000
#   -0.07102014  0.54397820  1.00000000
#   0.23285263 -0.45274503 -0.39740584  1.0000000 '
#
#   Becker.cov <-
#     getCov(lower, names = c("performance", "cognitive",
#                             "somatic", "selfconf"))
#   Becker.model <- '
#     selfconf ~ cognitive + somatic
#     performance ~ selfconf + cognitive + somatic '
#
#            inv.Vr <-  sm2vec(solve(Vr), diag = TRUE)
#             v2  <- t(as.matrix(inv.Vr))
#
#    Vr2 <- '
#   0.014980913
#  -0.006039049      0.005185337
#  -0.019740138      0.005947667      0.032953772
#  -0.014531948      0.006255515      0.018044849      0.015212327
#  -0.016562207      0.005726316      0.023328645      0.015398370      0.020826405
#  -0.014166334      0.005642970      0.017371754      0.013636384      0.016245011      0.01537143'
#
#
# fit0 <- sem(Becker.model,
#            sample.cov = Becker.cov,
#           # estimator="WLS",
#           # WLS.V = "inv.Vr",
#           # NACOV = Vr2,
#            sample.nobs = 600)
#   summary(fit0, standardized = TRUE)
#
#   4*(4-1)/2
#
#   fit <- sem(Becker.model,
#            sample.cov = Becker.cov,
#            estimator="WLS",
#            WLS.V = "Vr2",
#           # NACOV = Vr2,
#            sample.nobs = 600)
#   summary(fit, standardized = TRUE)
#
#
#   fit.2 <- lavaan.survey(lavaan.fit=fit, survey.design=wi.survey)
#
# #-------------------------------------------------------------------------------
#   # vectorizing values for reml in metaSEM
#    # lst <- OLw
#   #  plyr::llply(lst, function(x) as.matrix(unlist(lapply(x, '[[', i))))
#
#    attr(dat[[1]][[1]], "dimnames")[[1]]
#  # Br[upper.tri(Br, diag = FALSE)]  <- NA
#  # F1 <- getCov(Br, names =  c("Performance", "Cognitive", "Somatic", "Self_confidence"))
#  #  colnames(Br) <- c("Performance", "Cognitive", "Somatic", "Self_confidence")
#    colnames(Br) <- attr(dat[[1]][[1]], "dimnames")[[1]]
#    # paste('name', 1:7)
#   model0 <-'
#   Self_confidence ~ Cognitive + Somatic
#   Performance ~ Self_confidence + Cognitive + Somatic
#   Cognitive ~~ Somatic
#   Self_confidence ~~ Self_confidence
#   Performance ~~ Performance
#  '
#
#   fit <- sem(model0,
#            sample.cov = Br,
#            sample.nobs = 10)
#   summary(fit)
#
#     X <- diag(ncol(Br))
#    # X <- t(x[, rep(seq_len(ncol(x)),  nrow(Br))])# identity matrix
#     s <- bdiag(Vr)                    # var_cov
#     s <- as.matrix(s)
#     solve( t(X) %*% solve(s) %*% X)
#
#   fit0 <- lavaan2RAM(model0, obs.variables=c("Performance", "Cognitive",
#                                          "Somatic", "Self_confidence"))
#
#    A <- fit0$A
#    S <- fit0$S
#
#
#
# # model1 <- "## Regression paths
# #          Performance ~ Cog2Per*Cognitive + SO2Per*Somatic + SC2Per*Self_confidence
# #          Self_confidence ~ Cog2SC*Cognitive + SO2SC*Somatic
# #          ## Fix the variances of Cog and SO at 1
# #          Cognitive ~~ 1*Cognitive
# #          Somatic ~~ 1*Somatic
# #          ## Label the correlation between Cog and SO
# #          Cognitive ~~ cor*Somatic
# #          ## Label the error variances of Per and SC
# #          Performance ~~ var_Per*Performance
# #          Self_confidence ~~ var_SC*Self_confidence"
#
# #RAM <- lavaan2RAM(model1, obs.variables=c("Performance", "Cognitive",
#                                          # "Somatic", "Self_confidence"))
# #RAM
#
# #A1 <- RAM$A
# #S1 <- RAM$S
#
# #------------------------------------------------------------------
#
#
#
#
#
