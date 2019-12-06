#-------------------------------------------
# raw correlations -- MATH
#-------------------------------------------
Tracz.r <- corpcor::vec2sm(c(0.14, 0.62,0.22), diag = FALSE)
diag(Tracz.r) <- 1
rownames(Tracz.r) <- colnames(Tracz.r) <- c("ACH", "PTE", "GTE")

Sheft.r <- corpcor::vec2sm(c(-0.01, 0.19, 0.27), diag = FALSE)
diag(Sheft.r) <- 1
rownames(Sheft.r) <- colnames(Sheft.r) <- c("ACH", "PTE", "GTE")

Wats1.r <- corpcor::vec2sm(c(0.2437, 0.1394, 0.1768), diag = FALSE)
diag(Wats1.r) <- 1
rownames(Wats1.r) <- colnames(Wats1.r) <- c("ACH", "PTE", "GTE")

Wats2.r <- corpcor::vec2sm(c(-0.1127, 0.2122, 0.5096), diag = FALSE)
diag(Wats2.r) <- 1
rownames(Wats2.r) <- colnames(Wats2.r) <- c("ACH", "PTE", "GTE")

Gresh.r <- corpcor::vec2sm(c(0.140, -0.300, 0.2941), diag = FALSE)
diag(Gresh.r) <- 1
rownames(Gresh.r) <- colnames(Gresh.r) <- c("ACH", "PTE", "GTE")

Ande1.r <- corpcor::vec2sm(c(0.320, 0.160, 0.2941), diag = FALSE)
diag(Ande1.r) <- 1
rownames(Ande1.r) <- colnames(Ande1.r) <- c("ACH", "PTE", "GTE")

Ande2.r <- corpcor::vec2sm(c(0.080, 0.240, 0.2941), diag = FALSE)
diag(Ande2.r) <- 1
rownames(Ande2.r) <- colnames(Ande2.r) <- c("ACH", "PTE", "GTE")
#--------------------------------------------------------------
n <- c(448, 1057, 929, 608, 1250, 288, 416)
math_correlations <-list(Tracz.r = Tracz.r, Sheft.r = Sheft.r,
                         Wats1.r = Wats1.r, Wats2.r = Wats2.r,
                         Gresh.r = Gresh.r, Ande1.r = Ande1.r,
                         Ande2.r = Ande2.r)
math_correlations <- list(math_correlations = math_correlations,
                       sample_size = n)
#--------------------------------------------------------------------

save(math_correlations, file = 'data/math_correlations.rda')
