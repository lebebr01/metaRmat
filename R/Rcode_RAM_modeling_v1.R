## Proposed path model 
model <- "## Regression paths
          Performance ~ Cognitive + Somatic + Self_confidence
          Self_confidence ~ Cognitive + Somatic
          ## Fix the variances of Cog and SO at 1
          Cognitive ~~ 1*Cognitive
          Somatic ~~ 1*Somatic
          Cognitive ~~ Somatic"

RAM <- lavaan2RAM(model, obs.variables=c("Performance", "Cognitive",
                                         "Somatic", "Self_confidence"))


RAM
#------------------------------------------------------
  A <- RAM$A
  F <- RAM$F
  S <- RAM$S
  I <- diag(ncol(A))

 C.fun <- function(x){
        out <- F %*% solve(I-A) %*% S %*% t(solve(I-A)) %*% t(F)
        return(out)
        }
