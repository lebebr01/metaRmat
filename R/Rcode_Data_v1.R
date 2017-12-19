  setwd("H:/BeckerAloe")
  dat <- read.csv("Becker09.csv")
  L1 <- vec2sm( as.matrix(dat[1,c(4:ncol(dat))]), diag = FALSE)
    diag(L1) <- 1