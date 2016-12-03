#install.packages("evd")
rm(list = ls())
require(evd)
set.seed(1234)

source(file = "flow_payoff.R")
source(file = "future_payoff.R")
source(file = "xmax.R")
source(file = "val_func_iter.R")
source(file = "val_func.R")

euler <- -digamma(1)
beta <- 0.99

i0 <- 2
ibar <- 4
X <- c(0, 1)
C <- c(0, 1/4)
P <- c(2, 1/2)
I <- seq(0, ibar, 1/4)

n <- length(P)*length(C)*length(I)
Omega <- cbind(rep(P, times = c(n/2, n/2)), rep(rep(C, times = c(n/4,n/4)), times = 2), rep(I, length.out = n))

nsim <- 21

epsilon <- matrix(data = rgumbel(nsim), nrow = length(X), ncol = nsim)


PiP <- matrix(c(0.75, 0.95, 0.25, 0.05), nrow = 2, ncol = 2)
PiC <- matrix(c(1/2, 1/2), nrow = 1, ncol = 2)
PiIcondCX0 <- rbind(diag(nrow = length(I), ncol = length(I)), diag(nrow = length(I) + 1 , ncol = length(I) + 1)[-18,-1])
PiIcondCX0[18,1] <- 1 #if x = 0, c = 1 (row 19), then if i = 0, i' = 0 instead of i' = i - 1/4

PiIcondCX1 <- rbind(diag(nrow = length(I) + 4, ncol = length(I) + 4)[5:21,1:17], diag(nrow = length(I) + 3, ncol = length(I) + 3)[4:20,1:17])
PiIcondCX1[14:17,17] <- 1 #if x = 1, c = 0 then if i > 3  (rows 15 to 18), then i' = 4 instead of i' = i + 1 > ibar
PiIcondCX1[32:34,17] <- 1 #if x = 1, c = -1/4 then if i > 3.25  (rows 34 to 36), then i' = 4 instead of i' = i + .75 > ibar

PiOmega <- rbind(kronecker(PiP, kronecker(PiC, PiIcondCX0)), kronecker(PiP, kronecker(PiC, PiIcondCX1)))
#rowSums(PiOmega) # transition proba matrix OK : rows 1 to n = 72 -> conditional on X = 0, rows n + 1 = 73 to 2*n = 144
#which(rowSums(PiIcondCX1) == 0)

Iter <- 0
V0 <- matrix(data = 0, nrow = dim(Omega)[1], ncol = 1)
V <- val_func_noeps(Omega, PiOmega, V0 = V0)
