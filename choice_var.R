choice_var <- function(p, c, i, Omega, PiOmega, V){ #state variable (p, c, i)
  X <- c(0,1)
  eps <- rgumbel(2)
  s <- intersect(intersect(which(Omega[,1] == p), which(Omega[,2] == c)), which(Omega[,3] == i))
  
  payoff <- 2*Omega[s,2] - 3*(Omega[s,2] > 0)*(Omega[s,3] == 0) - X*Omega[s,1] + eps + beta*as.vector(rbind(PiOmega[s,], PiOmega[s + n,]) %*% V)
  as.numeric(payoff[2] - payoff[1] > 0)
}