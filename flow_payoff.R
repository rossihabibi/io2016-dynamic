# computes the flow payoff part of the value function, for a given x

# testing :
# x = 1

flow_payoff <- function(x, Omega, epsilon){
  obs <- 2*Omega[,2] - 3*(Omega[,2] > 0)*(Omega[,3] == 0) - x*Omega[,1]
  obs <- obs %*% t(rep(1, nsim)) + rep(1, n) %*% t(epsilon[x + 1,])
  obs
}

flow_payoff_noeps <- function(x, Omega){ # does not use simulated epsilons
  obs <- 2*Omega[,2] - 3*(Omega[,2] > 0)*(Omega[,3] == 0) - x*Omega[,1]
  obs <- obs %*% t(rep(1, 1))
  obs
}