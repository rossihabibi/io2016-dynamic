# computes the next iteration k+1 value function for each element in Omega

# V <- V0
# rm(V)

val_func_k1 <- function(Omega, epsilon, PiOmega, V){
  g_epsilon <- dgumbel(epsilon)
  int <- xmax(Omega, epsilon, PiOmega, V) * (rep(1, n) %*% t((g_epsilon[1,]*g_epsilon[2,])))
  matrix((1/nsim)*rowSums(int), nrow = n, ncol = 1)
}

val_func_k1_noeps <- function(Omega, PiOmega, V){
  log(exp(flow_payoff_noeps(x = 0, Omega = Omega) + beta*future_payoff_noeps(x = 0, PiOmega = PiOmega, V = V)) + exp(flow_payoff_noeps(x = 1, Omega = Omega) + beta*future_payoff_noeps(x = 1, PiOmega = PiOmega, V = V))) + euler
}