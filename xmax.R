# computes maximum of function among x in X for a given flow payoff, future payoff

# V <- V0
# rm(V)

xmax <- function(Omega, epsilon, PiOmega, V){
  forX0 <- flow_payoff(x = 1, Omega = Omega, epsilon = epsilon) + future_payoff(x = 1, PiOmega = PiOmega, V = V)
  forX1 <- flow_payoff(x = 1, Omega = Omega, epsilon = epsilon) + future_payoff(x = 1, PiOmega = PiOmega, V = V)
  (forX0 - forX1 > 0)*forX0 + (1- (forX0 - forX1 > 0))*forX1
}