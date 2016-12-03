# computes the future payoff part of the value function, for a given x

# testing
 # x <- 1
 # V <- V0

future_payoff <- function(x, PiOmega, V){
  start.index <- x*n+1
  end.index <-  x*n+n
  PiOmegacondX <- PiOmega[start.index : end.index , ]
  V <- V %*% rep(1, nsim) #V needs to be a column matrix as V0 was defined in val_func
  PiOmegacondX %*% V
}


future_payoff_noeps <- function(x, PiOmega, V){ #formats V to be consistent with the no simulated epsilon case
  start.index <- x*n+1
  end.index <-  x*n+n
  PiOmegacondX <- PiOmega[start.index : end.index , ]
  V <- V %*% rep(1, 1) #V needs to be a column matrix as V0 was defined in val_func
  PiOmegacondX %*% V
}