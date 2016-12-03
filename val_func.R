## Computes the value function through the contraction mapping

val_func_noeps <- function(Omega, PiOmega, tol = 1e-9, V0, max.iter = 10000){

iter <- 0
Vkminus1 <- V0
Vk <- val_func_k1_noeps(Omega, PiOmega, V0)
while( (max(abs(Vk - Vkminus1)) > tol) & (iter < max.iter) ){
  Vkminus1 <- Vk
  Vk <- val_func_k1_noeps(Omega, PiOmega, Vk)
  
  iter <- iter + 1
}
Iter <<- iter
Vk
}


# iter <- 0
# tol <- 1e-9
# Vkminus1 <- V0
# Vk <- val_func_k1(Omega, epsilon, PiOmega, V0)
# while( (max(abs(Vk - Vkminus1)) > tol) & (iter < 100) ){
#   Vkminus1 <- Vk
#   Vk <- val_func_k1(Omega, epsilon, PiOmega, Vk)
#   
#   iter <- iter + 1
# }