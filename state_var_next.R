state_var_next <- function(x, p, c, i, Omega, PiOmega){
  s <- intersect(intersect(which(Omega[,1] == p), which(Omega[,2] == c)), which(Omega[,3] == i))
  Omega[sample(x = seq(1:68), size = 1, prob = PiOmega[x*n + s, ]),]
}