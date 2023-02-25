source("utils.R")
phi <- 0.7
sigma <- 2
n_sim <- 1000

sim <- iar_sim(n_sim, phi, sigma)
p <- optim(par = c(0.5, 1), fn = iar_loglik, x = sim)
p
