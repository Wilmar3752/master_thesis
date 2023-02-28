
source("utils.R")

phi <- 0.7
sigma <- 1
n_sim <- 10
sim <- read.csv("val_data.csv")

#sim <- iar_sim(n_sim, phi, sigma)
#sim
p <- optim(par = c(0.5, 1), fn = iar_loglik, x = sim)
par <- p$par
delta_n <- diff(sim$t_n)
calc_phi_n(phi, delta_n)


phi_n <- calc_phi_n(par[1], delta_n)

calc_w(delta_n, phi, sigma)

w_n <- calc_w(delta_n, par[1], par[2])
x_hat <- calc_xhat(sim, par[1])

innovations <- calc_innovations(sim, x_hat = x_hat, w_n = w_n)

### remuestrear innovaciones con reemplazo y calclar n veces
new_x <- calc_new_x(innovations, w_n, phi_n)
sim$x <- new_x
p <- optim(par = c(0.5, 1), fn = iar_loglik, x = sim)
par <- p$par
par
#write.csv(sim, "val_data.csv")
#phi
#sim


