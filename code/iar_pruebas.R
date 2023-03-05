source("utils.R")

phi <- c(-0.1, -0.5, -0.9, 0.1, 0.5, 0.9)
sigma <- 1
n_sim <- 1000
#sim <- read.csv("val_data.csv")

sim <- iar_sim(n_sim, phi, sigma)
#sim
p <- iar_mle(par = c(-0.99, 0.99), fn = iar_loglik, data = sim, hessian = TRUE)
p
par <- p$par
stde  <- p$stde
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
p <- iar(par = c(0.5, 1), fn = iar_loglik, x = sim)
par <- p$par

phi_boost <- 0
sigma_boost <- 0
for (i in 1:100) {
    innovation_sample <- 0
    innovation_sample <- sample(innovations, replace = TRUE)
    new_x <- calc_new_x(innovation_sample, w_n, phi_n)
    sim$x <- new_x
    p <- iar_mle(par = c(0.5, 1), fn = iar_loglik, data = sim, hessian = TRUE)
    par <- p$par
    phi_boost[i] <- par[1]
    sigma_boost[i] <- par[2]
}
d <- density(phi_boost)
plot(d)
