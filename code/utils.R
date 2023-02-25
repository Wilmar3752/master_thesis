
iar_sim <- function(n_sim, phi = 0.7, sigma = 1) {
  e <- rnorm(n_sim)
  x <- rep(0, n_sim)
  delta_n <- 1 + rpois(n_sim - 1, 2)
  t_n <- diffinv(delta_n, xi = 1)
  phi_n <- calc_phi_n(phi, delta_n)
  w_n <- calc_w(delta_n, phi, sigma)
  for (t in 1:n_sim){
    if (t == 1) {
      x[t] <- sqrt(w[t]) * e[t]
    } else {
      x[t] <- phi_n[t - 1] * x[t - 1] +
      sqrt(w_n[t]) * e[t]
    }
  }
  return(data.frame(cbind(x, t_n)))
}

calc_xhat <- function(x, phi) {
  n <- nrow(x)
  x_hat <- c()
  delta_n <- diff(x$t_n)
  x_hat[1] <- 0
  x_hat[2:n] <- sign(phi) * abs(phi)^(delta_n) * x$x[1:(n - 1)]
  return(x_hat)
}

calc_w <- function(delta_n, phi, sigma) {
  n <- length(delta_n) + 1
  w <- c()
  var <- (sigma^2) / (1 - phi^2)
  w[1] <- var
  w[2:n] <- var * (1 - sign(phi)^(2) * abs(phi)^(2 * delta_n))
  return(w)
}
calc_phi_n <- function(phi, delta_n) {
  phi_n <- sign(phi) * abs(phi)^delta_n
  return(phi_n)
}

iar_loglik <- function(x, par) {
  phi <- par[1]
  sigma <- par[2]
  n <- nrow(x)
  x_hat <- calc_xhat(x, phi)
  delta_n <- diff(x$t_n)
  w <- calc_w(delta_n, phi, sigma)
  ll <- (n / 2) * log(2 * pi) +
        (1 / 2) * sum(log(w)) +
        (1 / 2) * sum(((x$x - x_hat)^2) / w)
  return(ll)
}
