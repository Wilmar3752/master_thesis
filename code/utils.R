
iar_sim <- function(n_sim, phi = 0.7, sigma = 1) {
  e <- rnorm(n_sim)
  x <- rep(0, n_sim)
  delta_n <- 1 + rpois(n_sim - 1, 2)
  t_n <- diffinv(delta_n, xi = 1)
  phi_n <- calc_phi_n(phi, delta_n)
  w_n <- calc_w(delta_n, phi, sigma)
  for (t in 1:n_sim){
    if (t == 1) {
      x[t] <- sqrt(w_n[t]) * e[t]
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

calc_innovations <- function(x, x_hat, w_n) {
e <- (x$x - x_hat) / sqrt(w_n)
innovation <- e - mean(e)
return(innovation)
}

calc_new_x <- function(innovations, w_n, phi_n) {
  x_new <- c()
  n <- length(innovations)
  x_new[1] <- innovations[1] * sqrt(w_n[1])
  for (i in 2:n) {
      x_new[i] <- phi_n[i - 1] * x_new[i - 1] + sqrt(w_n[i]) * innovations[i]
  }
  return(x_new)
}

iar_mle <- function(par, fn, data, hessian) {
    p <- optim(par = par, fn = fn, x = data, hessian = hessian)
    par <- p$par
    stde <- sqrt(diag(solve(p$hessian)))
    mle_list <- list("par" = par, "stde" = stde)
    return(mle_list)
}

iar_boot_est <- function(x, num_bootstrap_trayetories) {
  delta_n <- diff(x$t_n)
  mle <- iar_mle(par = c(-0.99, 0.99),
                fn = iar_loglik,
                data = sim,
                hessian = TRUE)
  par <- mle$par
  w_n <- calc_w(delta_n, par[1], par[2])
  x_hat <- calc_xhat(sim, par[1])
  innovations <- calc_innovations(sim, x_hat = x_hat, w_n = w_n)
  phi_n <- calc_phi_n(par[1], delta_n)
  phi_boost <- 0
  sigma_boost <- 0

  for (i in 1:num_bootstrap_trayetories) {
      innovation_sample <- 0
      innovation_sample <- sample(innovations, replace = TRUE)
      new_x <- calc_new_x(innovation_sample, w_n, phi_n)
      sim$x <- new_x
      p <- iar_mle(par = c(0.5, 1), fn = iar_loglik, data = sim, hessian = TRUE)
      par <- p$par
      phi_boost[i] <- par[1]
      sigma_boost[i] <- par[2]
}
return(phi_boost)
}