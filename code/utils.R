
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
calc_w_fit <- function(x, phi) {
  n <- nrow(x)
  w <- c()
  sigma <- calc_sigma(x, phi)
  delta_n <- diff(x$t_n)
  var <- (sigma) / (1 - phi^2)
  w[1] <- var
  w[2:n] <- var * (1 - sign(phi)^(2) * abs(phi)^(2 * delta_n))
  return(w)
}

calc_phi_n <- function(phi, delta_n) {
  phi_n <- sign(phi) * abs(phi)^delta_n
  return(phi_n)
}
calc_sigma <- function(x, phi) {
  x_hat <- calc_xhat(x, phi)
  delta_n <- diff(x$t_n)
  n <- nrow(x)
  k <- (1 - phi^2) / n
  e <- x$x - x_hat
  sigma <- k * e[1]^2 + k * sum(e[2:n]^2 / (1 - (phi^2)^(delta_n)))
  return(sigma)
}


iar_loglik <- function(x, phi) {
  w <- calc_w_fit(x, phi)
  ll <- (1 / 2) * sum(log(w))
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

iar_mle <- function(par = 0,
                    fn = iar_loglik, data,
                    hessian = TRUE,
                    method = "Brent") {
    p <-  optim(par = par,
          fn = fn, x = data,
          hessian = hessian,
          method = 'Brent', lower = -0.99, upper = 0.99)
    par <- p$par
    return(par)
}

iar_boot_est <- function(x, num_bootstrap_trayetories) {
  delta_n <- diff(x$t_n)
  mle <- iar_mle(fn = iar_loglik,
                data = x,
                hessian = TRUE)
  par <- mle
  print('llega')
  sigma <- calc_sigma(x, par)
  w_n <- calc_w(delta_n, par, sigma)
  x_hat <- calc_xhat(x, par)
  innovations <- calc_innovations(x, x_hat = x_hat, w_n = w_n)
  phi_n <- calc_phi_n(par, delta_n)
  phi_boost <- 0
  #sigma_boost <- 0

  for (i in 1:num_bootstrap_trayetories) {
      innovation_sample <- 0
      innovation_sample <- sample(innovations, replace = TRUE)
      new_x <- calc_new_x(innovation_sample, w_n, phi_n)
      print(i)
      x$x <- new_x
      p <- iar_mle(fn = iar_loglik, data = x, hessian = FALSE)
      par <- p
      phi_boost[i] <- par
      #sigma_boost[i] <- par[2]
}
return(cbind(phi_boost))
}

calc_var_pred <- function(x, phi){
  sigma <- calc_sigma(x, phi)
  delta_n <- diff(x$t_n)
  a <- (phi^2)^delta_n
  b <- sigma / (1 - phi^2)
  var_pred <- a * b
  return(var_pred)
}