#t_n <- c(1,2,4,6,7,8,11,15)
source("utils.R")

getwd()


phi <- c(-0.1, -0.5, -0.9)

n_sims <- c(20, 50, 100)


make_plot <- function() {
  par(mfrow = c(3, 3))
  for (n in n_sims){
    for (p in phi){
      res_sim <- iar_sim(n_sim = n, phi = p, sigma = 1) # nolint
      p2 <- as.character(p)
      n2 <- as.character(n)
      plot(res_sim[, 2], res_sim[, 1], pch = 20, type = "line", xaxt = "n",
           main = bquote(paste(n == .(n2), ", ", phi == .(p2))),
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
      axis(3, at = res_sim[, 2], col = "red", labels = FALSE)
      axis(1, at = seq(0, (max(res_sim[, 2]) + 50), 50), col = "black")
    }
  }
}
postscript("./../informe/Kap3/Fig_Cap3/sim2.eps")
make_plot()
dev.off()




## x es un input con el valor de la serie y el tiempo en que fue medida

calc.xhat <- function(x,phi) {
  n=nrow(x)
  x_hat = c()
  delta_n = diff(x$t_n)
  
  x_hat[1] = 0
  x_hat[2:n] = sign(phi)*abs(phi)^(delta_n)*x$x[1:(n-1)]
  return(x_hat)
  
}
  
calc.w <- function(x,phi,sigma){
  n = nrow(x)
  w <- c()
  delta_n = diff(x$t_n)
  var <- (sigma^2)/(1-phi^2)
  w[1] = var
  w[2:n] =var*(1-sign(phi)^(2)*abs(phi)^(2*delta_n))
  return(w)
}

#calc.w(sim, 0.3, 2)

#x_hat <-calc.xhat(sim, 0.3)

iar.loglik <- function(x, par){
  phi <- par[1]
  sigma <- par[2]
  n = nrow(x)
  x_hat <- calc.xhat(x, phi)
  w = calc.w(x,phi,sigma)
  
  ll = (n/2)*log(2*pi) + (1/2)*sum(log(w)) + (1/2)*sum(((x$x - x_hat)^2)/w)
  return(ll)
  
}



sim <- iar.sim(100000, -0.3, 2)





p <- optim(par = c(0.5, 1),fn = iar.loglik, x= sim)
p
