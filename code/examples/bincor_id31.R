#install.packages("BINCOR")
source('utils.R')

library(BINCOR)
#data(N)
data <- ID31.dat

head(data)
colnames(data) <- c("t_n", "x")
data$x <- as.numeric(data$x) ## convierto en numericos
data$t_n <- as.numeric(data$t_n)


plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = data[, 1], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")

data$x <- data$x - mean(data$x)
##No converge la estimacion

pars <- optim(par = 0,
    fn = iar_loglik, x = data,
    hessian = TRUE,
    method = 'Brent', lower = -0.9, upper = 0.99)
pars
pars
pred <- calc_xhat(data, pars$par[1])

lines(data$t_n, pred, col="red", lty = 1,lwd=1)


### Hago diferenciacion
data <- ID31.dat

head(data)

colnames(data) <- c("t_n", "x")


data$x <- as.numeric(data$x) ## convierto en numericos
data$t_n <- as.numeric(data$t_n)
data$x <- c(0,diff(data$x))

#postscript("./../informe/Kap3/Fig_Cap3/example_data_diff_estimation.eps")

plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]),col='gray', lwd=3)
axis(3, at = data[, 1], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")
pars <- optim(par = 0,
    fn = iar_loglik, x = data,
    hessian = TRUE,
    method = 'Brent', lower = -0.9, upper = 0.99)
pars
pred <- calc_xhat(data, pars$par[1])

lines(data[, 1], pred, col = "red", lwd=1)
boot_est <- iar_boot_est(data, num_bootstrap_trayetories = 500)
boot_est <- data.frame(boot_est)


phi = 0.99
iar_loglik(data, -0.99)
calc_w_fit(data, -0.99)
calc_sigma(data,-0.99)
x_hat <-calc_xhat(data,-0.99)
delta_n <- diff(data$t_n)
 n <- nrow(data)
 k <- (1 - phi^2) / n
e <- data$x - x_hat
k * e[1]^2 
e[2:n]^2 / (1 - phi^(2 * delta_n))
delta_n
(1 - (phi^2)^(375.1))
((-0.99)^2)^375.1
(1 - phi^(2 * 375.1))



delta_n
##residuales
res <- data$x - pred
hist(res)
acf(res)
plot.ts(res)
