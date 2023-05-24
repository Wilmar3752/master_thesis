#install.packages("iAR")
library(iAR)
data(Planets)
head(Planets)
data <- Planets
head(data)
colnames(data) <- c("t_n", "x")
data$x <- as.numeric(data$x) ## convierto en numericos
data$t_n <- as.numeric(data$t_n)

#data$x <- data$x - mean(data$x)

plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = data[, 1], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")


pars <- optim(par = 0,
    fn = iar_loglik, x = data,
    hessian = TRUE,
    method = 'Brent', lower = -0.99, upper = 0.99)
pars
pred <- calc_xhat(data, pars$par[1])
lines(data$t_n, pred, col="red", lty = 1,lwd=1)


## Hago loess para eliminar tendencia
pars
model <- loess(x ~ t_n, data = data, span = 0.1)
plot(data$t_n, data$x, type="l", main="Serie de tiempo original")
lines(data$t_n, model$fitted, col="red",lwd=2)

hist(model$residuals)
data$x <- model$residuals


pars <- optim(par = 0,
    fn = iar_loglik, x = data,
    hessian = TRUE,
    method = 'Brent', lower = -0.99, upper = 0.99)
pars
pred <- calc_xhat(data, pars$par[1])
plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]), col='gray')
axis(3, at = data[, 1], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")


lines(data$t_n, pred, col="red")
head(data)

res <- data$x - pred
hist(res)
acf(res)



### Hago diferenciacion
data <- Planets

head(data)

colnames(data) <- c("t_n", "x")

data$x <- as.numeric(data$x) ## convierto en numericos
data$t_n <- as.numeric(data$t_n)
data$x <- c(0,diff(data$x))

#postscript("./../informe/Kap3/Fig_Cap3/example_data_diff_estimation.eps")

plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]),col='gray', lwd=3)
#axis(3, at = data[, 1], col = "red", labels = FALSE)
#axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")
pars <- optim(par = 0,
    fn = iar_loglik, x = data,
    hessian = TRUE,
    method = 'Brent', lower = -0.99, upper = 0.99)
pars
pred <- calc_xhat(data, pars$par[1])
lines(data[, 1], pred, col = "red", lwd=1)
boot_est <- iar_boot_est(data, num_bootstrap_trayetories = 500)
boot_est <- data.frame(boot_est)
apply(boot_est, 2, mean)
apply(boot_est, 2, sd)