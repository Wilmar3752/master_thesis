#install.packages("BINCOR")
source('utils.R')

library(BINCOR)
data(N)
data <- NHSST.dat

head(data)
colnames(data) <- c("t_n", "x")
data$x <- as.numeric(data$x) ## convierto en numericos
data$t_n <- as.numeric(data$t_n)


plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = data[, 1], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")

##No converge la estimacion

pars <- iar_mle(par = c(0.1, 1),
                        fn = iar_loglik,
                        data = data,
                        hessian = TRUE)
pars
pred <- calc_xhat(data, pars$par[1])
lines(data$t_n, pred, col="red", lty = 1,lwd=1)


### Hago diferenciacion
data <- NHSST.dat

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
pars <- iar_mle(par = c(-0.99, 0.99),
                        fn = iar_loglik,
                        data = data,
                        hessian = TRUE)
pars
pred <- calc_xhat(data, pars$par[1])

lines(data[, 1], pred, col = "red", lwd=1)
boot_est <- iar_boot_est(data, num_bootstrap_trayetories = 500)
boot_est <- data.frame(boot_est)

