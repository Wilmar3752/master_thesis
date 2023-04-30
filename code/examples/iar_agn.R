#install.packages("iAR")
source("utils.R")
library(iAR)
data(agn)
head(agn)
data <- agn
head(data)
colnames(data) <- c("t_n", "x", 'merr')
data$x <- as.numeric(data$x) ## convierto en numericos
data$t_n <- as.numeric(data$t_n)

#data$x <- data$x - mean(data$x)

plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = data[, 1], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")


pars <- iar_mle(par = c(-0.99, 0.99),
                        fn = iar_loglik,
                        data = data,
                        hessian = FALSE)
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


pars <- iar_mle(par = c(-0.99, 0.99),
                        fn = iar_loglik,
                        data = data,
                        hessian = TRUE)
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


