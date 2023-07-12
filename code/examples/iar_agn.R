#install.packages("iAR")
source("utils.R")
library(optimx)
library(iAR)
data(agn)
head(agn)
data <- agn
head(data)
colnames(data) <- c("t_n", "x", 'merr')
data$x <- as.numeric(data$x) ## convierto en numericos
data$t_n <- as.numeric(data$t_n)

data$x <- data$x - mean(data$x)

postscript("./../informe/Kap3/Fig_Cap3/agn_data.eps")

plot(data[, 1], data[, 2], pch = 20, type = "p", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]),
           col="blue")
lines(data[, 1], data[, 2], type = "l")
axis(3, at = data[, 1], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")

dev.off()

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
postscript("./../informe/Kap3/Fig_Cap3/agn_loess.eps")

plot(data$t_n, data$x, type="l", main="Serie de tiempo original",
xlab=expression(t[n]), ylab = expression(X[t[n]]))
lines(data$t_n, model$fitted, col="red",lwd=2)
dev.off()
hist(model$residuals)

data$x <- model$residuals

pars <- optim(par = 0,
    fn = iar_loglik, x = data,
    hessian = TRUE,
    method = 'Brent', lower = -0.9999, upper = 0.999)
pars
pred <- calc_xhat(data, pars$par[1])
postscript("./../informe/Kap3/Fig_Cap3/agn_ajuste.eps")

plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]), col='black')
axis(3, at = data[, 1], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")
lines(data$t_n, pred, col="red")
dev.off()
head(data)


pars$par


sqrt(calc_sigma(data, pars$par))

boost <- iar_boot_est(data,500)
mean(boost)
sd(boost)


postscript("./../informe/Kap3/Fig_Cap3/agn_ajuste.eps")

plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]), col='black',
           ylim=c(-1.556e-16-3*5.866354e-17,1.782e-16+3*5.866354e-17), )
axis(3, at = data[, 1], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")
lines(data$t_n, pred, col="red")
lines(data$t_n, pred + 2*0.06134056, col="red")
dev.off()


res <- data$x - pred

postscript("./../informe/Kap3/Fig_Cap3/agn_ajuste_bandas.eps")
plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]), col='gray' )
axis(3, at = data[, 1], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")
lines(data$t_n, pred, col="red")
vars_pred <- calc_var_pred(data, pars$par)
lines(data$t_n, pred + 2*sqrt(vars_pred), col="blue", lty=2)
lines(data$t_n, pred - 2*sqrt(vars_pred), col="blue", lty=2)

dev.off()

postscript("./../informe/Kap3/Fig_Cap3/agn_acf.eps")
acf(res)
dev.off()

postscript("./../informe/Kap3/Fig_Cap3/agn_qqplot.eps")

qqnorm(res, pch=20)
qqline(res, col=2)
dev.off()


n2 <- length(res)
ljung_mat <- matrix(0, nrow = (n2), ncol = 1)

for (i in 1:n2)
{
  box <- 
    Box.test(
      res,
      lag = i,
      type = "Ljung-Box")
  ljung_mat[i, ] <- box$p.value 
}
postscript("./../informe/Kap3/Fig_Cap3/agn_ljung.eps")

plot(ljung_mat, pch=20, ylab= 'p-value', xlab="lag", main="Ljung-box test")
abline(h=0.01, col=2, lty=2)
dev.off()




calc_sigma(data,pars$par)
