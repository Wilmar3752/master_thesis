source("utils.R")
data <- read.csv2("./data/mudelse1.10b.csv", sep = "\t", header = FALSE)

head(data)
colnames(data) <- c("t_n", "x")

data$x <- as.numeric(data$x) ## convierto en numericos
data$t_n <- as.numeric(data$t_n)
data$x <- data$x - mean(data$x)
plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = data[, 1], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")

data$x <- data$x - mean(data$x)
##No converge la estimacion

pars <- optim(par = 0,
    fn = iar_loglik, x = data,
    hessian = TRUE,
    method = 'Brent', lower = -0.99, upper = 0.99)
pars
## Hago loess para eliminar tendencia
pars
model <- loess(x ~ t_n, data = data, span = 0.1)

plot(data$t_n, data$x, type="l", main="Serie de tiempo original")
lines(data$t_n, model$fitted, col="red",lwd=3)
data$x <- model$residuals
pars <- optim(par = 0,
    fn = iar_loglik, x = data,
    hessian = TRUE,
    method = 'Brent', lower = -0.99, upper = 0.99)
pars
pred <- calc_xhat(data, pars$par[1])
plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = data[, 1], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")


lines(data$t_n, pred, col="blue")



### Hago diferenciacion
data <- read.csv2("./data/mudelse1.10b.csv", sep = "\t", header = FALSE)

head(data)

colnames(data) <- c("t_n", "x")

data$x <- as.numeric(data$x) ## convierto en numericos
data$t_n <- as.numeric(data$t_n)
data$x <- c(0,diff(data$x))
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
lines(data[, 1], pred
, col = "blue")

