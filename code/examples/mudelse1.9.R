source("utils.R")
data <- read.csv2("./data/mudelse1.9.csv", sep = "\t", header = FALSE)

head(data)
colnames(data) <- c("t_n", "x")

data$x <- as.numeric(data$x) ## convierto en numericos
data$t_n <- as.numeric(data$t_n)

plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = data[, 1], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")

##No converge la estimacion

pars <- iar_mle(par = c(-0.99, 0.99),
                        fn = iar_loglik,
                        data = data,
                        hessian = TRUE)
## Hago loess para eliminar tendencia
pars
model <- loess(x ~ t_n, data = data, span = 0.1)
data$x <- model$residuals

pars <- iar_mle(par = c(-0.99, 0.99),
                        fn = iar_loglik,
                        data = data,
                        hessian = TRUE)
pars
pred <- calc_xhat(data, pars$par[1])
plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = data[, 1], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")

lines(data$t_n, pred, col="blue")


### Hago diferenciacion
data <- read.csv2("./data/mudelse1.9.csv", sep = "\t", header = FALSE)

head(data)

colnames(data) <- c("t_n", "x")

data$x <- as.numeric(data$x) ## convierto en numericos
data$t_n <- as.numeric(data$t_n)
data$x <- c(0,diff(data$x))
plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = data[, 1], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")
pars <- iar_mle(par = c(-0.99, 0.99),
                        fn = iar_loglik,
                        data = data,
                        hessian = TRUE)
pars
pred <- calc_xhat(data, pars$par[1])
lines(data[, 1], pred
, col = "blue")
