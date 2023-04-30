source("utils.R")


data <- iar_sim(100, phi = -0.7865363, sigma = 16.2046037)
load(file = "data/asth.rda")

data <- data.frame(asth)
summary(data)
#data <- data[data$X1 <= 350,]

colnames(data) <- c("t_n", "x")

head(data)

#data$x <- c(0,diff(data$x))
plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           #main = bquote(paste(n == .(n2), ", ", phi == .(p2))),
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = data[, 1], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")

media <- mean(data$x)
data$x <- data$x - media

pars <- iar_mle(par = c(-0.99, 0.99),
                        fn = iar_loglik,
                        data = data,
                        hessian = TRUE)
pred <- calc_xhat(data, pars$par[1])

lines(data[, 1], pred, col = "blue", lty =2)
pars

###

data <- iar_sim(100, phi = 0.7865363, sigma = 16.2046037)
load(file = "data/V22174.rda")

data <- data.frame(V22174)
summary(data)

colnames(data) <- c("t_n", "x")

head(data)

plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           #main = bquote(paste(n == .(n2), ", ", phi == .(p2))),
           xlab = expression(t[n]), ylab = expression(X[t[n]]),
           ylim=c(-2,2))
axis(3, at = data[, 1], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")

#media <- mean(data$x)
data$x <- data$x

pars <- iar_mle(par = c(-0.99, 0.99),
                        fn = iar_loglik,
                        data = data,
                        hessian = TRUE)
pred <- calc_xhat(data, pars$par[1])

lines(data[, 1], pred, col = "blue", lty =2)

pars

###----------
#install.packages("astsa")
library(astsa)
sangre <- blood
sangre <- data.frame(sangre)

sangre$t_n <- 1:91
sangre <- sangre[!is.na(sangre$WBC),]
nrow(sangre)
plot(sangre[, 4], sangre[, 2], pch = 20, type = "l", xaxt = "n",
           #main = bquote(paste(n == .(n2), ", ", phi == .(p2))),
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = sangre[, 4], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(sangre[, 4]) + 50), 50), col = "black")


colnames(sangre) <- c("y", "x", "z", "t_n")
head(sangre)
sangre$x <- c(0,diff(sangre$x))

media <- mean(sangre$x)
sangre$x <- sangre$x - media
pars <- iar_mle(par = c(-0.99, 0.99),
                        fn = iar_loglik,
                        data = sangre,
                        hessian = TRUE)
pred <- calc_xhat(sangre, pars$par[1])


lines(sangre[, 4], pred, col = "blue", lty =2)

pars





### mudelse

data <- read.csv2("./data/mudelse1.2.csv", sep = "\t", header = FALSE)
head(data)
colnames(data) <- c("t_n", "x")
data$x <- as.numeric(data$x)
plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = data[, 1], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")
data$x <- data$x - mean(data$x)
pars <- iar_mle(par = c(-0.99, 0.99),
                        fn = iar_loglik,
                        data = data,
                        hessian = TRUE)
pars

pred <- calc_xhat(data, 0.1)


#mudelse2

data <- read.csv2("./data/mudelse1.3.csv", sep = "\t", header = FALSE)

head(data)
colnames(data) <- c("t_n", "x")

data$x <- as.numeric(data$x)
x_i <- data$x[1]
data$t_n <- as.numeric(data$t_n)
#data$x <- c(0,diff(data$x))
plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = data[, 1], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")
#data$x<- c(0,diff(data$x))
pars <- iar_mle(par = c(-0.99, 0.99),
                        fn = iar_loglik,
                        data = data,
                        hessian = TRUE)

pars


pred <- calc_xhat(data, pars$par[1])
lines(data[, 1], pred, col = "blue", lty =2)

head(data)

data$x_diff <- c(0,diff(data$x))

write.csv2(data, "example1.3mudelse.csv")





diffinv(pred, xi = x_i)
diffinv(data$x, xi = x_i)

head(data)
model <- loess(x ~ t_n, data=data, span=0.5)

plot(data$t_n, data$x, type="l", main="Serie de tiempo original")
lines(data$t_n, model$fitted, col="red")

data$x <- model$residuals
acf(data$x)
pacf(data$x)

#mudelse1.11b

data <- read.csv2("./data/mudelse1.11.csv", sep = "\t", header = FALSE)
head(data)
colnames(data) <- c("t_n", "x")
data$x <- as.numeric(data$x)
#x_i <- data$x[1]
data$t_n <- as.numeric(data$t_n)
data$x <- c(0,diff(data$x))
plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = data[, 1], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")
#data$x<- c(0,diff(data$x))
pars <- iar_mle(par = c(-0.99, 0.99),
                        fn = iar_loglik,
                        data = data,
                        hessian = TRUE)
pars

pred <- calc_xhat(data, pars$par[1])
lines(data[, 1], pred, col = "blue", lty =2)

diffinv(pred, xi = x_i)
diffinv(data$x, xi = x_i)

head(data)



#mudelse1.11b

data <- read.csv2("./data/mudelse1.11b.csv", sep = "\t", header = FALSE)
head(data)
colnames(data) <- c("t_n", "x")
data$x <- as.numeric(data$x)
#x_i <- data$x[1]
data$t_n <- as.numeric(data$t_n)
#data$x <- c(0,diff(data$x))
plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = data[, 1], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")
#data$x<- c(0,diff(data$x))
pars <- iar_mle(par = c(-0.99, 0.99),
                        fn = iar_loglik,
                        data = data,
                        hessian = TRUE)
pars
pred <- calc_xhat(data, pars$par[1])
lines(data[, 1], pred, col = "blue", lty =2)

diffinv(pred, xi = x_i)
diffinv(data$x, xi = x_i)

head(data)


#mudelse1.2b

data <- read.csv2("./data/mudelse1.2.csv", sep = "\t", header = FALSE)
head(data)
colnames(data) <- c("t_n", "x")
data$x <- as.numeric(data$x)
#x_i <- data$x[1]
data$t_n <- as.numeric(data$t_n)
#data$x <- c(0,diff(data$x))
plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = data[, 1], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")
#data$x<- c(0,diff(data$x))
pars <- iar_mle(par = c(-0.99, 0.99),
                        fn = iar_loglik,
                        data = data,
                        hessian = TRUE)
pars
pred <- calc_xhat(data, pars$par[1])
lines(data[, 1], pred, col = "blue", lty =2)

#mudelse1.4a

data <- read.csv2("./data/mudelse1.4a.csv", sep = "\t", header = FALSE)
head(data)
colnames(data) <- c("t_n", "x")
data$x <- as.numeric(data$x)
#x_i <- data$x[1]
data$t_n <- as.numeric(data$t_n)
#data$x <- c(0,diff(data$x))
plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = data[, 1], col = "red", labels = FALSE)
axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")
#data$x<- c(0,diff(data$x))
pars <- iar_mle(par = c(-0.99, 0.99),
                        fn = iar_loglik,
                        data = data,
                        hessian = TRUE)
pars
pred <- calc_xhat(data, pars$par[1])
lines(data[, 1], pred, col = "blue", lty =2)

#mudelse1.6a
data <- read.csv2("./data/mudelse1.6a.csv", sep = "\t", header = FALSE)
head(data)
colnames(data) <- c("t_n", "x")
data$x <- as.numeric(data$x)
#x_i <- data$x[1]
data$t_n <- as.numeric(data$t_n)
data$x <- c(0,diff(data$x))
plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = data[, 1], col = "red", labels = FALSE)

axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")
#data$x<- c(0,diff(data$x))
pars <- iar_mle(par = c(-0.99, 0.99),
                        fn = iar_loglik,
                        data = data,
                        hessian = TRUE)
pars

pred <- calc_xhat(data, pars$par[1])
lines(data[, 1], pred, col = "blue", lty =2)


#mudelse1.6c
data <- read.csv2("./data/mudelse1.6c.csv", sep = "\t", header = FALSE)
head(data)
colnames(data) <- c("t_n", "x")
data$x <- as.numeric(data$x)
#x_i <- data$x[1]
data$t_n <- as.numeric(data$t_n)
#data$x <- c(0,diff(data$x))
plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = data[, 1], col = "red", labels = FALSE)

axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")
#data$x<- c(0,diff(data$x))
pars <- iar_mle(par = c(-0.99, 0.99),
                        fn = iar_loglik,
                        data = data,
                        hessian = TRUE)
pars

pred <- calc_xhat(data, pars$par[1])
lines(data[, 1], pred, col = "blue", lty =2)


#mudelse1.9
data <- read.csv2("./data/mudelse1.9.csv", sep = "\t", header = FALSE)
head(data)
colnames(data) <- c("t_n", "x")
data$x <- as.numeric(data$x)
#x_i <- data$x[1]
data$t_n <- as.numeric(data$t_n)
#data$x <- c(0,diff(data$x))
plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = data[, 1], col = "red", labels = FALSE)

axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")
#data$x<- c(0,diff(data$x))
pars <- iar_mle(par = c(-0.99, 0.99),
                        fn = iar_loglik,
                        data = data,
                        hessian = TRUE)
pars

pred <- calc_xhat(data, pars$par[1])
lines(data[, 1], pred, col = "blue", lty =2) 

#mudelse1.10a
data <- read.csv2("./data/mudelse1.10a.csv", sep = "\t", header = FALSE)
head(data)
colnames(data) <- c("t_n", "x")
data$x <- as.numeric(data$x)
#x_i <- data$x[1]
data$t_n <- as.numeric(data$t_n)
#data$x <- c(0,diff(data$x))
plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = data[, 1], col = "red", labels = FALSE)

axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")
#data$x<- c(0,diff(data$x))
pars <- iar_mle(par = c(-0.99, 0.99),
                        fn = iar_loglik,
                        data = data,
                        hessian = TRUE)
pars

pred <- calc_xhat(data, pars$par[1])
lines(data[, 1], pred, col = "blue", lty =2) 

#mudelse1.10b
data <- read.csv2("./data/mudelse1.10b.csv", sep = "\t", header = FALSE)
head(data)
colnames(data) <- c("t_n", "x")
data$x <- as.numeric(data$x)
#x_i <- data$x[1]
data$t_n <- as.numeric(data$t_n)
data$x <- c(0,diff(data$x))
plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = data[, 1], col = "red", labels = FALSE)

axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")
#data$x<- c(0,diff(data$x))
pars <- iar_mle(par = c(-0.99, 0.99),
                        fn = iar_loglik,
                        data = data,
                        hessian = TRUE)
pars

pred <- calc_xhat(data, pars$par[1])
lines(data[, 1], pred, col = "blue", lty =2) 

#mudelse8.8ab ##OJO AQUI
data <- read.csv2("./data/mudelse8.8a.csv", sep = "\t", header = FALSE)
head(data)
colnames(data) <- c("t_n", "x")
data$x <- as.numeric(data$x)
#x_i <- data$x[1]
data$t_n <- as.numeric(data$t_n)
media <- mean(data$x)
data$x <- data$x - media
plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = data[, 1], col = "red", labels = FALSE)

axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")
#data$x<- c(0,diff(data$x))
pars <- iar_mle(par = c(-0.99, 0.99),
                        fn = iar_loglik,
                        data = data,
                        hessian = TRUE)
pars

pred <- calc_xhat(data, pars$par[1]) 
lines(data[, 1], pred, col = "blue", lty =2) 

#mudelse8.8b ##
data <- read.csv2("./data/mudelse8.8b.csv", sep = "\t", header = FALSE)
head(data)
colnames(data) <- c("t_n", "x")
data$x <- as.numeric(data$x)
#x_i <- data$x[1]
data$t_n <- as.numeric(data$t_n)
data$x <- data$x - media
#data$x <- c(0,diff(data$x))
plot(data[, 1], data[, 2], pch = 20, type = "l", xaxt = "n",
           xlab = expression(t[n]), ylab = expression(X[t[n]]))
axis(3, at = data[, 1], col = "red", labels = FALSE)

axis(1, at = seq(0, (max(data[, 1]) + 50), 50), col = "black")
#data$x<- c(0,diff(data$x))
pars <- iar_mle(par = c(-0.99, 0.99),
                        fn = iar_loglik,
                        data = data,
                        hessian = TRUE)
pars

pred <- calc_xhat(data, pars$par[1])
lines(data[, 1], pred, col = "blue", lty =2) 


