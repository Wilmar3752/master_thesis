source("utils.R")

m <- 1000
mle_phi <- 0
mle_se <- 0
bte_phi <- 0
bte_se <- 0

for (i in 1:m){
    n <- 100
    phi <-  0.1
    sigma <- 1

    sim <- iar_sim(n_sim = n, phi = phi, sigma = sigma)
    mle <- iar_mle(par = c(-0.99, 0.99),
                fn = iar_loglik,
                data = sim,
                hessian = TRUE)
    boot_est <- iar_boot_est(sim, 500)

    mle_phi[i] <- mle$par[1]
    mle_se[i] <- mle$stde[1]
    bte_phi[i] <- mean(boot_est)
    bte_se[i] <- sd(boot_est)

}
df_sim <- data.frame(cbind(mle_phi, mle_se, bte_phi, bte_se))
d <- density(df_sim$mle_phi)
#d$y <- d$y/max(d$y)
d2 <- density(df_sim$bte_phi)
#d2$y <- d2$y/max(d2$y)
plot(d2, xlim = c(-1,1) )
lines(d, col = "red")
abline(v = 0.1, col = "blue", lty = 2)
