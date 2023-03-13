source("utils.R")
n <- c(100, 500, 1500)
phi <- c(-0.1, -0.5, -0.9, 0.1, 0.5, 0.9)
sigma <- 1
m <- 1000

mle_phi <- 0
mle_se <- 0
bte_phi <- 0
bte_se <- 0
df_final  <- data.frame()
for (n in n) {
    print(n)
    for (p in phi) {
        print(p)
        for (i in 1:m){
            sim <- iar_sim(n_sim = n, phi = p, sigma = sigma) ## simulo 1 traye
            mle <- iar_mle(par = c(-0.99, 0.99),
                        fn = iar_loglik,
                        data = sim,
                        hessian = TRUE) ## MVE
            boot_est <- iar_boot_est(sim, 500) ## BOTSTRAP 500 trayectoras

            mle_phi[i] <- mle$par[1] #guardo phi por mv
            mle_se[i] <- mle$stde[1] # guardo stde por mv
            bte_phi[i] <- mean(boot_est) #promedio las estimaciones bootstrap
            bte_se[i] <- sd(boot_est) # Desviacion estandar bootstrap

            }
        df_sim <- data.frame(cbind(mle_phi, mle_se, bte_phi, bte_se))
        df_sim$phi <- p
        df_sim$n <- n

        df_final <- rbind(df_final, df_sim)
    }
}
df_final$n <- factor(df_final$n)

write.csv(df_final, "resultados_simulacion.csv")


library(ggplot2)
library(tidyverse)
install.packages("dplyr")

ggplot(df_final, aes(x = bte_phi, group = n)) +
geom_density(aes(col = n)) +
scale_color_manual(values = c("red", "blue", "black")) +
facet_wrap(~phi, scales = "free")

ggplot(df_final, aes(x = bte_phi, group = n)) +
geom_density(aes(col = n)) +
scale_color_manual(values = c("red", "blue", "black")) +
facet_wrap(~phi)

library(dplyr)

t <- df_final %>%
    group_by(n, phi) %>%
    summarise(
        mle = mean(mle_phi),
        se_mle = mean(mle_se),
        bias = mean(phi - mle_phi),
         .groups = "drop"
    )

write_excel <- function(x, row_names = FALSE, col_names = TRUE, ...) {
    con <- pipe("xclip -selection clipboard -i", open = "w")
    write.table(x, con, sep = "\t", row.names = row_names,
    col.names = col_names, ...)
    close(con)
}
write.excel(t)

t2 <- df_final %>%
      group_by(n, phi) %>%
      summarise(
        bte = mean(bte_phi),
        se_bte = mean(bte_se),
        bias = mean(phi - bte_phi),
         .groups = "drop"
    )
write.excel(t2)
