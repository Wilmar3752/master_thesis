source("utils.R")
library(iAR)
library(reshape2)
library(ggplot2)
## simulando IAR
sim <- iar_sim(10, phi = -0.4)

ciar_sim <- function(sim) {
    ciar <- CIARkalman(sim$x, sim$t_n)
    yhat <- CIARfit(phiValues = c(ciar$phiR, ciar$phiI), y = sim$x, t = sim$t_n)
    e <- sim$x - yhat$yhat
    ecm <- sqrt(mean(e^2))
return(ecm)
}

iar_simc <- function(sim) {
    pars <- iar_mle(data = sim)
    pred <- calc_xhat(sim, pars$par[1])
    e <- sim$x - pred
    ecm <- sqrt(mean(e^2))
return(ecm)
}

n <- c(100, 500, 1500)
phi <- c(-0.1, -0.5, -0.9)

ecm_ciar <- 0
ecm_iar <- 0
df_final_1 <- data.frame()
for (n in n){
    for (p in phi){
        for (i in 1:1000){
        sim <- iar_sim(n_sim = n, phi = p)
        ecm_ciar[i] <- ciar_sim(sim)
        ecm_iar[i] <- iar_simc(sim)
        }
    df_sim <- data.frame(cbind(ecm_ciar, ecm_iar))
    df_sim$phi <- p
    df_sim$n <- n
    df_final_1 <- rbind(df_final_1, df_sim)
    }
}

head(df_final_1)
df_final_1$n <- factor(df_final_1$n)

res_1 <- melt(df_final_1, id = c('phi', 'n'))
res_1$n <- as.numeric(as.character(res_1$n))

ggplot(res_1, aes(x = variable, y = value)) +
geom_boxplot() +
facet_wrap(~ phi + n, scales = "free", nrow = 3,
labeller = label_bquote(phi == .(phi)~","~"n="~.(paste(n))))

#write.csv(res_1, './data/iarvsciar.csv')

## Simulando Ciar
n <- c(100, 500, 1500)
phi <- c(-0.1, -0.5, -0.9)

ecm_ciar <- 0
ecm_iar <- 0
df_final_2 <- data.frame()
for (n in n){
    for (p in phi){
        for (i in 1:1000){
        sim <- iar_sim(n_sim = n, phi = p)
        sim$x < CIARsample(n = n, phiR = p, phiI = 0, st = sim$t_n)$y
        ecm_ciar[i] <- ciar_sim(sim)
        ecm_iar[i] <- iar_simc(sim)
        }
    df_sim <- data.frame(cbind(ecm_ciar, ecm_iar))
    df_sim$phi <- p
    df_sim$n <- n
    df_final_2 <- rbind(df_final_2, df_sim)
    }
}

head(df_final_2)

df_final_2$n <- factor(df_final_2$n)

res_2 <- melt(df_final_2, id = c('phi', 'n'))


ggplot(res_2, aes(x = variable, y = value)) +
geom_boxplot() +
facet_wrap(~ phi + n, scales = "free", nrow = 3,
    labeller = label_bquote(phi == .(phi) ~ "," ~ "n=" ~ .(paste(n))))

write.csv(res_2, './data/ciarvsiar.csv')
