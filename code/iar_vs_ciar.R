source("utils.R")
library(iAR)


library(reshape2)
library(ggplot2)
## simulando IAR


ciar_sim <- function(x) {
    ciar <- CIARkalman(x$x, x$t_n)
    yhat <- CIARfit(phiValues = c(ciar$phiR, ciar$phiI), y = x$x, t = x$t_n)
    e <- x$x - yhat$yhat
    ecm <- sqrt(mean(e^2))
return(c(ciar$phiR, ecm))
}

iar_simc <- function(x) {
    pars <- iar_mle(data = x)
    pred <- calc_xhat(x, pars)
    e <- x$x - pred
    ecm <- sqrt(mean(e^2))
return(c(pars, ecm))
}

sim_results <- function(x){
    iar<- iar_simc(x)
    ciar <- ciar_sim(x)
return(c(iar,ciar))
}

n_list <- c(100, 500, 1500)
phi_list <- c(-0.1, -0.5, -0.9)
df_final <- data.frame()
resultados <- list()
sim <- data.frame()
final_res <- data.frame()
for (n in n_list){
    for (p in phi_list){
        for (i in 1:1000){
            sim <- iar_sim(n_sim = n, phi = p)
            resultados[[i]] <- sim
        }
    final_res <- lapply(resultados, sim_results)
    final_res <- data.frame(do.call(rbind, final_res))
    final_res$n <- n
    final_res$p <- p
   df_final <- rbind(df_final, final_res)
    }
}
colnames(df_final) <- c("phi_iar", "ecm_iar","phi_ciar", "ecm_ciar", "n", "phi")

df_final$n <- factor(df_final$n)

res_1 <- melt(df_final, id = c("phi", "n"))
res_1$n <- as.numeric(as.character(res_1$n))

#checkpoint
write.csv(res_1, "./data/iarvsciar.csv")
res_1 <- read.csv("./data/iarvsciar.csv")
res_1$variable <- as.factor(res_1$variable)
#levels(res_1$variable) <- c("CIAR", "IAR")

postscript("./../informe/Kap3/Fig_Cap3/sim4_IARvsCIARphi.eps")

ggplot(res_1[res_1$variable %in% c("phi_iar", "phi_ciar"), ],
        aes(x = variable, y = value)) +
geom_boxplot() +
facet_wrap(~ phi + n, nrow = 3,
labeller = label_bquote(phi == .(phi) ~ "," ~ "n=" ~ .(paste(n)))) +
labs(y = "Phi", x = "Modelo")
dev.off()

postscript("./../informe/Kap3/Fig_Cap3/sim4_IARvsCIAR.eps")

ggplot(res_1[res_1$variable %in% c("ecm_iar", "ecm_ciar"), ],
        aes(x = variable, y = value)) +
geom_boxplot() +
facet_wrap(~ phi + n, nrow = 3,
labeller = label_bquote(phi == .(phi) ~ "," ~ "n=" ~ .(paste(n)))) +
labs(y = "ECM", x = "Modelo")
dev.off()
#write.csv(res_1, "./data/iarvsciar.csv")

## Simulando Ciar
n_list <- c(100, 500, 1500)
phi_list <- c(-0.1, -0.5, -0.9)
df_final2 <- data.frame()
resultados2 <- list()
sim2 <- data.frame()
final_res2 <- data.frame()
for (n in n_list){
    for (p in phi_list){
        for (i in 1:1000){
            t_n <- iar_sim(n)$t_n
            CIAR <- CIARsample(n = n, phiR = p, phiI = 0, st = t_n)
            sim2 <- data.frame(cbind(CIAR$y, CIAR$t))
            colnames(sim2) <- c("x", "t_n")
            resultados2[[i]] <- sim2
        }
    final_res2 <- lapply(resultados2, sim_results)
    final_res2 <- data.frame(do.call(rbind, final_res2))
    final_res2$n <- n
    final_res2$p <- p
   df_final2 <- rbind(df_final2, final_res2)
    }
}
resultados2
df_final2

colnames(df_final2) <- c("phi_iar", "ecm_iar","phi_ciar", "ecm_ciar", "n", "phi")

df_final2$n <- factor(df_final2$n)

res_2 <- melt(df_final2, id = c("phi", "n"))
res_2$n <- as.numeric(as.character(res_2$n))

#checkpoint
write.csv(res_2, "./data/ciarvsiar.csv")
res_2 <- read.csv("./data/ciarvsiar.csv")
res_2$variable <- as.factor(res_2$variable)

#levels(res_2$variable) <- c("CIAR", "IAR")

postscript("./../informe/Kap3/Fig_Cap3/sim4_CIARvsIARphi.eps")

ggplot(res_2[res_2$variable %in% c("phi_iar", "phi_ciar"), ],
        aes(x = variable, y = value)) +
geom_boxplot() +
facet_wrap(~ phi + n, nrow = 3,
labeller = label_bquote(phi == .(phi) ~ "," ~ "n=" ~ .(paste(n)))) +
labs(y = "Phi", x = "Modelo")

dev.off()

postscript("./../informe/Kap3/Fig_Cap3/sim4_CIARvsIAR.eps")

ggplot(res_2[res_2$variable %in% c("ecm_iar", "ecm_ciar"), ],
        aes(x = variable, y = value)) +
geom_boxplot() +
facet_wrap(~ phi + n, nrow = 3,
labeller = label_bquote(phi == .(phi) ~ "," ~ "n=" ~ .(paste(n)))) +
labs(y = "ECM", x = "Modelo")
dev.off()

# Crear una lista de listas con tres elementos cada una
# Crear la lista original
t_n <- iar_sim(100)$t_n
sim2 <- data.frame()
CIAR <- CIARsample(n = 100, phiR = -0.5, phiI = 0, st = t_n)

CIARkalman(y = CIAR$y, t = CIAR$t)
sim2 <- data.frame(cbind(CIAR$y, CIAR$t))
colnames(sim2) <- c('x', 't_n')

sim_results(sim2)


head(resultados2[12])

