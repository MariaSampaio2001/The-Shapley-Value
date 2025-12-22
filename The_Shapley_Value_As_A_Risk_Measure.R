library(quantmod)
library(xts)
library(tidyquant)
library(zoo)

###########################################################################################
############################ TRATAMENDO DOS DADOS #############################
##########################################################################################

start_date <- as.Date("2021-10-01")
end_date <- as.Date("2024-10-01")

library(tidyquant)

getSymbols("^STOXX", src = "yahoo", from = start_date, to = end_date)
stoxx600_prices <- Ad(STOXX)


getSymbols("^STOXX50E", src = "yahoo", from = start_date, to = end_date )
getSymbols("^GDAXI", src = "yahoo", from = start_date, to = end_date )
getSymbols("^FTSE", src = "yahoo", from = start_date, to = end_date )

S_prices <- Ad(STOXX50E)
G_prices <- Ad(GDAXI)
F_prices <- Ad(FTSE)


# calcular os retornos logarítmicos
log_returns <- function(data) {
  return(diff(log(as.matrix(data))) * 100)
}


STOXX <- log_returns(S_prices)  
GDAXI <- log_returns(G_prices)
FTSE <- log_returns(F_prices)




nrow(STOXX)
nrow(GDAXI)
nrow(FTSE)
ncol(STOXX)
ncol(GDAXI)
ncol(FTSE)


STOXX_returns_xts <- xts(STOXX, order.by = index(S_prices)[-1])
GDAXI_returns_xts <- xts(GDAXI, order.by = index(G_prices)[-1])
FTSE_returns_xts <- xts(FTSE, order.by = index(F_prices)[-1])

# Combinar os retornos num único objeto xts
retornos <- merge.xts(STOXX_returns_xts, GDAXI_returns_xts, FTSE_returns_xts, all = FALSE)
colnames(retornos) <- c("STOXX", "GDAXI", "FTSE")  # Renomear colunas
retornos <- na.omit(retornos) # remover NA

nrow(retornos)

par(mar = c(5, 5, 4, 8))
# Primeiro STOXX (mais volátil)
plot(retornos[, "STOXX"],
     col = "darkgoldenrod2", lwd = 3,
     main = " ",
     ylab = "Log Returns",
     xlab = "Date",
     ylim = c(-6, 8))

# Depois GDAXI
lines(retornos[, "GDAXI"], col = "brown3", lwd = 3)

# Por último FTSE (menos volátil, fica em primeiro plano)
lines(retornos[, "FTSE"], col = "darkolivegreen4", lwd = 3)

legend("topright",
       inset = c(-0.05, 0),
       legend = colnames(retornos),
       col = c("darkgoldenrod2", "brown3", "darkolivegreen4"),
       lty = 1,
       lwd = 2,
       cex = 0.9,
       bty = "n")






library(ggplot2)
library(tidyr)

# Converter xts para data.frame
retornos_df <- fortify.zoo(retornos, name = "Date")

# Passar de wide para long
retornos_long <- pivot_longer(retornos_df,
                              cols = -Date,
                              names_to = "Index",
                              values_to = "Return")

# Forçar a ordem de desenho (STOXX atrás, GDAXI no meio, FTSE na frente)
retornos_long$Index <- factor(retornos_long$Index,
                              levels = c("STOXX", "GDAXI", "FTSE"))

# Gráfico
ggplot(retornos_long, aes(x = Date, y = Return, color = Index)) +
  geom_line(linewidth = 0.9) +  # linhas mais grossas
  scale_color_manual(values = c("STOXX" = "orange",
                                "GDAXI" = "blue",
                                "FTSE"  = "darkolivegreen4")) +
  labs(title = "Log Returns of STOXX, GDAXI and FTSE",
       x = "Date", y = "Log Returns") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        legend.title = element_blank())

#################################################################################################
######################################## SHAPLEY PVM #############################################
################################################################################################

mu <- c(mean(retornos$STOXX),mean(retornos$GDAXI),mean(retornos$FTSE))
mu

mcov <- cov(retornos)
mcov
mcov_inv <- solve(mcov)
uns <- c(1,1,1)
uns_2 <- c(1,1)

v_S <- sd(retornos$STOXX, TRUE) # se não meter TRUE, pára quando encontra NA
v_G <- sd(retornos$GDAXI, TRUE)
v_F <- sd(retornos$FTSE, TRUE)
v_S
v_G
v_F


# Posso achar v_SD atarvés do vértice porque estamos a falar da carteira MVP
mcov_2 <- mcov[c("STOXX", "GDAXI"), c("STOXX", "GDAXI")]
v_SG <- sqrt(1/(t(uns_2) %*% solve(mcov_2) %*% uns_2))

mcov_3 <- mcov[c("STOXX", "FTSE"), c("STOXX", "FTSE")]
v_SF <- sqrt(1/(t(uns_2) %*% solve(mcov_3) %*% uns_2))

mcov_4 <- mcov[c("GDAXI", "FTSE"), c("GDAXI", "FTSE")]
v_GF <- sqrt(1/(t(uns_2) %*% solve(mcov_4) %*% uns_2))

v_SGF <- sqrt(1/(t(uns)%*%mcov_inv%*%uns)) # desvio p. do vértice 






### valores:
v_S # 1.1558
v_G # 1.0877
v_F # 0.8277
v_SGF # 0.8075
v_SG # 1.0856
v_SF # 0.8208
v_GF # 0.8273



####### Construir a tabela 

ordens <- c("SGF", "SFG", "FSG", "FGS", "GSF", "GFS", "média")
jogadores <- c("S", "G", "F")

tabela <- data.frame(matrix(NA, nrow = 7, ncol = 3))
rownames(tabela) <- ordens
colnames(tabela) <- jogadores


tabela["SGF", "S"] <- v_S
tabela["SGF", "G"] <- v_SG - v_S
tabela["SGF", "F"] <- v_SGF - v_SG 

tabela["SFG", "S"] <- v_S
tabela["SFG", "G"] <- v_SGF - v_SF 
tabela["SFG", "F"] <- v_SF - v_S 

tabela["FSG", "S"] <- v_SF - v_F
tabela["FSG", "G"] <- v_SGF - v_SF
tabela["FSG", "F"] <- v_F 

tabela["FGS", "S"] <- v_SGF - v_GF
tabela["FGS", "G"] <- v_GF - v_F 
tabela["FGS", "F"] <- v_F

tabela["GSF", "S"] <- v_SG - v_G
tabela["GSF", "G"] <- v_G
tabela["GSF", "F"] <- v_SGF - v_SG

tabela["GFS", "S"] <- v_SGF - v_GF
tabela["GFS", "G"] <- v_G
tabela["GFS", "F"] <- v_GF - v_G

tabela["Shapley Values", ] <- colMeans(tabela[1:6, ], na.rm = TRUE)

print(round(tabela, 4))



library("CoopGame")
x<-c(v_S,v_G,v_F,v_SG,v_SF,v_GF,v_SGF)
shapleyValue(x)



###############################################################################################
########################## QUANTIDADES DE CADA RENTABILIDADE ###################################
###############################################################################################


mu <- c(mean(retornos$STOXX),mean(retornos$GDAXI),mean(retornos$FTSE))
mu

A <- t(uns) %*% mcov_inv %*% mu
B <- t(mu) %*% mcov_inv %*% mu
C <- t(uns) %*% mcov_inv %*% uns
D <- B*C - (A)^2

g <- function(mu_p){
  E <- (C * mu_p - A) / D
  i <- E[1] * mcov  
  F <- (B - A * mu_p) / D
  
  x_star =  (E[1] * (mcov_inv %*% mu)  + F[1] * mcov_inv %*% uns ) # usamos E[1] para acessar um escalar e não vetor
  x_star
  
  Erp_star <- (t(mu) %*% x_star) 
  Erp_star
  
  sigmarp_star <- (sqrt(t(x_star) %*% mcov %*% x_star)) 
  sigmarp_star
  
  
  return(list(sigmarp_star = sigmarp_star, x_star = x_star, Erp_star = Erp_star))
}


ERpmin <- 0.020
ERpmax <- 0.025
passo <- 0.001

x_star_values <- list()
desvio <- c(NA)
rent <- c(NA)
j <- 1

Rentabilidades <- seq(ERpmin, ERpmax, passo) # vetor d retabilidades

for (i in Rentabilidades){
  result <- g(i)
  desvio[j] <- result$sigmarp_star
  rent[j] <- i
  x_star_values[[j]] <- result$x_star
  j <- j + 1
}

# PLOT 
points(desvio,rent)
lines(desvio,rent)
plot(desvio,rent,xlab = "Risk (Standard Deviation)",
     ylab = "Expected Return")


# TABELA QUANTIDADES DE CADA RENTABILIDADE

tabela3 <- data.frame(
  Rentabilidade = rent,
  Desvio_Padrao = desvio,
  STOXX = sapply(x_star_values, function(x) x[1]), # extrai o primeiro elemento de cada vetor da lista 
  GDAXI = sapply(x_star_values, function(x) x[2]),
  FTSE = sapply(x_star_values, function(x) x[3])
)

print(round(tabela3,4))


############################################################################################
############################### PHI DOS ATIVOS ############################################### 
########################################################################################## 
V <- mcov # estabelecida antes
phi_values <- list()

for (k in seq_along(x_star_values)) { # itera sobre cada porrtfoio
  x_star <- x_star_values[[k]] 
  sigma_p2 <- t(x_star) %*% V %*% x_star # sigma_p^2 
  phi_k <- numeric(length(x_star)) 
  
  for (i in 1:length(x_star)) { # itera sobre cada ativo 
    sigma_ip <- sum(x_star * V[i, ]) 
    beta_i <- sigma_ip / sigma_p2 
    phi_k[i] <- x_star[i] * beta_i
    } 
  
  names(phi_k) <- colnames(V)
  phi_values[[k]] <- phi_k 
  }

phi_values






#########################################################################################
######################### NOVO BETA DE MERCADO #############################################
##############################################################################################

STOXX600 <- log_returns(stoxx600_prices)
STOXX600_returns_xts <- xts(STOXX600, order.by = index(stoxx600_prices)[-1])
retornos_1 <- merge.xts(STOXX_returns_xts, GDAXI_returns_xts, FTSE_returns_xts, STOXX600_returns_xts, all = FALSE)
colnames(retornos_1) <- c("STOXX", "GDAXI", "FTSE", "STOXX600")  # Renomear colunas
retornos_1 <- na.omit(retornos) # remover NA


# Função para calcular o beta de cada carteira ótima
calcular_beta <- function(retornos_carteira, retornos_mercado) {
  covariancia <- cov(retornos_carteira, retornos_mercado)
  variancia_mercado <- var(retornos_mercado)
  beta <- covariancia / variancia_mercado
  return(beta)
}

retornos_mercado <- retornos_1$STOXX600  

betas <- matrix(NA, nrow = length(rent), ncol = 3)  # Matriz 6x3 para armazenar os betas


for (j in 1:length(rent)) {
  x_star_s <- x_star_values[[j]][1]  # Quantidade do STOXX
  x_star_g <- x_star_values[[j]][2]  # Quantidade do GDAXI
  x_star_f <- x_star_values[[j]][3]  # Quantidade do FTSE
  
  # Calcular os retornos da carteira ótima 
  R_carteira <- x_star_s * retornos$STOXX + x_star_g * retornos$GDAXI + x_star_f * retornos$FTSE
  
  betas[j, 1] <- calcular_beta(R_carteira, retornos$STOXX)   # Beta em relação ao STOXX
  betas[j, 2] <- calcular_beta(R_carteira, retornos$GDAXI)   # Beta em relação ao GDAXI
  betas[j, 3] <- calcular_beta(R_carteira, retornos$FTSE)    # Beta em relação ao FTSE
}

# Exibir a matriz de betas
colnames(betas) <- c("Beta_STOXX", "Beta_GDAXI", "Beta_FTSE")
rownames(betas) <- paste("Rentabilidade", 1:length(rent), sep = "_")

print(round(betas, 4))




########################################################################################
######################### SHAPLEY PARA CADA RENTABILIDADE ####################
#########################################################################################



#### Encontrar v_SD v_SF v_DF
mu_2 <- c(mean(retornos$STOXX),mean(retornos$GDAXI))
mu_3 <- c(mean(retornos$STOXX),mean(retornos$FTSE))
mu_4 <- c(mean(retornos$GDAXI),mean(retornos$FTSE))
lista_mu <- list(mu_2, mu_3, mu_4)


mcov_2 <- mcov[c("STOXX", "GDAXI"), c("STOXX", "GDAXI")]
mcov_3 <- mcov[c("STOXX", "FTSE"), c("STOXX", "FTSE")]
mcov_4 <- mcov[c("GDAXI", "FTSE"), c("GDAXI", "FTSE")]
lista_mcov <- list(mcov_2, mcov_3, mcov_4)
lista_mcov

mcov_2_inv <- solve(mcov_2)
mcov_3_inv <- solve(mcov_3)
mcov_4_inv <- solve(mcov_4)  
lista_mcov_inv <- list(mcov_2_inv, mcov_3_inv, mcov_4_inv)


novo_values_2 <- list()
desvio_novo <- numeric()
w <- 1

for (combo in 1:3) {    # fixamos uma combinaçao (SG,SF,GF)
  mcov_x <- lista_mcov[[combo]] 
  mcov_x_inv <- lista_mcov_inv[[combo]]
  mu_x <- lista_mu[[combo]]
  
  for (i in Rentabilidades) {    # itera sobre cada combo
    novo_1 <- (i - mu_x[2]) / (mu_x[1] - mu_x[2])
    novo_2 <- 1 - novo_1
    novo_pesos <- c(novo_1, novo_2)
    novo_values_2[[w]] <- novo_pesos
    desvio_novo[w] <- sqrt(t(novo_pesos) %*% mcov_x %*% novo_pesos)
    
    w <- w + 1
  }
}


lista_SG_novo <- list(desvio_novo[1], desvio_novo[2], desvio_novo[3], desvio_novo[4], desvio_novo[5], desvio_novo[6])
lista_SF_novo <- list(desvio_novo[7], desvio_novo[8], desvio_novo[9], desvio_novo[10], desvio_novo[11], desvio_novo[12])
lista_GF_novo <- list(desvio_novo[13], desvio_novo[14], desvio_novo[15], desvio_novo[16], desvio_novo[17], desvio_novo[18])


shapley_values_novo <- list()
y <- 1
vetores_x <- list()
for (i in desvio) {  
  
  v_S_novo <- sd(retornos$STOXX, TRUE) # se não meter TRUE, pára quando encontra NA
  v_G_novo <- sd(retornos$GDAXI, TRUE)
  v_F_novo <- sd(retornos$FTSE, TRUE)
  
  v_SG_novo <- as.numeric(lista_SG_novo[[y]])
  v_SF_novo <- as.numeric(lista_SF_novo[[y]])
  v_GF_novo <- as.numeric(lista_GF_novo[[y]])
  
  x <- c(v_S_novo, v_G_novo, v_F_novo, v_SG_novo, v_SF_novo, v_GF_novo, i)
  vetores_x[[y]] <- x
  shapley_values_novo[[y]] <- shapleyValue(x)
  
  y <- y+1
}


# TABELAS SHAPLEY E RELATIVE SHAPLEY 

tabela_nova <- data.frame(
  Rentabilidade = rent,
  Desvio_Padrao = desvio,
  S_STOXX_N = sapply(shapley_values_novo, function(x) x[1]),
  S_GDAXI_N = sapply(shapley_values_novo, function(x) x[2]),
  S_FTSE_N = sapply(shapley_values_novo, function(x) x[3])
)
print(round(tabela_nova, 4))


tabela_relative <- data.frame(
  Rentabilidade = rent,
  Desvio_Padrao = desvio,
  S_R_STOXX_N = tabela_nova$S_STOXX_N / tabela_nova$Desvio_Padrao,
  S__R_GDAXI_N = tabela_nova$S_GDAXI_N / tabela_nova$Desvio_Padrao,
  S__R_FTSE_N = tabela_nova$S_FTSE_N / tabela_nova$Desvio_Padrao
)
print(round(tabela_relative,4))




########################################################################################
####################### ALGORITMO SHAPLEY RENTABILIDADES 0.020 E 0.023 #####################
##########################################################################################


ordens <- c("SGF", "SFG", "FSG", "FGS", "GSF", "GFS", "média")
jogadores <- c("S", "G", "F")



v_S_1 <- vetores_x[[1]][1]
v_G_1 <- vetores_x[[1]][2]
v_F_1 <- vetores_x[[1]][3]
v_SG_1 <- vetores_x[[1]][4]
v_SF_1 <- vetores_x[[1]][5]
v_GF_1 <- vetores_x[[1]][6]
v_SGF_1 <- vetores_x[[1]][7]
v_S_2 <- vetores_x[[4]][1]
v_G_2 <- vetores_x[[4]][2]
v_F_2 <- vetores_x[[4]][3]
v_SG_2 <- vetores_x[[4]][4]
v_SF_2 <- vetores_x[[4]][5]
v_GF_2 <- vetores_x[[4]][6]
v_SGF_2 <- vetores_x[[4]][7]


tabela_1 <- data.frame(matrix(NA, nrow = 7, ncol = 3))
rownames(tabela_1) <- ordens
colnames(tabela_1) <- jogadores


tabela_1["SGF", "S"] <- v_S_1
tabela_1["SGF", "G"] <- v_SG_1 - v_S_1
tabela_1["SGF", "F"] <- v_SGF_1 - v_SG_1 

tabela_1["SFG", "S"] <- v_S_1
tabela_1["SFG", "G"] <- v_SGF_1 - v_SF_1 
tabela_1["SFG", "F"] <- v_SF_1 - v_S_1

tabela_1["FSG", "S"] <- v_SF_1 - v_F_1
tabela_1["FSG", "G"] <- v_SGF_1 - v_SF_1
tabela_1["FSG", "F"] <- v_F_1 

tabela_1["FGS", "S"] <- v_SGF_1 - v_GF_1
tabela_1["FGS", "G"] <- v_GF_1 - v_F_1 
tabela_1["FGS", "F"] <- v_F_1

tabela_1["GSF", "S"] <- v_SG_1 - v_G_1
tabela_1["GSF", "G"] <- v_G_1
tabela_1["GSF", "F"] <- v_SGF_1 - v_SG_1

tabela_1["GFS", "S"] <- v_SGF_1 - v_GF_1
tabela_1["GFS", "G"] <- v_G_1
tabela_1["GFS", "F"] <- v_GF_1 - v_G_1

tabela_1["Shapley Values 1 à mão", ] <- colMeans(tabela_1[1:6, ], na.rm = TRUE)

print(round(tabela_1, 5))




tabela_2 <- data.frame(matrix(NA, nrow = 7, ncol = 3))
rownames(tabela_2) <- ordens
colnames(tabela_2) <- jogadores


tabela_2["SGF", "S"] <- v_S_2
tabela_2["SGF", "G"] <- v_SG_2 - v_S_2
tabela_2["SGF", "F"] <- v_SGF_2 - v_SG_2 

tabela_2["SFG", "S"] <- v_S_2
tabela_2["SFG", "G"] <- v_SGF_2 - v_SF_2
tabela_2["SFG", "F"] <- v_SF_2 - v_S_2

tabela_2["FSG", "S"] <- v_SF_2 - v_F_2
tabela_2["FSG", "G"] <- v_SGF_2 - v_SF_2
tabela_2["FSG", "F"] <- v_F_2 

tabela_2["FGS", "S"] <- v_SGF_2 - v_GF_2
tabela_2["FGS", "G"] <- v_GF_2 - v_F_2 
tabela_2["FGS", "F"] <- v_F_2

tabela_2["GSF", "S"] <- v_SG_2 - v_G_2
tabela_2["GSF", "G"] <- v_G_2
tabela_2["GSF", "F"] <- v_SGF_2 - v_SG_2

tabela_2["GFS", "S"] <- v_SGF_2 - v_GF_2
tabela_2["GFS", "G"] <- v_G_2
tabela_2["GFS", "F"] <- v_GF_2 - v_G_2

tabela_2["Shapley Values 2 à mão", ] <- colMeans(tabela_2[1:6, ], na.rm = TRUE)

print(round(tabela_2, 5))



