#install.packages("CoopGame")
#install.packages("dplyr")
#install.packages("writexl")

library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)

file_path <- "C:/Users/maria/OneDrive/Ambiente de Trabalho/Tese/Exemplo_Shalit/Preços_Retornos_ESG.xlsx"
dados_excel <- read_excel(file_path, skip = 4)
dados_excel[[1]] <- as.Date(dados_excel[[1]]) # ler datas no formato certo 
dados_empresas <- dados_excel[ , -ncol(dados_excel)] # remove 1 e ultima linha


dados_2022 <- subset(dados_empresas, format(as.Date(Date), "%Y") == "2022")
dados_2023 <- subset(dados_empresas, format(as.Date(Date), "%Y") == "2023")


log_returns <- function(data) {
  return(diff(log(as.matrix(data))) * 100)
}


dados_sem_data_2022 <- dados_2022[, -1]  
dados_sem_data_2023 <- dados_2023[, -1]

# retornos 2022
retornos_log_2022 <- log_returns(dados_sem_data_2022)
retornos_2022 <- cbind(Date = dados_2022$Date[-1], retornos_log_2022)  # tiramos a primeira data
retornos_2022 <- as.data.frame(retornos_2022)


# retornos 2023
retornos_log_2023 <- log_returns(dados_sem_data_2023)
retornos_2023 <- cbind(Date = dados_2023$Date[-1], retornos_log_2023)  # tiramos a primeira data
retornos_2023 <- as.data.frame(retornos_2023)

#  níveis 2022
high_2022 <- cbind(Date = retornos_2022$Date, retornos_2022[, seq(2, ncol(retornos_2022), by = 3)])
medium_2022 <- cbind(Date = retornos_2022$Date, retornos_2022[, seq(3, ncol(retornos_2022), by = 3)])
low_2022 <- cbind(Date = retornos_2022$Date, retornos_2022[, seq(4, ncol(retornos_2022), by = 3)])

# níveis 2023
high_2023 <- cbind(Date = retornos_2023$Date, retornos_2023[, seq(2, ncol(retornos_2023), by = 3)])
medium_2023 <- cbind(Date = retornos_2023$Date, retornos_2023[, seq(3, ncol(retornos_2023), by = 3)])
low_2023 <- cbind(Date = retornos_2023$Date, retornos_2023[, seq(4, ncol(retornos_2023), by = 3)])

# Combinações 2022
high_medium_2022 <- cbind(Date = high_2022$Date, high_2022[, -1], medium_2022[, -1])
high_low_2022 <- cbind(Date = high_2022$Date, high_2022[, -1], low_2022[, -1])
medium_low_2022 <- cbind(Date = medium_2022$Date, medium_2022[, -1], low_2022[, -1])
high_medium_low_2022 <- cbind(Date = high_2022$Date, high_2022[, -1], medium_2022[, -1], low_2022[, -1])

# Combinações 2023
high_medium_2023 <- cbind(Date = high_2023$Date, high_2023[, -1], medium_2023[, -1])
high_low_2023 <- cbind(Date = high_2023$Date, high_2023[, -1], low_2023[, -1])
medium_low_2023 <- cbind(Date = medium_2023$Date, medium_2023[, -1], low_2023[, -1])
high_medium_low_2023 <- cbind(Date = high_2023$Date, high_2023[, -1], medium_2023[, -1], low_2023[, -1])

head(high_medium_2022)
head(high_low_2022)
head(medium_low_2022)
head(high_medium_low_2022)


nrow(high_2022)

##########################################################################################################
################################ PORTFÓLIO VARIÂNCIA MÍNIMA ##############################################
################################            2022            ##############################################
##########################################################################################################



mu_high_2022 <- colMeans(high_2022[, -1], na.rm = TRUE) # exclui a primeirla linha porque data
mu_medium_2022 <- colMeans(medium_2022[, -1], na.rm = TRUE)
mu_low_2022 <- colMeans(low_2022[, -1], na.rm = TRUE)

cov_high_2022 <- cov(high_2022[, -1])
cov_inv_high_2022 <- solve(cov_high_2022)

cov_medium_2022 <- cov(medium_2022[, -1])
cov_inv_medium_2022 <- solve(cov_medium_2022)

cov_low_2022 <- cov(low_2022[, -1])
cov_inv_low_2022 <- solve(cov_low_2022)




############################################# PARA CADA NÍVEL #############################################

uns_um <- rep(1, 10)


g <- function(mu, mcov, mcov_inv){
  A <- t(uns_um) %*% mcov_inv %*% mu
  B <- t(mu) %*% mcov_inv %*% mu
  C <- t(uns_um) %*% mcov_inv %*% uns_um
  D <- B%*%C - (A)^2
  
  mu_p <- A / C
  
  E <- (C * mu_p - A) / D
  i <- E[1] * mcov  
  F <- (B - A * mu_p) / D
  
  x_star =  (E[1] * (mcov_inv %*% mu)  + F[1] * mcov_inv %*% uns_um ) # usamos E[1] para acessar um escalar e não vetor
  x_star
  
  R_p <- (t(mu) %*% x_star) 
  R_p
  
  sigma_p <- (sqrt(t(x_star) %*% mcov %*% x_star)) 
  sigma_p
  
  
  return(list(sigma_p = sigma_p, x_star = x_star, R_p = R_p))
}



mu_list_2022 <- list(mu_high_2022, mu_medium_2022, mu_low_2022)
cov_list_2022 <- list(cov_high_2022, cov_medium_2022, cov_low_2022)
cov_inv_list_2022 <- list(cov_inv_high_2022, cov_inv_medium_2022, cov_inv_low_2022)


# APLICAR FUNÇAO G 

resultados_individual_2022 <- list()


for (i in 1:length(mu_list_2022)) {
  resultados_individual_2022[[i]] <- g(mu_list_2022[[i]], cov_list_2022[[i]], cov_inv_list_2022[[i]])
}


names(resultados_individual_2022) <- c("HIGH_2022", "MEDIUM_2022", "LOW_2022")
resultados_individual_2022


resultados_individual_2022[[1]]$R_p
resultados_individual_2022[[2]]$R_p
resultados_individual_2022[[3]]$R_p

resultados_individual_2022[[1]]$sigma_p
resultados_individual_2022[[2]]$sigma_p
resultados_individual_2022[[3]]$sigma_p

resultados_individual_2022[[1]]$x_star
resultados_individual_2022[[2]]$x_star
resultados_individual_2022[[3]]$x_star


# SÉRIE TEMPORAL DAS CARTEIRAS (PESOS X RETORNOS)
série_high_2022 <- as.matrix(high_2022[, -1]) %*% resultados_individual_2022[[1]]$x_star 
série_medium_2022 <- as.matrix(medium_2022[, -1])%*% resultados_individual_2022[[2]]$x_star 
série_low_2022 <- as.matrix(low_2022[, -1])%*% resultados_individual_2022[[3]]$x_star 

df_series <- data.frame(
  Date = high_2022$Date,  # Usamos as datas do high_2022
  High = série_high_2022,
  Medium = série_medium_2022,
  Low = série_low_2022
)


df_series$Date <- as.Date(df_series$Date)
df_long <- df_series %>%
  pivot_longer(cols = -Date, names_to = "Portfolio", values_to = "Return")


ggplot(df_long, aes(x = Date, y = Return, color = Portfolio)) +
  geom_line(size = 1.2) +  
  scale_color_manual(values = c("High" = "#1b9e77", "Medium" = "#fdd972", "Low" = "#d3363d")) +  # Cores menos intensas
  labs(title = "Portfolio Returns Over Time (MVP)",
       x = "Date",
       y = "Portfolio Return",
       color = "Portfolio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# RÁCIO SHARPE PARA AS CARTEIRAS SINGULARES (H,M,L)

calcular_sharpe <- function(R_p, sigma_p, R_f = 0) {
  sharpe_ratio <- (R_p - R_f) / sigma_p
  return(sharpe_ratio)
}

sharpe_individual_2022 <- list()

for (i in 1:length(resultados_individual_2022)) {
  sharpe_individual_2022[[i]] <- calcular_sharpe(resultados_individual_2022[[i]]$R_p, resultados_individual_2022[[i]]$sigma_p)
}


names(sharpe_individual_2022) <- names(resultados_individual_2022)
sharpe_individual_2022[2]

# v(HIGH_2022) = 0.006
# v(MEDIUM_2022) = 0.021
# v(LOW_2022) = -0.025





############################################ PARA AS DUPLAS ############################################


# mu combinados HM, HL, ML

mu_high_medium_2022 <- c(mu_high_2022, mu_medium_2022)
mu_high_low_2022 <- c(mu_high_2022, mu_low_2022)
mu_medium_low_2022 <- c(mu_medium_2022, mu_low_2022)



# covariancias combinadas HM, HL, ML

cov_high_medium_2022 <- rbind(
  cbind(cov_high_2022, cov(high_2022[, -1], medium_2022[, -1])),
  cbind(cov(medium_2022[, -1], high_2022[, -1]), cov_medium_2022)
)


cov_high_low_2022 <- rbind(
  cbind(cov_high_2022, cov(high_2022[, -1], low_2022[, -1])),
  cbind(cov(low_2022[, -1], high_2022[, -1]), cov_low_2022)
)


cov_medium_low_2022 <- rbind(
  cbind(cov_medium_2022, cov(medium_2022[, -1], low_2022[, -1])),
  cbind(cov(low_2022[, -1], medium_2022[, -1]), cov_low_2022)
)


# matriz cov inversa combinada HM, HL, ML

cov_inv_high_medium_2022 <- solve(cov_high_medium_2022)
cov_inv_high_low_2022 <- solve(cov_high_low_2022)
cov_inv_medium_low_2022 <- solve(cov_medium_low_2022)



uns_um <- rep(1, 20)




mu_list_2_2022 <- list(
  mu_high_medium_2022, mu_high_low_2022, mu_medium_low_2022
)

cov_list_2_2022 <- list(
  cov_high_medium_2022, cov_high_low_2022, cov_medium_low_2022
)

cov_inv_list_2_2022 <- list(
  cov_inv_high_medium_2022, cov_inv_high_low_2022, cov_inv_medium_low_2022
)




# APLICAR FUNÇAO  G 

resultados_combinados_2022 <- list()

for (i in 1:length(mu_list_2_2022)) { 
  resultados_combinados_2022[[i]] <- g(mu_list_2_2022[[i]], cov_list_2_2022[[i]], cov_inv_list_2_2022[[i]])
}
names(resultados_combinados_2022) <- c("HIGH_MEDIUM_2022", "HIGH_LOW_2022", "MEDIUM_LOW_2022")




# RÁCIO SHARPE PARA AS CARTEIRAS COMBINADAS (HM, HL, ML)

sharpe_combinados_2022 <- list()


for (i in 1:length(resultados_combinados_2022)) {
  sharpe_combinados_2022[[i]] <- calcular_sharpe(resultados_combinados_2022[[i]]$R_p, resultados_combinados_2022[[i]]$sigma_p)
}

names(sharpe_combinados_2022) <- names(resultados_combinados_2022)
sharpe_combinados_2022

resultados_combinados_2022[[1]]$R_p
resultados_combinados_2022[[2]]$R_p
resultados_combinados_2022[[3]]$R_p

resultados_combinados_2022[[1]]$sigma_p
resultados_combinados_2022[[2]]$sigma_p
resultados_combinados_2022[[3]]$sigma_p


# v(HIGH_MEDIUM_2022) = 0.032
# v(HIGH_LOW_2022) = -0.0036
# v(MEDIUM_LOW_2022) = 0.0144

resultados_combinados_2022[[1]]$x_star
resultados_combinados_2022[[2]]$x_star
resultados_combinados_2022[[3]]$x_star

# SÉRIE TEMPORAL DAS CARTEIRAS (PESOS X RETORNOS)

série_high_medium_2022 <- as.matrix(high_medium_2022[,-1]) %*% resultados_combinados_2022[[1]]$x_star
série_high_low_2022 <- as.matrix(high_low_2022[,-1]) %*% resultados_combinados_2022[[2]]$x_star 
série_medium_low_2022 <- as.matrix(medium_low_2022[,-1]) %*% resultados_combinados_2022[[3]]$x_star 


plot_series <- function(series, title) {
  df <- data.frame(Date = as.Date(high_2022$Date), Return = as.numeric(series))
  
  ggplot(df, aes(x = Date, y = Return)) +
    geom_line(color = "blue", size = 1) +
    labs(title = title, x = "Date", y = "Portfolio Return") +
    theme_minimal() +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") # Ajusta os rótulos do eixo X
}

# Plotar cada série individualmente garantindo que as datas são corretamente interpretadas
plot_series(série_high_medium_2022, "High-Medium Portfolio Returns (MVP)")
plot_series(série_high_low_2022, "High-Low Portfolio Returns (MVP)")
plot_series(série_medium_low_2022, "Medium-Low Portfolio Returns (MVP)")


################################### PARA A TRIPLA ########################################################



mu_high_medium_low_2022 <- c(mu_high_2022, mu_medium_2022, mu_low_2022)


cov_high_medium_low_2022 <- rbind(
  cbind(cov_high_2022, cov(high_2022[, -1], medium_2022[, -1]), cov(high_2022[, -1], low_2022[, -1])),
  cbind(cov(medium_2022[, -1], high_2022[, -1]), cov_medium_2022, cov(medium_2022[, -1], low_2022[, -1])),
  cbind(cov(low_2022[, -1], high_2022[, -1]), cov(low_2022[, -1], medium_2022[, -1]), cov_low_2022)
)


cov_inv_high_medium_low_2022 <- solve(cov_high_medium_low_2022)


uns_um <- rep(1, 30)  

# APLICAR FUNÇÃO G

resultados_hml_2022 <- g(mu_high_medium_low_2022, cov_high_medium_low_2022, cov_inv_high_medium_low_2022)

# RÁCIO DE SHARPE HML
sharpe_hml_2022 <- calcular_sharpe(resultados_hml_2022$R_p, resultados_hml_2022$sigma_p)


list(HIGH_MEDIUM_LOW_2022 = resultados_hml_2022, Sharpe_HML = sharpe_hml_2022)
sharpe_hml_2022

# v(HML) = 0.022


resultados_hml_2022$x_star


# SÉRIE TEMPORAL DAS CARTEIRAS (PESOS X RETORNOS)

série_high_medium_low_2022 <- as.matrix(high_medium_low_2022[,-1]) %*% resultados_hml_2022$x_star 

plot_series <- function(series, title) {
  df <- data.frame(Date = as.Date(high_2022$Date), Return = as.numeric(series))
  
  ggplot(df, aes(x = Date, y = Return)) +
    geom_line(color = "blue", size = 1) +
    labs(title = title, x = "Date", y = "Portfolio Return") +
    theme_minimal() +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") # Ajusta os rótulos do eixo X
}

# Plotar cada série individualmente garantindo que as datas são corretamente interpretadas
plot_series(série_high_medium_low_2022, "High-Medium-Low Portfolio Returns (MVP)")









################################### SHAPLEY VALUE PVM 2022 ###########################################

v_H_2022 <- as.numeric(sharpe_individual_2022[[1]])
v_M_2022 <- as.numeric(sharpe_individual_2022[[2]])
v_L_2022 <- as.numeric(sharpe_individual_2022[[3]])
v_HM_2022 <- as.numeric(sharpe_combinados_2022[[1]])
v_HL_2022 <- as.numeric(sharpe_combinados_2022[[2]])
v_ML_2022 <- as.numeric(sharpe_combinados_2022[[3]])
v_HML_2022 <- as.numeric(sharpe_hml_2022)



library("CoopGame")
x_2022<-c(v_H_2022,v_M_2022,v_L_2022,v_HM_2022,v_HL_2022,v_ML_2022,v_HML_2022)
shapleyValue(x_2022)

# Shapley 2022 : 0.0099 0.026529  -0.01422



ordens <- c("HML", "HLM", "MHL", "MLH", "LHM", "LMH")
jogadores <- c("H", "M", "L")


tabela_1 <- data.frame(matrix(NA, nrow = 6, ncol = 3))
rownames(tabela_1) <- ordens
colnames(tabela_1) <- jogadores


tabela_1["HML", "H"] <- v_H_2022
tabela_1["HML", "M"] <- v_HM_2022 - v_H_2022
tabela_1["HML", "L"] <- v_HML_2022 - v_HM_2022 

tabela_1["HLM", "H"] <- v_H_2022
tabela_1["HLM", "M"] <- v_HML_2022 - v_HL_2022
tabela_1["HLM", "L"] <- v_HL_2022 - v_H_2022 

tabela_1["MHL", "H"] <- v_HM_2022 - v_M_2022
tabela_1["MHL", "M"] <- v_M_2022
tabela_1["MHL", "L"] <- v_HML_2022 - v_HM_2022

tabela_1["MLH", "H"] <- v_HML_2022 - v_ML_2022
tabela_1["MLH", "M"] <- v_M_2022 
tabela_1["MLH", "L"] <- v_ML_2022 - v_M_2022

tabela_1["LHM", "H"] <- v_HL_2022 - v_L_2022
tabela_1["LHM", "M"] <- v_HML_2022 - v_HL_2022
tabela_1["LHM", "L"] <- v_L_2022

tabela_1["LMH", "H"] <- v_HML_2022 - v_ML_2022
tabela_1["LMH", "M"] <- v_ML_2022 - v_L_2022
tabela_1["LMH", "L"] <- v_L_2022


tabela_1["Shapley Values 2022 (PVM)", ] <- colMeans(tabela_1[1:6, ], na.rm = TRUE)
tabela_1["Shapley Values Relative 2022 (PVM)", ] <- tabela_1["Shapley Values 2022 (PVM)", ] / v_HML_2022
print(round(tabela_1, 4))








##########################################################################################################
################################ PORTFÓLIO VARIÂNCIA MÍNIMA ##############################################
################################            2023            ##############################################
##########################################################################################################





mu_high_2023 <- colMeans(high_2023[, -1], na.rm = TRUE)
mu_medium_2023 <- colMeans(medium_2023[, -1], na.rm = TRUE)
mu_low_2023 <- colMeans(low_2023[, -1], na.rm = TRUE)

cov_high_2023 <- cov(high_2023[, -1])
cov_inv_high_2023 <- solve(cov_high_2023)

cov_medium_2023 <- cov(medium_2023[, -1])
cov_inv_medium_2023 <- solve(cov_medium_2023)

cov_low_2023 <- cov(low_2023[, -1])
cov_inv_low_2023 <- solve(cov_low_2023)




############################################# PARA CADA NÍVEL #############################################

uns_um <- rep(1, 10)

mu_list_2023 <- list(mu_high_2023, mu_medium_2023, mu_low_2023)
cov_list_2023 <- list(cov_high_2023, cov_medium_2023, cov_low_2023)
cov_inv_list_2023 <- list(cov_inv_high_2023, cov_inv_medium_2023, cov_inv_low_2023)

# APLICAR G 

resultados_individual_2023 <- list()

for (i in 1:length(mu_list_2023)) {
  resultados_individual_2023[[i]] <- g(mu_list_2023[[i]], cov_list_2023[[i]], cov_inv_list_2023[[i]])
}

names(resultados_individual_2023) <- c("HIGH_2023", "MEDIUM_2023", "LOW_2023")
resultados_individual_2023



# RACIO DE SHARPE INDIVIDUAL

calcular_sharpe <- function(R_p, sigma_p, R_f = 0) {
  sharpe_ratio <- (R_p - R_f) / sigma_p
  return(sharpe_ratio)
}

sharpe_individual_2023 <- list()

for (i in 1:length(resultados_individual_2023)) {
  sharpe_individual_2023[[i]] <- calcular_sharpe(resultados_individual_2023[[i]]$R_p, resultados_individual_2023[[i]]$sigma_p)
}

names(sharpe_individual_2023) <- names(resultados_individual_2023)
sharpe_individual_2023


resultados_individual_2023[[1]]$R_p
resultados_individual_2023[[2]]$R_p
resultados_individual_2023[[3]]$R_p

resultados_individual_2023[[1]]$sigma_p
resultados_individual_2023[[2]]$sigma_p
resultados_individual_2023[[3]]$sigma_p

# v(H) = 0.007
# v(M) = -0.017
# v(L) =  0.038

resultados_individual_2023[[1]]$x_star
resultados_individual_2023[[2]]$x_star
resultados_individual_2023[[3]]$x_star

# SÉRIE TEMPORAL DAS CARTEIRAS (PESOS X RETORNOS)

série_high_2023 <- as.matrix(high_2023[, -1]) %*% resultados_individual_2023[[1]]$x_star 
série_medium_2023 <- as.matrix(medium_2023[, -1])%*% resultados_individual_2023[[2]]$x_star 
série_low_2023 <- as.matrix(low_2023[, -1])%*% resultados_individual_2023[[3]]$x_star 

df_series <- data.frame(
  Date = high_2023$Date,  # Usamos as datas do high_2022
  High = série_high_2023,
  Medium = série_medium_2023,
  Low = série_low_2023
)


df_series$Date <- as.Date(df_series$Date)
df_long <- df_series %>%
  pivot_longer(cols = -Date, names_to = "Portfolio", values_to = "Return")


ggplot(df_long, aes(x = Date, y = Return, color = Portfolio)) +
  geom_line(size = 1.2) +  
  scale_color_manual(values = c("High" = "#1b9e77", "Medium" = "#fdd972", "Low" = "#d3363d")) +  # Cores menos intensas
  labs(title = "Portfolio Returns Over Time (MVP)",
       x = "Date",
       y = "Portfolio Return",
       color = "Portfolio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############################################ PARA AS DUPLAS ############################################


# Combinar os mu para 2023

mu_high_medium_2023 <- c(mu_high_2023, mu_medium_2023)
mu_high_low_2023 <- c(mu_high_2023, mu_low_2023)
mu_medium_low_2023 <- c(mu_medium_2023, mu_low_2023)



#  matrizes covariância combinadas para 2023

cov_high_medium_2023 <- rbind(
  cbind(cov_high_2023, cov(high_2023[, -1], medium_2023[, -1])),
  cbind(cov(medium_2023[, -1], high_2023[, -1]), cov_medium_2023)
)

cov_high_low_2023 <- rbind(
  cbind(cov_high_2023, cov(high_2023[, -1], low_2023[, -1])),
  cbind(cov(low_2023[, -1], high_2023[, -1]), cov_low_2023)
)

cov_medium_low_2023 <- rbind(
  cbind(cov_medium_2023, cov(medium_2023[, -1], low_2023[, -1])),
  cbind(cov(low_2023[, -1], medium_2023[, -1]), cov_low_2023)
)


cov_inv_high_medium_2023 <- solve(cov_high_medium_2023)
cov_inv_high_low_2023 <- solve(cov_high_low_2023)
cov_inv_medium_low_2023 <- solve(cov_medium_low_2023)

uns_um <- rep(1, 20)


# listas para as duplas 

mu_list_2_2023 <- list(
  mu_high_medium_2023, mu_high_low_2023, mu_medium_low_2023
)

cov_list_2_2023 <- list(
  cov_high_medium_2023, cov_high_low_2023, cov_medium_low_2023
)

cov_inv_list_2_2023 <- list(
  cov_inv_high_medium_2023, cov_inv_high_low_2023, cov_inv_medium_low_2023
)


# APLICAR FUNÇAO G 

resultados_combinados_2023 <- list()

for (i in 1:length(mu_list_2_2023)) { 
  resultados_combinados_2023[[i]] <- g(mu_list_2_2023[[i]], cov_list_2_2023[[i]], cov_inv_list_2_2023[[i]])
}
names(resultados_combinados_2023) <- c("HIGH_MEDIUM_2023", "HIGH_LOW_2023", "MEDIUM_LOW_2023")



# RÁCIO DE SHARPE COMBINADO (HM, HL, ML)

sharpe_combinados_2023 <- list()

for (i in 1:length(resultados_combinados_2023)) {
  sharpe_combinados_2023[[i]] <- calcular_sharpe(resultados_combinados_2023[[i]]$R_p, resultados_combinados_2023[[i]]$sigma_p)
}

names(sharpe_combinados_2023) <- names(resultados_combinados_2023)
sharpe_combinados_2023


# v(HM) = -0.014
# v(HL) = 0.024
# v(ML) =  0.008

resultados_combinados_2023[[1]]$R_p
resultados_combinados_2023[[2]]$R_p
resultados_combinados_2023[[3]]$R_p

resultados_combinados_2023[[1]]$sigma_p
resultados_combinados_2023[[2]]$sigma_p
resultados_combinados_2023[[3]]$sigma_p

resultados_combinados_2023[[1]]$x_star
resultados_combinados_2023[[2]]$x_star
resultados_combinados_2023[[3]]$x_star

# SÉRIE TEMPORAL DAS CARTEIRAS (PESOS X RETORNOS)

série_high_medium_2023 <- as.matrix(high_medium_2023[,-1]) %*% resultados_combinados_2023[[1]]$x_star
série_high_low_2023 <- as.matrix(high_low_2023[,-1]) %*% resultados_combinados_2023[[2]]$x_star 
série_medium_low_2023 <- as.matrix(medium_low_2023[,-1]) %*% resultados_combinados_2023[[3]]$x_star 


plot_series <- function(series, title) {
  df <- data.frame(Date = as.Date(high_2023$Date), Return = as.numeric(series))
  
  ggplot(df, aes(x = Date, y = Return)) +
    geom_line(color = "blue", size = 1) +
    labs(title = title, x = "Date", y = "Portfolio Return") +
    theme_minimal() +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") # Ajusta os rótulos do eixo X
}

# Plotar cada série individualmente garantindo que as datas são corretamente interpretadas
plot_series(série_high_medium_2023, "High-Medium Portfolio Returns (MVP)")
plot_series(série_high_low_2023, "High-Low Portfolio Returns (MVP)")
plot_series(série_medium_low_2023, "Medium-Low Portfolio Returns (MVP)")



################################### PARA A TRIPLA ########################################################

mu_high_medium_low_2023 <- c(mu_high_2023, mu_medium_2023, mu_low_2023)

cov_high_medium_low_2023 <- rbind(
  cbind(cov_high_2023, cov(high_2023[, -1], medium_2023[, -1]), cov(high_2023[, -1], low_2023[, -1])),
  cbind(cov(medium_2023[, -1], high_2023[, -1]), cov_medium_2023, cov(medium_2023[, -1], low_2023[, -1])),
  cbind(cov(low_2023[, -1], high_2023[, -1]), cov(low_2023[, -1], medium_2023[, -1]), cov_low_2023)
)

cov_inv_high_medium_low_2023 <- solve(cov_high_medium_low_2023)

uns_um <- rep(1, 30)

# FUNÇÃO G 
resultados_hml_2023 <- g(mu_high_medium_low_2023, cov_high_medium_low_2023, cov_inv_high_medium_low_2023)

# RÁCIO DE SHARPE HML
sharpe_hml_2023 <- calcular_sharpe(resultados_hml_2023$R_p, resultados_hml_2023$sigma_p)


# v(HML) = 0.008



resultados_hml_2023$x_star

# SÉRIE TEMPORAL DAS CARTEIRAS (PESOS X RETORNOS)

série_high_medium_low_2023 <- as.matrix(high_medium_low_2023[,-1]) %*% resultados_hml_2023$x_star 

plot_series <- function(series, title) {
  df <- data.frame(Date = as.Date(high_2023$Date), Return = as.numeric(series))
  
  ggplot(df, aes(x = Date, y = Return)) +
    geom_line(color = "blue", size = 1) +
    labs(title = title, x = "Date", y = "Portfolio Return") +
    theme_minimal() +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") # Ajusta os rótulos do eixo X
}

# Plotar cada série individualmente garantindo que as datas são corretamente interpretadas
plot_series(série_high_medium_low_2023, "High-Medium-Low Portfolio Returns (MVP)")


################################### SHAPLEY VALUE PVM 2023 ###########################################

v_H_2023 <- as.numeric(sharpe_individual_2023[[1]])
v_M_2023 <- as.numeric(sharpe_individual_2023[[2]])
v_L_2023 <- as.numeric(sharpe_individual_2023[[3]])
v_HM_2023 <- as.numeric(sharpe_combinados_2023[[1]])
v_HL_2023 <- as.numeric(sharpe_combinados_2023[[2]])
v_ML_2023 <- as.numeric(sharpe_combinados_2023[[3]])
v_HML_2023 <- as.numeric(sharpe_hml_2023)

library("CoopGame")
x_2023 <- c(v_H_2023, v_M_2023, v_L_2023, v_HM_2023, v_HL_2023, v_ML_2023, v_HML_2023)
shapleyValue(x_2023) 

# Shapley 2023: 0.0005 -0.0200 0.0272



ordens <- c("HML", "HLM", "MHL", "MLH", "LHM", "LMH")
jogadores <- c("H", "M", "L")

tabela_2 <- data.frame(matrix(NA, nrow = 6, ncol = 3))
rownames(tabela_2) <- ordens
colnames(tabela_2) <- jogadores

tabela_2["HML", "H"] <- v_H_2023
tabela_2["HML", "M"] <- v_HM_2023 - v_H_2023
tabela_2["HML", "L"] <- v_HML_2023 - v_HM_2023 

tabela_2["HLM", "H"] <- v_H_2023
tabela_2["HLM", "M"] <- v_HML_2023 - v_HL_2023
tabela_2["HLM", "L"] <- v_HL_2023 - v_H_2023 

tabela_2["MHL", "H"] <- v_HM_2023 - v_M_2023
tabela_2["MHL", "M"] <- v_M_2023
tabela_2["MHL", "L"] <- v_HML_2023 - v_HM_2023

tabela_2["MLH", "H"] <- v_HML_2023 - v_ML_2023
tabela_2["MLH", "M"] <- v_M_2023 
tabela_2["MLH", "L"] <- v_ML_2023 - v_M_2023

tabela_2["LHM", "H"] <- v_HL_2023 - v_L_2023
tabela_2["LHM", "M"] <- v_HML_2023 - v_HL_2023
tabela_2["LHM", "L"] <- v_L_2023

tabela_2["LMH", "H"] <- v_HML_2023 - v_ML_2023
tabela_2["LMH", "M"] <- v_ML_2023 - v_L_2023
tabela_2["LMH", "L"] <- v_L_2023


tabela_2["Shapley Values 2023 (PVM)", ] <- colMeans(tabela_2[1:6, ], na.rm = TRUE)
tabela_2["Shapley Values Relative 2023 (PVM)", ] <- tabela_2["Shapley Values 2023 (PVM)", ] / v_HML_2023
print(round(tabela_2, 4))







###############################################################################################################
############################### CARTEIRA MAIOR RÁCIO SHARPE ###################################################
###############################            2022             ###################################################
###############################################################################################################




######################################## INDIVIDUAIS ##############################################

carteira_MRS <- function(mu, mcov, mcov_inv) {
  r_f <- 0
  uns_MRS <- matrix(rep(1, length(mu)), ncol = 1) 
  
  A <- t(uns_MRS) %*% mcov_inv %*% mu
  C <- t(uns_MRS) %*% mcov_inv %*% uns_MRS
  
  excess_mu <- mu - r_f * uns_MRS  # Como r_f = 0, fica apenas mu
  
  x_MRS <- (mcov_inv %*% excess_mu) / as.numeric(A)  # Como r_f = 0, o denominador vira apenas A
  
  R_MRS <- t(mu) %*% x_MRS
  
  sigma_MRS <- sqrt(t(x_MRS) %*% mcov %*% x_MRS)
  
  return(list(x_MRS = x_MRS, R_MRS = R_MRS, sigma_MRS = sigma_MRS))
}

# APLICAR FUNÇAO MRS 

resultados_individual_2022_MRS <- list()


for (i in 1:length(mu_list_2022)) {
  resultados_individual_2022_MRS[[i]] <- carteira_MRS(mu_list_2022[[i]], cov_list_2022[[i]], cov_inv_list_2022[[i]])
}


names(resultados_individual_2022_MRS) <- c("HIGH_2022", "MEDIUM_2022", "LOW_2022")
resultados_individual_2022_MRS



# RÁCIO SHARPE PARA AS CARTEIRAS SINGULARES (H,M,L)

calcular_sharpe <- function(R_MRS, sigma_MRS, R_f = 0) {
  sharpe_ratio <- (R_MRS - R_f) / sigma_MRS
  return(sharpe_ratio)
}

sharpe_individual_2022_MRS <- list()

for (i in 1:length(resultados_individual_2022_MRS)) {
  sharpe_individual_2022_MRS[[i]] <- calcular_sharpe(resultados_individual_2022_MRS[[i]]$R_MRS, resultados_individual_2022_MRS[[i]]$sigma_MRS)
}


names(sharpe_individual_2022_MRS) <- names(resultados_individual_2022_MRS)
sharpe_individual_2022_MRS

# v(H) = 0.198
# v(M) = 0.148
# v(L) = -0.182



resultados_individual_2022_MRS[[1]]$R_MRS
resultados_individual_2022_MRS[[2]]$R_MRS
resultados_individual_2022_MRS[[3]]$R_MRS

resultados_individual_2022_MRS[[1]]$sigma_MRS
resultados_individual_2022_MRS[[2]]$sigma_MRS
resultados_individual_2022_MRS[[3]]$sigma_MRS

resultados_individual_2022_MRS[[1]]$x_MRS
resultados_individual_2022_MRS[[2]]$x_MRS
resultados_individual_2022_MRS[[3]]$x_MRS

# SÉRIE TEMPORAL DAS CARTEIRAS (PESOS X RETORNOS)


série_high_2022_MRS <- as.matrix(high_2022[, -1]) %*% resultados_individual_2022_MRS[[1]]$x_MRS 
série_medium_2022_MRS <- as.matrix(medium_2022[, -1]) %*% resultados_individual_2022_MRS[[2]]$x_MRS
série_low_2022_MRS <- as.matrix(low_2022[, -1]) %*% resultados_individual_2022_MRS[[3]]$x_MRS 

df_series <- data.frame(
  Date = high_2022$Date,  # Usamos as datas do high_2022
  High = série_high_2022_MRS,
  Medium = série_medium_2022_MRS,
  Low = série_low_2022_MRS
)


df_series$Date <- as.Date(df_series$Date)
df_long <- df_series %>%
  pivot_longer(cols = -Date, names_to = "Portfolio", values_to = "Return")


ggplot(df_long, aes(x = Date, y = Return, color = Portfolio)) +
  geom_line(size = 1.2) +  
  scale_color_manual(values = c("High" = "#1b9e77", "Medium" = "#fdd972", "Low" = "#d3363d")) +  # Cores menos intensas
  labs(title = "Portfolio Returns Over Time (MSRP)",
       x = "Date",
       y = "Portfolio Return",
       color = "Portfolio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############################################# DUPLAS #################################################

resultados_combinados_2022_MRS <- list()

for (i in 1:length(mu_list_2_2022)) { 
  resultados_combinados_2022_MRS[[i]] <- carteira_MRS(mu_list_2_2022[[i]], cov_list_2_2022[[i]], cov_inv_list_2_2022[[i]])
}
names(resultados_combinados_2022_MRS) <- c("HIGH_MEDIUM_2022", "HIGH_LOW_2022", "MEDIUM_LOW_2022")




# RÁCIO SHARPE PARA AS CARTEIRAS COMBINADAS (HM, HL, ML)

sharpe_combinados_2022_MRS <- list()


for (i in 1:length(resultados_combinados_2022_MRS)) {
  sharpe_combinados_2022_MRS[[i]] <- calcular_sharpe(resultados_combinados_2022_MRS[[i]]$R_MRS, resultados_combinados_2022_MRS[[i]]$sigma_MRS)
}

names(sharpe_combinados_2022_MRS) <- names(resultados_combinados_2022_MRS)
sharpe_combinados_2022_MRS


# v(HM) = 0.214
# v(HL) = -0.236
# v(ML) = 0.221



resultados_combinados_2022_MRS[[1]]$R_MRS
resultados_combinados_2022_MRS[[2]]$R_MRS
resultados_combinados_2022_MRS[[3]]$R_MRS

resultados_combinados_2022_MRS[[1]]$sigma_MRS
resultados_combinados_2022_MRS[[2]]$sigma_MRS
resultados_combinados_2022_MRS[[3]]$sigma_MRS

resultados_combinados_2022_MRS[[1]]$x_MRS
resultados_combinados_2022_MRS[[2]]$x_MRS
resultados_combinados_2022_MRS[[3]]$x_MRS

# SÉRIE TEMPORAL DAS CARTEIRAS (PESOS X RETORNOS)

série_high_medium_2022_MRS <- as.matrix(high_medium_2022[,-1]) %*% resultados_combinados_2022_MRS[[1]]$x_MRS
série_high_low_2022_MRS <- as.matrix(high_low_2022[,-1]) %*% resultados_combinados_2022_MRS[[2]]$x_MRS 
série_medium_low_2022_MRS <- as.matrix(medium_low_2022[,-1]) %*% resultados_combinados_2022_MRS[[3]]$x_MRS


plot_series <- function(series, title) {
  df <- data.frame(Date = as.Date(high_2022$Date), Return = as.numeric(series))
  
  ggplot(df, aes(x = Date, y = Return)) +
    geom_line(color = "blue", size = 1) +
    labs(title = title, x = "Date", y = "Portfolio Return") +
    theme_minimal() +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") # Ajusta os rótulos do eixo X
}

# Plotar cada série individualmente garantindo que as datas são corretamente interpretadas
plot_series(série_high_medium_2022_MRS, "High-Medium Portfolio Returns (MSRP)")
plot_series(série_high_low_2022_MRS, "High-Low Portfolio Returns (MSRP)")
plot_series(série_medium_low_2022_MRS, "Medium-Low Portfolio Returns (MSRP)")




######################################### TRIPLAS #########################################################


resultados_hml_2022_MRS <- carteira_MRS(mu_high_medium_low_2022, cov_high_medium_low_2022, cov_inv_high_medium_low_2022)

# RÁCIO DE SHARPE HML
sharpe_hml_2022_MRS <- calcular_sharpe(resultados_hml_2022_MRS$R_MRS, resultados_hml_2022_MRS$sigma_MRS)


list(HIGH_MEDIUM_LOW_2022_MRS = resultados_hml_2022_MRS, Sharpe_HML_MRS = sharpe_hml_2022_MRS)
sharpe_hml_2022_MRS

# v(HML) = 0.265




série_high_medium_low_2022_MRS <- as.matrix(high_medium_low_2022[,-1]) %*% resultados_hml_2022_MRS$x_MRS 

plot_series <- function(series, title) {
  df <- data.frame(Date = as.Date(high_2022$Date), Return = as.numeric(series))
  
  ggplot(df, aes(x = Date, y = Return)) +
    geom_line(color = "blue", size = 1) +
    labs(title = title, x = "Date", y = "Portfolio Return") +
    theme_minimal() +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") # Ajusta os rótulos do eixo X
}

# Plotar cada série individualmente garantindo que as datas são corretamente interpretadas
plot_series(série_high_medium_low_2022_MRS, "High-Medium-Low Portfolio Returns (MSRP)")






###################################### SHAPLEY MRS 2022 ######################################################

v_H_2022_MRS <- as.numeric(sharpe_individual_2022_MRS[[1]])
v_M_2022_MRS <- as.numeric(sharpe_individual_2022_MRS[[2]])
v_L_2022_MRS <- as.numeric(sharpe_individual_2022_MRS[[3]])
v_HM_2022_MRS <- as.numeric(sharpe_combinados_2022_MRS[[1]])
v_HL_2022_MRS <- as.numeric(sharpe_combinados_2022_MRS[[2]])
v_ML_2022_MRS <- as.numeric(sharpe_combinados_2022_MRS[[3]])
v_HML_2022_MRS <- as.numeric(sharpe_hml_2022_MRS)



library("CoopGame")
x_2022_MRS<-c(v_H_2022_MRS,v_M_2022_MRS,v_L_2022_MRS,v_HM_2022_MRS,v_HL_2022_MRS,v_ML_2022_MRS,v_HML_2022_MRS)
shapleyValue(x_2022_MRS)

# Shapley 2022 : 0.0828  0.286553  -0.1040



ordens <- c("HML", "HLM", "MHL", "MLH", "LHM", "LMH")
jogadores <- c("H", "M", "L")


tabela_3 <- data.frame(matrix(NA, nrow = 6, ncol = 3))
rownames(tabela_3) <- ordens
colnames(tabela_3) <- jogadores


tabela_3["HML", "H"] <- v_H_2022_MRS
tabela_3["HML", "M"] <- v_HM_2022_MRS - v_H_2022_MRS
tabela_3["HML", "L"] <- v_HML_2022_MRS - v_HM_2022_MRS 

tabela_3["HLM", "H"] <- v_H_2022_MRS
tabela_3["HLM", "M"] <- v_HML_2022_MRS - v_HL_2022_MRS
tabela_3["HLM", "L"] <- v_HL_2022_MRS - v_H_2022_MRS 

tabela_3["MHL", "H"] <- v_HM_2022_MRS - v_M_2022_MRS
tabela_3["MHL", "M"] <- v_M_2022_MRS
tabela_3["MHL", "L"] <- v_HML_2022_MRS - v_HM_2022_MRS

tabela_3["MLH", "H"] <- v_HML_2022_MRS - v_ML_2022_MRS
tabela_3["MLH", "M"] <- v_M_2022_MRS 
tabela_3["MLH", "L"] <- v_ML_2022_MRS - v_M_2022_MRS

tabela_3["LHM", "H"] <- v_HL_2022_MRS - v_L_2022_MRS
tabela_3["LHM", "M"] <- v_HML_2022_MRS - v_HL_2022_MRS
tabela_3["LHM", "L"] <- v_L_2022_MRS

tabela_3["LMH", "H"] <- v_HML_2022_MRS - v_ML_2022_MRS
tabela_3["LMH", "M"] <- v_ML_2022_MRS - v_L_2022_MRS
tabela_3["LMH", "L"] <- v_L_2022_MRS


tabela_3["Shapley Values 2022 (MRS)", ] <- colMeans(tabela_3[1:6, ], na.rm = TRUE)
tabela_3["Shapley Values Relative 2022 (MRS)", ] <- tabela_3["Shapley Values 2022 (MRS)", ] / v_HML_2022_MRS
print(round(tabela_3, 4))





###############################################################################################################
############################### CARTEIRA MAIOR RÁCIO SHARPE ###################################################
###############################            2023             ###################################################
###############################################################################################################




######################################## INDIVIDUAIS ##############################################

carteira_MRS <- function(mu, mcov, mcov_inv) {
  r_f <- 0
  uns_MRS <- matrix(rep(1, length(mu)), ncol = 1) 
  
  A <- t(uns_MRS) %*% mcov_inv %*% mu
  C <- t(uns_MRS) %*% mcov_inv %*% uns_MRS
  
  excess_mu <- mu - r_f * uns_MRS  # Como r_f = 0, fica apenas mu
  
  x_MRS <- (mcov_inv %*% excess_mu) / as.numeric(A)  # Como r_f = 0, o denominador vira apenas A
  
  R_MRS <- t(mu) %*% x_MRS
  
  sigma_MRS <- sqrt(t(x_MRS) %*% mcov %*% x_MRS)
  
  return(list(x_MRS = x_MRS, R_MRS = R_MRS, sigma_MRS = sigma_MRS))
}

# APLICAR FUNÇAO MRS 

resultados_individual_2023_MRS <- list()


for (i in 1:length(mu_list_2023)) {
  resultados_individual_2023_MRS[[i]] <- carteira_MRS(mu_list_2023[[i]], cov_list_2023[[i]], cov_inv_list_2023[[i]])
}


names(resultados_individual_2023_MRS) <- c("HIGH_2023", "MEDIUM_2023", "LOW_2023")
resultados_individual_2023_MRS



# RÁCIO SHARPE PARA AS CARTEIRAS SINGULARES (H,M,L)

calcular_sharpe <- function(R_MRS, sigma_MRS, R_f = 0) {
  sharpe_ratio <- (R_MRS - R_f) / sigma_MRS
  return(sharpe_ratio)
}

sharpe_individual_2023_MRS <- list()

for (i in 1:length(resultados_individual_2023_MRS)) {
  sharpe_individual_2023_MRS[[i]] <- calcular_sharpe(resultados_individual_2023_MRS[[i]]$R_MRS, resultados_individual_2023_MRS[[i]]$sigma_MRS)
}


names(sharpe_individual_2023_MRS) <- names(resultados_individual_2023_MRS)
sharpe_individual_2023_MRS

# v(H) = 0.147
# v(M) = -0.162
# v(L) = 0.206 


resultados_individual_2023_MRS[[1]]$R_MRS
resultados_individual_2023_MRS[[2]]$R_MRS
resultados_individual_2023_MRS[[3]]$R_MRS

resultados_individual_2023_MRS[[1]]$sigma_MRS
resultados_individual_2023_MRS[[2]]$sigma_MRS
resultados_individual_2023_MRS[[3]]$sigma_MRS

resultados_individual_2023_MRS[[1]]$x_MRS
resultados_individual_2023_MRS[[2]]$x_MRS
resultados_individual_2023_MRS[[3]]$x_MRS

# SÉRIE TEMPORAL DAS CARTEIRAS (PESOS X RETORNOS)

série_high_2023_MRS <- as.matrix(high_2023[, -1]) %*% resultados_individual_2023_MRS[[1]]$x_MRS 
série_medium_2023_MRS <- as.matrix(medium_2023[, -1]) %*% resultados_individual_2023_MRS[[2]]$x_MRS
série_low_2023_MRS <- as.matrix(low_2023[, -1]) %*% resultados_individual_2023_MRS[[3]]$x_MRS 

df_series <- data.frame(
  Date = high_2023$Date,  # Usamos as datas do high_2022
  High = série_high_2023_MRS,
  Medium = série_medium_2023_MRS,
  Low = série_low_2023_MRS
)


df_series$Date <- as.Date(df_series$Date)
df_long <- df_series %>%
  pivot_longer(cols = -Date, names_to = "Portfolio", values_to = "Return")


ggplot(df_long, aes(x = Date, y = Return, color = Portfolio)) +
  geom_line(size = 1.2) +  
  scale_color_manual(values = c("High" = "#1b9e77", "Medium" = "#fdd972", "Low" = "#d3363d")) +  # Cores menos intensas
  labs(title = "Portfolio Returns Over Time (MSRP)",
       x = "Date",
       y = "Portfolio Return",
       color = "Portfolio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############################################# DUPLAS #################################################

resultados_combinados_2023_MRS <- list()

for (i in 1:length(mu_list_2_2023)) { 
  resultados_combinados_2023_MRS[[i]] <- carteira_MRS(mu_list_2_2023[[i]], cov_list_2_2023[[i]], cov_inv_list_2_2023[[i]])
}
names(resultados_combinados_2023_MRS) <- c("HIGH_MEDIUM_2023", "HIGH_LOW_2023", "MEDIUM_LOW_2023")




# RÁCIO SHARPE PARA AS CARTEIRAS COMBINADAS (HM, HL, ML)

sharpe_combinados_2023_MRS <- list()


for (i in 1:length(resultados_combinados_2023_MRS)) {
  sharpe_combinados_2023_MRS[[i]] <- calcular_sharpe(resultados_combinados_2023_MRS[[i]]$R_MRS, resultados_combinados_2023_MRS[[i]]$sigma_MRS)
}

names(sharpe_combinados_2023_MRS) <- names(resultados_combinados_2023_MRS)
sharpe_combinados_2023_MRS


# v(HM) = -0.199
# v(HL) = 0.235
# v(ML) = 0.232



resultados_combinados_2023_MRS[[1]]$R_MRS
resultados_combinados_2023_MRS[[2]]$R_MRS
resultados_combinados_2023_MRS[[3]]$R_MRS

resultados_combinados_2023_MRS[[1]]$sigma_MRS
resultados_combinados_2023_MRS[[2]]$sigma_MRS
resultados_combinados_2023_MRS[[3]]$sigma_MRS

resultados_combinados_2023_MRS[[1]]$x_MRS
resultados_combinados_2023_MRS[[2]]$x_MRS
resultados_combinados_2023_MRS[[3]]$x_MRS

# SÉRIE TEMPORAL DAS CARTEIRAS (PESOS X RETORNOS)

série_high_medium_2023_MRS <- as.matrix(high_medium_2023[,-1]) %*% resultados_combinados_2023_MRS[[1]]$x_MRS
série_high_low_2023_MRS <- as.matrix(high_low_2023[,-1]) %*% resultados_combinados_2023_MRS[[2]]$x_MRS 
série_medium_low_2023_MRS <- as.matrix(medium_low_2023[,-1]) %*% resultados_combinados_2023_MRS[[3]]$x_MRS


plot_series <- function(series, title) {
  df <- data.frame(Date = as.Date(high_2023$Date), Return = as.numeric(series))
  
  ggplot(df, aes(x = Date, y = Return)) +
    geom_line(color = "blue", size = 1) +
    labs(title = title, x = "Date", y = "Portfolio Return") +
    theme_minimal() +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") # Ajusta os rótulos do eixo X
}

# Plotar cada série individualmente garantindo que as datas são corretamente interpretadas
plot_series(série_high_medium_2023_MRS, "High-Medium Portfolio Returns (MSRP)")
plot_series(série_high_low_2023_MRS, "High-Low Portfolio Returns (MSRP)")
plot_series(série_medium_low_2023_MRS, "Medium-Low Portfolio Returns (MSRP)")


######################################### TRIPLAS #########################################################


resultados_hml_2023_MRS <- carteira_MRS(mu_high_medium_low_2023, cov_high_medium_low_2023, cov_inv_high_medium_low_2023)

# RÁCIO DE SHARPE HML
sharpe_hml_2023_MRS <- calcular_sharpe(resultados_hml_2023_MRS$R_MRS, resultados_hml_2023_MRS$sigma_MRS)


list(HIGH_MEDIUM_LOW_2023_MRS = resultados_hml_2023_MRS, Sharpe_HML_MRS = sharpe_hml_2023_MRS)
sharpe_hml_2023_MRS

# v(HML) = 0.260


série_high_medium_low_2023_MRS <- as.matrix(high_medium_low_2023[,-1]) %*% resultados_hml_2023_MRS$x_MRS 

plot_series <- function(series, title) {
  df <- data.frame(Date = as.Date(high_2023$Date), Return = as.numeric(series))
  
  ggplot(df, aes(x = Date, y = Return)) +
    geom_line(color = "blue", size = 1) +
    labs(title = title, x = "Date", y = "Portfolio Return") +
    theme_minimal() +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") # Ajusta os rótulos do eixo X
}

# Plotar cada série individualmente garantindo que as datas são corretamente interpretadas
plot_series(série_high_medium_low_2023_MRS, "High-Medium-Low Portfolio Returns (MSRP)")




################################# SHAPLEY MRS 2023 ########################################################


v_H_2023_MRS <- as.numeric(sharpe_individual_2023_MRS[[1]])
v_M_2023_MRS <- as.numeric(sharpe_individual_2023_MRS[[2]])
v_L_2023_MRS <- as.numeric(sharpe_individual_2023_MRS[[3]])
v_HM_2023_MRS <- as.numeric(sharpe_combinados_2023_MRS[[1]])
v_HL_2023_MRS <- as.numeric(sharpe_combinados_2023_MRS[[2]])
v_ML_2023_MRS <- as.numeric(sharpe_combinados_2023_MRS[[3]])
v_HML_2023_MRS <- as.numeric(sharpe_hml_2023_MRS)



library("CoopGame")
x_2023_MRS <- c(v_H_2023_MRS, v_M_2023_MRS, v_L_2023_MRS, v_HM_2023_MRS, v_HL_2023_MRS, v_ML_2023_MRS, v_HML_2023_MRS)
shapleyValue(x_2023_MRS)

# Shapley 2023 : 0.0569  -0.0992  0.3022



ordens <- c("HML", "HLM", "MHL", "MLH", "LHM", "LMH")
jogadores <- c("H", "M", "L")

tabela_4 <- data.frame(matrix(NA, nrow = 6, ncol = 3))
rownames(tabela_4) <- ordens
colnames(tabela_4) <- jogadores

tabela_4["HML", "H"] <- v_H_2023_MRS
tabela_4["HML", "M"] <- v_HM_2023_MRS - v_H_2023_MRS
tabela_4["HML", "L"] <- v_HML_2023_MRS - v_HM_2023_MRS 

tabela_4["HLM", "H"] <- v_H_2023_MRS
tabela_4["HLM", "M"] <- v_HML_2023_MRS - v_HL_2023_MRS
tabela_4["HLM", "L"] <- v_HL_2023_MRS - v_H_2023_MRS 

tabela_4["MHL", "H"] <- v_HM_2023_MRS - v_M_2023_MRS
tabela_4["MHL", "M"] <- v_M_2023_MRS
tabela_4["MHL", "L"] <- v_HML_2023_MRS - v_HM_2023_MRS

tabela_4["MLH", "H"] <- v_HML_2023_MRS - v_ML_2023_MRS
tabela_4["MLH", "M"] <- v_M_2023_MRS 
tabela_4["MLH", "L"] <- v_ML_2023_MRS - v_M_2023_MRS

tabela_4["LHM", "H"] <- v_HL_2023_MRS - v_L_2023_MRS
tabela_4["LHM", "M"] <- v_HML_2023_MRS - v_HL_2023_MRS
tabela_4["LHM", "L"] <- v_L_2023_MRS

tabela_4["LMH", "H"] <- v_HML_2023_MRS - v_ML_2023_MRS
tabela_4["LMH", "M"] <- v_ML_2023_MRS - v_L_2023_MRS
tabela_4["LMH", "L"] <- v_L_2023_MRS

tabela_4["Shapley Values 2023 (MRS)", ] <- colMeans(tabela_4[1:6, ], na.rm = TRUE)
tabela_4["Shapley Values Relative 2023 (MRS)", ] <- tabela_4["Shapley Values 2023 (MRS)", ] / v_HML_2023_MRS
print(round(tabela_4, 4))







########################################################################################################
###################################### VALUE AT RISK ###################################################
#########################################################################################################

########################################## MVP 2022 ###################################################

posicao_var <- 12.5  # 250 X 0.05

# SINGULAR 
retornos_ord_high <- sort(série_high_2022)
retornos_ord_medium <- sort(série_medium_2022)
retornos_ord_low <- sort(série_low_2022)

VV_H_2022 <- mean(c(retornos_ord_high[12], retornos_ord_high[13])) 
VV_M_2022 <- mean(c(retornos_ord_medium[12], retornos_ord_medium[13])) 
VV_L_2022 <- mean(c(retornos_ord_low[12], retornos_ord_low[13])) 



# PARES

retornos_ord_high_medium_2022 <- sort(série_high_medium_2022)
retornos_ord_high_low_2022 <- sort(série_high_low_2022)
retornos_ord_medium_low_2022 <- sort(série_medium_low_2022)

VV_HM_2022 <- mean(c(retornos_ord_high_medium_2022[12], retornos_ord_high_medium_2022[13])) 
VV_HL_2022 <- mean(c(retornos_ord_high_low_2022[12], retornos_ord_high_low_2022[13])) 
VV_ML_2022 <- mean(c(retornos_ord_medium_low_2022[12], retornos_ord_medium_low_2022[13]))


# TRIPLA

retornos_ord_high_medium_low_2022 <- sort(série_high_medium_low_2022)
VV_HML_2022 <- mean(c(retornos_ord_high_medium_low_2022[12], retornos_ord_high_medium_low_2022[13]))




ordens <- c("HML", "HLM", "MHL", "MLH", "LHM", "LMH")
jogadores <- c("H", "M", "L")

tabela_var_1 <- data.frame(matrix(NA, nrow = 6, ncol = 3))
rownames(tabela_var_1) <- ordens
colnames(tabela_var_1) <- jogadores

tabela_var_1["HML", "H"] <- VV_H_2022
tabela_var_1["HML", "M"] <- VV_HM_2022 - VV_H_2022
tabela_var_1["HML", "L"] <- VV_HML_2022 - VV_HM_2022 

tabela_var_1["HLM", "H"] <- VV_H_2022
tabela_var_1["HLM", "M"] <- VV_HML_2022 - VV_HL_2022
tabela_var_1["HLM", "L"] <- VV_HL_2022 - VV_H_2022 

tabela_var_1["MHL", "H"] <- VV_HM_2022 - VV_M_2022
tabela_var_1["MHL", "M"] <- VV_M_2022
tabela_var_1["MHL", "L"] <- VV_HML_2022 - VV_HM_2022

tabela_var_1["MLH", "H"] <- VV_HML_2022 - VV_ML_2022
tabela_var_1["MLH", "M"] <- VV_M_2022 
tabela_var_1["MLH", "L"] <- VV_ML_2022 - VV_M_2022

tabela_var_1["LHM", "H"] <- VV_HL_2022 - VV_L_2022
tabela_var_1["LHM", "M"] <- VV_HML_2022 - VV_HL_2022
tabela_var_1["LHM", "L"] <- VV_L_2022

tabela_var_1["LMH", "H"] <- VV_HML_2022 - VV_ML_2022
tabela_var_1["LMH", "M"] <- VV_ML_2022 - VV_L_2022
tabela_var_1["LMH", "L"] <- VV_L_2022

tabela_var_1["Shapley Values for VaR 2022 (PVM)", ] <- colMeans(tabela_var_1[1:6, ], na.rm = TRUE)
tabela_var_1["Raltive Shapley Values for VaR 2022 (PVM)", ] <- tabela_var_1["Shapley Values for VaR 2022 (PVM)", ] / VV_HML_2022

print(round(tabela_var_1, 4))


########################################## MVP 2023 ###################################################


# SINGULAR 
retornos_ord_high_2023 <- sort(série_high_2023)
retornos_ord_medium_2023 <- sort(série_medium_2023)
retornos_ord_low_2023 <- sort(série_low_2023)

VV_H_2023 <- mean(c(retornos_ord_high_2023[12], retornos_ord_high_2023[13])) 
VV_M_2023 <- mean(c(retornos_ord_medium_2023[12], retornos_ord_medium_2023[13])) 
VV_L_2023 <- mean(c(retornos_ord_low_2023[12], retornos_ord_low_2023[13])) 



# PARES

retornos_ord_high_medium_2023 <- sort(série_high_medium_2023)
retornos_ord_high_low_2023 <- sort(série_high_low_2023)
retornos_ord_medium_low_2023 <- sort(série_medium_low_2023)

VV_HM_2023 <- mean(c(retornos_ord_high_medium_2023[12], retornos_ord_high_medium_2023[13])) 
VV_HL_2023 <- mean(c(retornos_ord_high_low_2023[12], retornos_ord_high_low_2023[13])) 
VV_ML_2023 <- mean(c(retornos_ord_medium_low_2023[12], retornos_ord_medium_low_2023[13]))


# TRIPLA

retornos_ord_high_medium_low_2023 <- sort(série_high_medium_low_2023)
VV_HML_2023 <- mean(c(retornos_ord_high_medium_low_2023[12], retornos_ord_high_medium_low_2023[13]))



ordens <- c("HML", "HLM", "MHL", "MLH", "LHM", "LMH")
jogadores <- c("H", "M", "L")

tabela_var_2 <- data.frame(matrix(NA, nrow = 6, ncol = 3))
rownames(tabela_var_2) <- ordens
colnames(tabela_var_2) <- jogadores

tabela_var_2["HML", "H"] <- VV_H_2023
tabela_var_2["HML", "M"] <- VV_HM_2023 - VV_H_2023
tabela_var_2["HML", "L"] <- VV_HML_2023 - VV_HM_2023 

tabela_var_2["HLM", "H"] <- VV_H_2023
tabela_var_2["HLM", "M"] <- VV_HML_2023 - VV_HL_2023
tabela_var_2["HLM", "L"] <- VV_HL_2023 - VV_H_2023 

tabela_var_2["MHL", "H"] <- VV_HM_2023 - VV_M_2023
tabela_var_2["MHL", "M"] <- VV_M_2023
tabela_var_2["MHL", "L"] <- VV_HML_2023 - VV_HM_2023

tabela_var_2["MLH", "H"] <- VV_HML_2023 - VV_ML_2023
tabela_var_2["MLH", "M"] <- VV_M_2023 
tabela_var_2["MLH", "L"] <- VV_ML_2023 - VV_M_2023

tabela_var_2["LHM", "H"] <- VV_HL_2023 - VV_L_2023
tabela_var_2["LHM", "M"] <- VV_HML_2023 - VV_HL_2023
tabela_var_2["LHM", "L"] <- VV_L_2023

tabela_var_2["LMH", "H"] <- VV_HML_2023 - VV_ML_2023
tabela_var_2["LMH", "M"] <- VV_ML_2023 - VV_L_2023
tabela_var_2["LMH", "L"] <- VV_L_2023

tabela_var_2["Shapley Values for VaR 2023 (PVM)", ] <- colMeans(tabela_var_2[1:6, ], na.rm = TRUE)
tabela_var_2["Relative Shapley Values for VaR 2023 (PVM)", ] <- tabela_var_2["Shapley Values for VaR 2023 (PVM)", ] / VV_HML_2023

print(round(tabela_var_2, 4))



####################################### MSRP 2022 ############################################


# SINGULAR 
retornos_ord_high_MRS <- sort(série_high_2022_MRS)
retornos_ord_medium_MRS <- sort(série_medium_2022_MRS)
retornos_ord_low_MRS <- sort(série_low_2022_MRS)

VV_H_2022_MRS <- mean(c(retornos_ord_high_MRS[12], retornos_ord_high_MRS[13])) 
VV_M_2022_MRS <- mean(c(retornos_ord_medium_MRS[12], retornos_ord_medium_MRS[13])) 
VV_L_2022_MRS <- mean(c(retornos_ord_low_MRS[12], retornos_ord_low_MRS[13])) 



# PAREShttp://127.0.0.1:21297/graphics/0754fbdb-66b9-4022-970e-a5c87b3f1014.png

retornos_ord_high_medium_2022_MRS <- sort(série_high_medium_2022_MRS)
retornos_ord_high_low_2022_MRS <- sort(série_high_low_2022_MRS)
retornos_ord_medium_low_2022_MRS <- sort(série_medium_low_2022_MRS)

VV_HM_2022_MRS <- mean(c(retornos_ord_high_medium_2022_MRS[12], retornos_ord_high_medium_2022_MRS[13])) 
VV_HL_2022_MRS <- mean(c(retornos_ord_high_low_2022_MRS[12], retornos_ord_high_low_2022_MRS[13])) 
VV_ML_2022_MRS <- mean(c(retornos_ord_medium_low_2022_MRS[12], retornos_ord_medium_low_2022_MRS[13]))


# TRIPLA

retornos_ord_high_medium_low_2022_MRS <- sort(série_high_medium_low_2022_MRS)
VV_HML_2022_MRS <- mean(c(retornos_ord_high_medium_low_2022_MRS[12], retornos_ord_high_medium_low_2022_MRS[13]))



ordens <- c("HML", "HLM", "MHL", "MLH", "LHM", "LMH")
jogadores <- c("H", "M", "L")

tabela_var_3 <- data.frame(matrix(NA, nrow = 6, ncol = 3))
rownames(tabela_var_3) <- ordens
colnames(tabela_var_3) <- jogadores

tabela_var_3["HML", "H"] <- VV_H_2022_MRS
tabela_var_3["HML", "M"] <- VV_HM_2022_MRS - VV_H_2022_MRS
tabela_var_3["HML", "L"] <- VV_HML_2022_MRS - VV_HM_2022_MRS 

tabela_var_3["HLM", "H"] <- VV_H_2022_MRS
tabela_var_3["HLM", "M"] <- VV_HML_2022_MRS - VV_HL_2022_MRS
tabela_var_3["HLM", "L"] <- VV_HL_2022_MRS - VV_H_2022_MRS 

tabela_var_3["MHL", "H"] <- VV_HM_2022_MRS - VV_M_2022_MRS
tabela_var_3["MHL", "M"] <- VV_M_2022_MRS
tabela_var_3["MHL", "L"] <- VV_HML_2022_MRS - VV_HM_2022_MRS

tabela_var_3["MLH", "H"] <- VV_HML_2022_MRS - VV_ML_2022_MRS
tabela_var_3["MLH", "M"] <- VV_M_2022_MRS 
tabela_var_3["MLH", "L"] <- VV_ML_2022_MRS - VV_M_2022_MRS

tabela_var_3["LHM", "H"] <- VV_HL_2022_MRS - VV_L_2022_MRS
tabela_var_3["LHM", "M"] <- VV_HML_2022_MRS - VV_HL_2022_MRS
tabela_var_3["LHM", "L"] <- VV_L_2022_MRS

tabela_var_3["LMH", "H"] <- VV_HML_2022_MRS - VV_ML_2022_MRS
tabela_var_3["LMH", "M"] <- VV_ML_2022_MRS - VV_L_2022_MRS
tabela_var_3["LMH", "L"] <- VV_L_2022_MRS

tabela_var_3["Shapley Values for VaR 2022 (MSRP)", ] <- colMeans(tabela_var_3[1:6, ], na.rm = TRUE)
tabela_var_3["Relative Shapley Values for VaR 2022 (MSRP)", ] <- tabela_var_3["Shapley Values for VaR 2022 (MSRP)", ] / VV_HML_2022_MRS

print(round(tabela_var_3, 4))


####################################### MSRP 2023 ############################################

# SINGULAR 
retornos_ord_high_2023_MRS <- sort(série_high_2023_MRS)
retornos_ord_medium_2023_MRS <- sort(série_medium_2023_MRS)
retornos_ord_low_2023_MRS <- sort(série_low_2023_MRS)

VV_H_2023_MRS <- mean(c(retornos_ord_high_2023_MRS[12], retornos_ord_high_2023_MRS[13])) 
VV_M_2023_MRS <- mean(c(retornos_ord_medium_2023_MRS[12], retornos_ord_medium_2023_MRS[13])) 
VV_L_2023_MRS <- mean(c(retornos_ord_low_2023_MRS[12], retornos_ord_low_2023_MRS[13])) 


# PARES

retornos_ord_high_medium_2023_MRS <- sort(série_high_medium_2023_MRS)
retornos_ord_high_low_2023_MRS <- sort(série_high_low_2023_MRS)
retornos_ord_medium_low_2023_MRS <- sort(série_medium_low_2023_MRS)

VV_HM_2023_MRS <- mean(c(retornos_ord_high_medium_2023_MRS[12], retornos_ord_high_medium_2023_MRS[13])) 
VV_HL_2023_MRS <- mean(c(retornos_ord_high_low_2023_MRS[12], retornos_ord_high_low_2023_MRS[13])) 
VV_ML_2023_MRS <- mean(c(retornos_ord_medium_low_2023_MRS[12], retornos_ord_medium_low_2023_MRS[13]))


# TRIPLA

retornos_ord_high_medium_low_2023_MRS <- sort(série_high_medium_low_2023_MRS)
VV_HML_2023_MRS <- mean(c(retornos_ord_high_medium_low_2023_MRS[12], retornos_ord_high_medium_low_2023_MRS[13]))




ordens <- c("HML", "HLM", "MHL", "MLH", "LHM", "LMH")
jogadores <- c("H", "M", "L")

tabela_var_4 <- data.frame(matrix(NA, nrow = 6, ncol = 3))
rownames(tabela_var_4) <- ordens
colnames(tabela_var_4) <- jogadores

tabela_var_4["HML", "H"] <- VV_H_2023_MRS
tabela_var_4["HML", "M"] <- VV_HM_2023_MRS - VV_H_2023_MRS
tabela_var_4["HML", "L"] <- VV_HML_2023_MRS - VV_HM_2023_MRS 

tabela_var_4["HLM", "H"] <- VV_H_2023_MRS
tabela_var_4["HLM", "M"] <- VV_HML_2023_MRS - VV_HL_2023_MRS
tabela_var_4["HLM", "L"] <- VV_HL_2023_MRS - VV_H_2023_MRS 

tabela_var_4["MHL", "H"] <- VV_HM_2023_MRS - VV_M_2023_MRS
tabela_var_4["MHL", "M"] <- VV_M_2023_MRS
tabela_var_4["MHL", "L"] <- VV_HML_2023_MRS - VV_HM_2023_MRS

tabela_var_4["MLH", "H"] <- VV_HML_2023_MRS - VV_ML_2023_MRS
tabela_var_4["MLH", "M"] <- VV_M_2023_MRS 
tabela_var_4["MLH", "L"] <- VV_ML_2023_MRS - VV_M_2023_MRS

tabela_var_4["LHM", "H"] <- VV_HL_2023_MRS - VV_L_2023_MRS
tabela_var_4["LHM", "M"] <- VV_HML_2023_MRS - VV_HL_2023_MRS
tabela_var_4["LHM", "L"] <- VV_L_2023_MRS

tabela_var_4["LMH", "H"] <- VV_HML_2023_MRS - VV_ML_2023_MRS
tabela_var_4["LMH", "M"] <- VV_ML_2023_MRS - VV_L_2023_MRS
tabela_var_4["LMH", "L"] <- VV_L_2023_MRS

tabela_var_4["Shapley Values for VaR 2023 (MSRP)", ] <- colMeans(tabela_var_4[1:6, ], na.rm = TRUE)
tabela_var_4["Relative Shapley Values for VaR 2023 (MSRP)", ] <- tabela_var_4["Shapley Values for VaR 2023 (MSRP)", ] / VV_HML_2023_MRS

print(round(tabela_var_4, 4))










############################################################################################################################
#################################### BETA DE MERCADO  ##############################################################################
####################################################################################################################################


library(quantmod)


start_date <- as.Date("2022-01-01")
end_date <- as.Date("2024-01-01")


getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date)
sp500_prices <- Ad(GSPC)


sp500_2022 <- sp500_prices["2022"]
sp500_2023 <- sp500_prices["2023"]


log_returns <- function(data) {
  return(diff(log(as.matrix(data))) * 100)
}


returns_2022 <- log_returns(sp500_2022)
returns_2023 <- log_returns(sp500_2023)

nrow(returns_2022)
nrow(returns_2023)



# beta
calcular_beta <- function(retornos_carteira, retornos_mercado) {
  cov_matrix <- cov(retornos_carteira, retornos_mercado, use = 'complete.obs')
  beta <- cov(retornos_carteira, retornos_mercado, use = "complete.obs") /
    var(retornos_mercado, na.rm = TRUE)
  return(beta)
}


beta_high_2022 <- calcular_beta(série_high_2022, returns_2022)
beta_medium_2022 <- calcular_beta(série_medium_2022, returns_2022)
beta_low_2022 <- calcular_beta(série_low_2022, returns_2022)

beta_high_2022_MRS <- calcular_beta(série_high_2022_MRS, returns_2022)
beta_medium_2022_MRS <- calcular_beta(série_medium_2022_MRS, returns_2022)
beta_low_2022_MRS <- calcular_beta(série_low_2022_MRS, returns_2022)

# 2023

beta_high_2023 <- calcular_beta(série_high_2023, returns_2023)
beta_medium_2023 <- calcular_beta(série_medium_2023, returns_2023)
beta_low_2023 <- calcular_beta(série_low_2023, returns_2023)

beta_high_2023_MRS <- calcular_beta(série_high_2023_MRS, returns_2023)
beta_medium_2023_MRS <- calcular_beta(série_medium_2023_MRS, returns_2023)
beta_low_2023_MRS <- calcular_beta(série_low_2023_MRS, returns_2023)

# resultados
print(round(c(
  beta_high_2022, beta_medium_2022, beta_low_2022,
  beta_high_2022_MRS, beta_medium_2022_MRS, beta_low_2022_MRS,
  beta_high_2023, beta_medium_2023, beta_low_2023,
  beta_high_2023_MRS, beta_medium_2023_MRS, beta_low_2023_MRS
), 4))



























# Carregar biblioteca para salvar em Excel
library(writexl)

# Função para criar um dataframe com o número correto de linhas (preenchendo com NA quando necessário)
ajustar_tamanho <- function(lista_pesos) {
  max_linhas <- max(sapply(lista_pesos, length))  # Encontrar o maior vetor de pesos
  df <- do.call(cbind, lapply(lista_pesos, function(x) {
    length_x <- length(x)
    if (length_x < max_linhas) {
      c(x, rep(NA, max_linhas - length_x))  # Preencher com NA
    } else {
      x
    }
  }))
  return(as.data.frame(df))
}

# Criar listas de pesos para 2022
pesos_2022 <- ajustar_tamanho(list(
  High_2022 = resultados_individual_2022[[1]]$x_star,
  Medium_2022 = resultados_individual_2022[[2]]$x_star,
  Low_2022 = resultados_individual_2022[[3]]$x_star,
  High_Medium_2022 = resultados_combinados_2022[[1]]$x_star,
  High_Low_2022 = resultados_combinados_2022[[2]]$x_star,
  Medium_Low_2022 = resultados_combinados_2022[[3]]$x_star,
  High_Medium_Low_2022 = resultados_hml_2022$x_star
))

# Criar listas de pesos para 2023
pesos_2023 <- ajustar_tamanho(list(
  High_2023 = resultados_individual_2023[[1]]$x_star,
  Medium_2023 = resultados_individual_2023[[2]]$x_star,
  Low_2023 = resultados_individual_2023[[3]]$x_star,
  High_Medium_2023 = resultados_combinados_2023[[1]]$x_star,
  High_Low_2023 = resultados_combinados_2023[[2]]$x_star,
  Medium_Low_2023 = resultados_combinados_2023[[3]]$x_star,
  High_Medium_Low_2023 = resultados_hml_2023$x_star
))

# Criar listas para os pesos das carteiras de maior Sharpe Ratio (MRS) - 2022
pesos_MRS_2022 <- ajustar_tamanho(list(
  High_2022_MRS = resultados_individual_2022_MRS[[1]]$x_MRS,
  Medium_2022_MRS = resultados_individual_2022_MRS[[2]]$x_MRS,
  Low_2022_MRS = resultados_individual_2022_MRS[[3]]$x_MRS,
  High_Medium_2022_MRS = resultados_combinados_2022_MRS[[1]]$x_MRS,
  High_Low_2022_MRS = resultados_combinados_2022_MRS[[2]]$x_MRS,
  Medium_Low_2022_MRS = resultados_combinados_2022_MRS[[3]]$x_MRS,
  High_Medium_Low_2022_MRS = resultados_hml_2022_MRS$x_MRS
))

# Criar listas para os pesos das carteiras de maior Sharpe Ratio (MRS) - 2023
pesos_MRS_2023 <- ajustar_tamanho(list(
  High_2023_MRS = resultados_individual_2023_MRS[[1]]$x_MRS,
  Medium_2023_MRS = resultados_individual_2023_MRS[[2]]$x_MRS,
  Low_2023_MRS = resultados_individual_2023_MRS[[3]]$x_MRS,
  High_Medium_2023_MRS = resultados_combinados_2023_MRS[[1]]$x_MRS,
  High_Low_2023_MRS = resultados_combinados_2023_MRS[[2]]$x_MRS,
  Medium_Low_2023_MRS = resultados_combinados_2023_MRS[[3]]$x_MRS,
  High_Medium_Low_2023_MRS = resultados_hml_2023_MRS$x_MRS
))

# Salvar em um único arquivo Excel, com cada data frame em uma aba diferente
write_xlsx(
  list(
    "Pesos PVM 2022" = pesos_2022,
    "Pesos PVM 2023" = pesos_2023,
    "Pesos MRS 2022" = pesos_MRS_2022,
    "Pesos MRS 2023" = pesos_MRS_2023
  ),
  "Pesos_Carteiras.xlsx"
)

print("Arquivo Excel salvo com sucesso!")






# Criar dataframes para armazenar as séries temporais

series_2022 <- data.frame(
  Date = high_2022$Date,
  High_2022 = série_high_2022,
  Medium_2022 = série_medium_2022,
  Low_2022 = série_low_2022,
  High_Medium_2022 = série_high_medium_2022,
  High_Low_2022 = série_high_low_2022,
  Medium_Low_2022 = série_medium_low_2022,
  High_Medium_Low_2022 = série_high_medium_low_2022
)

series_2023 <- data.frame(
  Date = high_2023$Date,
  High_2023 = série_high_2023,
  Medium_2023 = série_medium_2023,
  Low_2023 = série_low_2023,
  High_Medium_2023 = série_high_medium_2023,
  High_Low_2023 = série_high_low_2023,
  Medium_Low_2023 = série_medium_low_2023,
  High_Medium_Low_2023 = série_high_medium_low_2023
)

series_2022_MRS <- data.frame(
  Date = high_2022$Date,
  High_2022_MRS = série_high_2022_MRS,
  Medium_2022_MRS = série_medium_2022_MRS,
  Low_2022_MRS = série_low_2022_MRS,
  High_Medium_2022_MRS = série_high_medium_2022_MRS,
  High_Low_2022_MRS = série_high_low_2022_MRS,
  Medium_Low_2022_MRS = série_medium_low_2022_MRS,
  High_Medium_Low_2022_MRS = série_high_medium_low_2022_MRS
)

series_2023_MRS <- data.frame(
  Date = high_2023$Date,
  High_2023_MRS = série_high_2023_MRS,
  Medium_2023_MRS = série_medium_2023_MRS,
  Low_2023_MRS = série_low_2023_MRS,
  High_Medium_2023_MRS = série_high_medium_2023_MRS,
  High_Low_2023_MRS = série_high_low_2023_MRS,
  Medium_Low_2023_MRS = série_medium_low_2023_MRS,
  High_Medium_Low_2023_MRS = série_high_medium_low_2023_MRS
)

# Salvar todas as séries em um único arquivo Excel
write_xlsx(
  list(
    "Series MVP 2022" = series_2022,
    "Series MVP 2023" = series_2023,
    "Series MRS 2022" = series_2022_MRS,
    "Series MRS 2023" = series_2023_MRS
  ),
  "Series_Temporais.xlsx"
)

print("Arquivo Excel com séries temporais salvo com sucesso!")
