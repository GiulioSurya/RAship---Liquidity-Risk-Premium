rm(list = ls())

library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
# library(optimx)

load("data.RData")
data<- data[, c(1, 2, 4, seq(4, ncol(data), by = 8))]
data<-data[,-3]

data_lot<-data[,1:3]

# Rinominiamo le colonne per semplicità
colnames(data_lot) <- c("Date", "Market_Price", "Stock_Price")

# Calcoliamo i rendimenti giornalieri per il mercato e il titolo
data_lot <- data_lot[order(data_lot$Date), ]  # Assicuriamoci che i dati siano ordinati per data

#data_lot$R_mt <- c(NA, diff(data_lot$Market_Price) / data_lot$Market_Price[-nrow(data_lot)])
#data_lot$R_jt <- c(NA, diff(data_lot$Stock_Price) / data_lot$Stock_Price[-nrow(data_lot)])

#data_lot$R_mt <-c(NA,diff(log(data_lot$Market_Price)))
#data_lot$R_jt <-c(NA,diff(log(data_lot$Stock_Price)))
# Rimuoviamo le prime osservazioni NA
#data_lot <- na.omit(data_lot)

# Classifichiamo le osservazioni nelle regioni:
# Regione 1: R_jt != 0 e R_mt < 0 (Regione di Vendita)
# Regione 2: R_jt != 0 e R_mt > 0 (Regione di Acquisto)
# Regione 0: R_jt == 0 (Nessuna Transazione)


#data_lot$Region <- with(data_lot, ifelse(R_jt == 0, 0,
 #                                 ifelse(R_mt < 0 & R_jt != 0, 1,
  #                                      ifelse(R_mt > 0 & R_jt != 0, 2, NA))))

# Rimuoviamo eventuali osservazioni NA nella colonna 'Region'
#data_lot <- data_lot[!is.na(data_lot$Region), ]

#prendere in cosindierazione di inizializzare beta in qualche modo


# Impostiamo le stime iniziali per gli altri parametri
initial_alpha1 <- -0.001  # Deve essere negativo
initial_alpha2 <- 0.001  # Deve essere positivo
initial_sigma <- sd(residuals(lm(diff(log(data_lot$Stock_Price)) ~ diff(log(data_lot$Market_Price)))))
initial_beta <- coef(lm(diff(log(data_lot$Stock_Price)) ~ diff(log(data_lot$Market_Price))))[2]

# Funzione di log-verosimiglianza
log_likelihood <- function(params, data) {
  alpha1 <- -exp(params[1])  # Soglia inferiore (negativa)
  alpha2 <- exp(params[2])  # Soglia superiore (positiva)
  beta_j <- params[3]  # Coefficiente beta
  sigma_j <- exp(params[4]) # Deviazione standard (positiva)

  

data$R_mt <-c(NA,diff(log(data$Market_Price)))
data$R_jt <-c(NA,diff(log(data$Stock_Price)))

  
  
  
  data$Region <- with(data, ifelse(R_jt == 0, 0,
                                 ifelse(R_mt < 0 & R_jt != 0, 1,
                                       ifelse(R_mt > 0 & R_jt != 0, 2, NA))))
  
  R_mt <- data$R_mt
  R_jt <- data$R_jt


  # Calcoliamo la log-verosimiglianza per ciascuna regione
  ll <- numeric(3)

  
  # Regione 1: R_jt != 0 e R_mt < 0
  idx1 <- which(data$Region == 1)
  R_jt_obs1 <- R_jt[idx1]
  R_mt_obs1 <- R_mt[idx1]
  #ll[1] <- sum(log((1/(2*pi*sigma_j^2)^0.5))) - sum(((1/(2*sigma_j^2)))*(R_jt_obs1 + alpha1 - beta_j * R_mt_obs1)^2)
  ll[1]<-sum(log((1/sigma_j)*dnorm((R_jt_obs1 + alpha1 - beta_j * R_mt_obs1)/sigma_j)))

  # Regione 2: R_jt != 0 e R_mt > 0
  idx2 <- which(data$Region == 2)
  R_jt_obs2 <- R_jt[idx2]
  R_mt_obs2 <- R_mt[idx2]
  #ll[2] <- sum(log((1/(2*pi*sigma_j^2)^0.5))) - sum(((1/(2*sigma_j^2)))*(R_jt_obs2 + alpha2 - beta_j * R_mt_obs2)^2)
  ll[2]<-sum(log((1/sigma_j)*dnorm((R_jt_obs2 + alpha2 - beta_j * R_mt_obs2)/sigma_j)))

  #questi servono per la regione 0
  z2 <- (alpha2 - beta_j * R_mt) / sigma_j
  z1 <- (alpha1 - beta_j * R_mt) / sigma_j
  # Regione 0: R_jt == 0
  idx0 <- which(data$Region == 0)
  z1_0 <- z1[idx0]
  z2_0 <- z2[idx0]
  ll[3] <- sum(log(pnorm(z2_0) - pnorm(z1_0)))
 # Aggiungiamo un piccolo valore per evitare log(0)

  # Calcoliamo la log-verosimiglianza totale (negativa per minimizzare)
  return(-sum(ll))
}

# Impostiamo i parametri iniziali
initial_params <- c(alpha1 = initial_alpha1,
                    alpha2 = initial_alpha2,
                    beta_j = initial_beta,
                    sigma_j = initial_sigma)

# Utilizziamo l'ottimizzazione numerica per massimizzare la log-verosimiglianza
optim_result <- optim(initial_params, log_likelihood, , data_lot)

# Estraiamo i parametri stimati
estimated_params <- optim_result$par
estimated_params[1] <- -exp(estimated_params[1])
estimated_params[2] <- exp(estimated_params[2])
estimated_params[4] <- exp(estimated_params[4])

LOT_cost <- estimated_params[2] - estimated_params[1]


