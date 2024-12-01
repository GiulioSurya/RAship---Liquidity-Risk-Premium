rm(list = ls())
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
data <- read_excel("Cartel1.xlsx")
View(head(data))
str(data)
#trasforma il primo attributo in data e tutti gli altri attributi in numerici
data[[1]] <- as.Date(data[[1]], format = "%Y-%m-%d")
data[, -1] <- lapply(data[, -1], as.numeric)
str(data)

#controlla per NA
sum(is.na(data)) #devo capire come gestirli

save(data, file = "data.RData")


#inizia da qui
load("data.RData")


#prendi solo data, sp prince e close price
data<- data[, c(1, 2, 4, seq(4, ncol(data), by = 8))]
data<-data[,-3]


#################################   ROLL SPREAD   #################################
#dati per roll
data_roll <- data[,-2]
head(data_roll)
str(data_roll)




# Assicurati che 'data' sia una colonna di tipo Date
data_roll$data <- as.Date(data_roll$data)

# Calcola le differenze di prezzo giornaliere
price_changes <- data_roll %>%
    mutate(across(-data, ~ . - lag(.)))



#monthly_covariance <- price_changes %>%
#        group_by(month = floor_date(data_roll$data, "month")) %>% #raggruppa per mese
#        summarise(across(-data, ~ if (sum(complete.cases(.)) > 1 && sum(complete.cases(lag(.))) > 1) #controlla se ci sono casi completi sia nella colonna che nella colonna lag
#                                                            cov(., lag(.), use = "complete.obs") #se entrambi sono disponibili calcola la cov altrimenti no
#                                                    else NA_real_,
#                            .names = "cov_{.col}"))  

#monthly_covariance
monthly_covariance <- price_changes %>%
    group_by(month = floor_date(data_roll$data, "month")) %>%
    summarise(across(-data, ~ cov(., lag(.), use = "pairwise.complete.obs")))

roll <- monthly_covariance %>%
    mutate(across(-month, ~ ifelse(. < 0, 2 * sqrt(-.), 0)))


############## EFFECTIVE TICK #######################

rm(list = ls())

load("data.RData")

#prendi solo data, sp prince e close price
data<- data[, c(1, 2, 4, seq(4, ncol(data), by = 8))]
data<-data[,-3]
data_roll <- data[,-2]
data_effective1<-as.data.frame(data_roll)


data_effective1 <- data_effective1 %>%
  mutate(data = format(data, "%Y-%m"))

#data_amazon<-data_effective1[,1:3]
#questo non va, ma non perchè non vada la funzione. c'è un problema sul numero di gruppi. troppi pochi gruppi, ce ne vogliono almeno 3
#amazon_grouped<-data_amazon%>% group_by(data)
#number of groups
#nrow(attr(amazon_grouped, "groups"))



data_effective_grouped<-data_effective1%>% group_by(data)
nrow(attr(data_effective_grouped, "groups"))
#effective_tick<-matrix(NA, nrow = nrow(attr(amazon_grouped, "groups")), ncol = ncol(data_amazon)-1)
effective_tick<-matrix(NA, nrow = nrow(attr(data_effective_grouped, "groups")), ncol = ncol(data_effective1)-1)
for (m in 2:ncol(data_effective1)) {

for (i in 1:nrow(attr(data_effective_grouped, "groups"))) {
  #group<-attr(amazon_grouped, "groups")$.rows[[i]]
group<-attr(data_effective_grouped, "groups")$.rows[[i]]
#calcolo i cluster per gruppo 
 # cents<-data_amazon[group,m]-floor(data_amazon[group,m])
  cents<-data_effective1[group,m]-floor(data_effective1[group,m])
   cents<-round(cents*100)
#cluster
cluster <- ifelse(cents %% 100 == 0, 1.00, # Dollar
             ifelse(cents %% 25 == 0, 0.25, # Quarter
             ifelse(cents %% 10 == 0, 0.10, # Dime
             ifelse(cents %% 5 == 0, 0.05,  # Nickel
                    0.01))))#penny
N_j <- table(cluster)
F_j <- N_j / sum(N_j) 


# Step 4: Calcolare le probabilità non vincolate
#inizializzazione
U_j <- numeric(length(F_j))
names(U_j) <- names(F_j)


#per il primo cluster
U_j[1] <- 2 * F_j[1] 
#per gli altri

#qua è un po contorto, le riporto anche la formula iniziale che avevo usato
#U_j[1] <- 2 * F_j[1]
#    if (length(U_j) > 1) {
#        for (j in 2:(length(U_j) - 1)) {
#            U_j[j] <- 2 * F_j[j] - F_j[j - 1]
#        }
#        U_j[length(U_j)] <- F_j[length(U_j)] - F_j[length(U_j) - 1]
#    }
#questa non andava appena c'erano 2 gruppi, ho dovuto modificarla per considerare il caso con 2 gruppi

if (length(F_j) > 1) {
    # Ciclo per gruppi con almeno 3 elementi
    if (length(F_j) > 2) {
        for (j in 2:(length(F_j) - 1)) {
            U_j[j] <- 2 * F_j[j] - F_j[j - 1]
        }
    }
    # Assegnazione per l'ultimo elemento
    U_j[length(F_j)] <- F_j[length(F_j)] - F_j[length(F_j) - 1]
} else if (length(F_j) == 2) {
    # Caso con un solo elemento: assegna valore direttamente
    U_j[2] <- F_j[2] - F_j[1] # O un altro valore significativo
}

# Step 5: Vincolare le probabilità
#inizializzazione
g_j <- numeric(length(U_j))
names(g_j) <- names(U_j)


  g_j[1] <- pmin(pmax(U_j[1], 0), 1)
if(length(g_j) > 1) {
  for (j in 2:length(U_j)) {
    g_j[j] <- pmin(pmax(U_j[j], 0), 1 - sum(g_j[1:(j - 1)]))
  }
}


sum(g_j)
# Step 6: Determinare lo spread effettivo per ciascun cluster
s_j <- as.numeric(names(F_j))

# Step 7: Calcolare il prezzo medio
#mean_price<- mean(data_amazon[group,m])
mean_price<- mean(data_effective1[group,m])

# Step 8: Calcolare l'Effective Tick
effective_tick[i,m-1]<- sum(g_j * s_j) / mean_price

}
}


#################### EFFECTIVE TICK FUNCTION###############

calc_tick <- function(X) { 
  effective_tick <- numeric(ncol(X) - 1)

  for (i in 2:(ncol(X))) {
    cents <- X[,i] - floor(X[,i])
    cents <- round(cents*100)
    #cluster
    cluster <- ifelse(cents %% 100 == 0, 1.00,                    # Dollar
                      ifelse(cents %% 25 == 0, 0.25,              # Quarter
                             ifelse(cents %% 10 == 0, 0.10,       # Dime
                                    ifelse(cents %% 5 == 0, 0.05, # Nickel
                                           0.01))))               # penny
    N_j <- table(cluster)
    F_j <- N_j / sum(N_j) 

    # Step 4: Calcolare le probabilità non vincolate
    #inizializzazione
    U_j <- numeric(length(F_j))
    names(U_j) <- names(F_j)

    
if (length(F_j) > 1) {
    # Ciclo per gruppi con almeno 3 elementi
    if (length(F_j) > 2) {
        for (j in 2:(length(F_j) - 1)) {
            U_j[j] <- 2 * F_j[j] - F_j[j - 1]
        }
    }
    # Assegnazione per l'ultimo elemento
    U_j[length(F_j)] <- F_j[length(F_j)] - F_j[length(F_j) - 1]
} else if (length(F_j) == 2) {
    # Caso con un solo elemento: assegna valore direttamente
    U_j[2] <- F_j[2] - F_j[1] # O un altro valore significativo
}
    # Step 5: Vincolare le probabilità
    #inizializzazione
    g_j <- numeric(length(U_j))
    names(g_j) <- names(U_j)

    g_j[1] <- pmin(pmax(U_j[1], 0), 1)
    if(length(g_j) > 1) {
      for (j in 2:length(U_j)) {
        g_j[j] <- pmin(pmax(U_j[j], 0), 1 - sum(g_j[1:(j - 1)]))
      }
    }

    # sum(g_j)
    # Step 6: Determinare lo spread effettivo per ciascun cluster
    s_j <- as.numeric(names(F_j))
    
    # Step 7: Calcolare il prezzo medio
    mean_price <- mean(data_effective1[,i])
    
    # Step 8: Calcolare l'Effective Tick
    effective_tick[i - 1L]<- sum(g_j * s_j) / mean_price
  }
  effective_tick |> t() |> as.data.frame() |> setNames(names(X)[-1L])
}

Month <- format(data_effective1$data, "%Y-%m")

effective_tick<-by(data_effective1, Month, calc_tick) |> 
  data.table::rbindlist(idcol = TRUE) |>
  as.data.frame()
#>       .id ABBOTT.LABORATORIES ALLSTATE.ORD.SHS
#> 1 2012-11        0.0003235766     0.0002465483

effective_tick<-split(data_effective1, Month) |>
  lapply(calc_tick) |> 
  data.table::rbindlist(idcol = TRUE) |>
  as.data.frame()
#>       .id ABBOTT.LABORATORIES ALLSTATE.ORD.SHS
#> 1 2012-11        0.0003235766     0.0002465483

#################### HOLDEN ####################################
rm(list = ls())
load("data.RData")

data<- data[, c(1, 2, 4, seq(4, ncol(data), by = 8))]
data<-data[,-3]
data_roll <- data[,-2]
data_holden<-as.data.frame(data_roll)

str(head(data_holden))
dput(head(data_holden))

data_test<-data_holden[,1:2]


############################## LOT ##############################

rm(list = ls())

library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(optimx)

load("data.RData")
data<- data[, c(1, 2, 4, seq(4, ncol(data), by = 8))]
data<-data[,-3]

data_lot<-data[,1:3]

# Rinominiamo le colonne per semplicità
colnames(data_lot) <- c("Date", "Market_Price", "Stock_Price")

# Calcoliamo i rendimenti giornalieri per il mercato e il titolo
data_lot <- data_lot[order(data_lot$Date), ]  # Assicuriamoci che i dati siano ordinati per data

data_lot$R_mt <- c(NA, diff(data_lot$Market_Price) / data_lot$Market_Price[-nrow(data_lot)])
data_lot$R_jt <- c(NA, diff(data_lot$Stock_Price) / data_lot$Stock_Price[-nrow(data_lot)])

# Rimuoviamo le prime osservazioni NA
data_lot <- na.omit(data_lot)

# Classifichiamo le osservazioni nelle regioni:
# Regione 1: R_jt != 0 e R_mt < 0 (Regione di Vendita)
# Regione 2: R_jt != 0 e R_mt > 0 (Regione di Acquisto)
# Regione 0: R_jt == 0 (Nessuna Transazione)


data_lot$Region <- with(data_lot, ifelse(R_jt == 0, 0,
                                  ifelse(R_mt < 0 & R_jt != 0, 1,
                                         ifelse(R_mt > 0 & R_jt != 0, 2, NA))))

# Rimuoviamo eventuali osservazioni NA nella colonna 'Region'
data_lot <- data_lot[!is.na(data_lot$Region), ]

#prendere in cosindierazione di inizializzare beta in qualche modo


# Impostiamo le stime iniziali per gli altri parametri
initial_alpha1 <- -0.01  # Deve essere negativo
initial_alpha2 <- 0.01  # Deve essere positivo
initial_sigma <- 1
initial_beta <- 0.5

# Funzione di log-verosimiglianza
log_likelihood <- function(params, data) {
  alpha1 <- params[1]  # Soglia inferiore (negativa)
  alpha2 <- params[2]  # Soglia superiore (positiva)
  beta_j <- params[3]  # Coefficiente beta
  sigma_j <- params[4] # Deviazione standard (positiva)
  
  if (alpha1 >= alpha2) return(Inf)
    if (sigma_j <= 0) return(Inf)
    if(beta_j <= 0) return(Inf)
    
  R_mt <- data$R_mt
  R_jt <- data$R_jt

  
  # Calcoliamo R_jt^* (rendimenti veri non osservati)
  #qua devo aggiungerlo epsilon? in caso come?

  R_star <- beta_j * R_mt  
  

  # Calcoliamo la log-verosimiglianza per ciascuna regione
  ll <- numeric(3)

#questi servono per la regione 0
  z1 <- (alpha2 - beta_j * R_mt) / sigma_j
    z2 <- (alpha1 - beta_j * R_mt) / sigma_j
  
  # Regione 1: R_jt != 0 e R_mt < 0
  idx1 <- which(data$Region == 1)
  R_jt_obs1 <- R_jt[idx1]
  R_mt_obs1 <- R_mt[idx1]
  ll[1] <- sum(log(0.5*(2*pi*sigma_j^2)^0.5)) - sum((0.5*(2*sigma_j^2))*(R_jt_obs1 - alpha1 - beta_j * R_mt_obs1)^2)


  # Regione 2: R_jt != 0 e R_mt > 0
  idx2 <- which(data$Region == 2)
  R_jt_obs2 <- R_jt[idx2]
    R_mt_obs2 <- R_mt[idx2]
  ll[2] <- sum(log(0.5*(2*pi*sigma_j^2)^0.5)) - sum((0.5*(2*sigma_j^2))*(R_jt_obs2 - alpha2 - beta_j * R_mt_obs2)^2)
  
  # Regione 0: R_jt == 0
  idx0 <- which(data$Region == 0)
  z1_0 <- z1[idx0]
  z2_0 <- z2[idx0]
  ll[3] <- sum(log(pnorm(z1_0) - pnorm(z2_0) + 1e-10))
 # Aggiungiamo un piccolo valore per evitare log(0)


  
  # Calcoliamo la log-verosimiglianza totale (negativa per minimizzare)
  return(-sum(ll))
}

# Impostiamo i parametri iniziali
initial_params <- c(alpha1 = initial_alpha1,
                    alpha2 = initial_alpha2,
                    beta_j = initial_beta,
                    sigma_j = initial_sigma)

# Impostiamo i vincoli sui parametri
lower_bounds <- c(-Inf, 0, 0, 0)
upper_bounds <- c(0, Inf, Inf, Inf)

# Utilizziamo l'ottimizzazione numerica per massimizzare la log-verosimiglianza
optim_result <- optimx(par = initial_params,
                       fn = log_likelihood,
                       data = data_lot,
                       lower = lower_bounds,
                       upper = upper_bounds,
                       method = c("L-BFGS-B", "nlminb", "Nelder-Mead", "spg", "BFGS"))

# Estraiamo i parametri stimati
estimated_params <- optim_result[2,1:4]


LOT_cost <- estimated_params[2] - estimated_params[1]

beta<-estimated_params[3]
Rmt<-data_lot$R_mt
R_jt_start<-Rmt*as.numeric(beta)

error<-data_lot$R_jt-R_jt_start
sigma_j<-as.numeric(estimated_params[4])

plot(density(error), main = "Error Density", xlab = "Error", ylab = "Density")


