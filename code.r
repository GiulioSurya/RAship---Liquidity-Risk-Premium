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


