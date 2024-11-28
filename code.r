rm(list = ls())
library(dplyr)
library(tidyr)
library(readxl)
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
subset_data <- data[, c(1, 2, 4, seq(4, ncol(data), by = 8))]
subset_data<-subset_data[,-3]


#################################   ROLL SPREAD   #################################
#dati per roll
data_roll <- subset_data[,-2]
head(data_roll)
str(data_roll)

library(dplyr)
library(lubridate)

# Assicurati che 'data' sia una colonna di tipo Date
data_roll$data <- as.Date(data_roll$data)

# Calcola le differenze di prezzo giornaliere
price_changes <- data_roll %>%
    mutate(across(-data, ~ . - lag(., default = first(.))))


monthly_covariance <- price_changes %>%
        group_by(month = floor_date(data_roll$data, "month")) %>% #raggruppa per mese
        summarise(across(-data, ~ if (sum(complete.cases(.)) > 1 && sum(complete.cases(lag(.))) > 1) #controlla se ci sono casi completi sia nella colonna che nella colonna lag
                                                            cov(., lag(.), use = "complete.obs") #se entrambi sono disponibili calcola la cov altrimenti no
                                                    else NA_real_,
                            .names = "cov_{.col}"))  

#trasforma la covarianza mensile in un dataframe
monthly_covariance <- as.data.frame(monthly_covariance)       

roll_amazon<-ifelse(monthly_covariance$cov_AMAZON.COM<0,2*sqrt(-monthly_covariance$cov_AMAZON.COM),0)
#questo non funziona, mi da nans non capito

# Isola i valori di covarianza negativi per Amazon
negative_cov_amazon <- monthly_covariance %>%
        filter(cov_AMAZON.COM < 0) %>%
        select(month, cov_AMAZON.COM)

roll_negative<-2*sqrt(-negative_cov_amazon$cov_AMAZON.COM)
roll_negative
#cosi funziona, non capisco, ho controllato numero di positivi e negativi e sono uguali, non capisco cosa
#non piaccia alla funzione ifelse



#conta il numero di covarianze negative
sum(negative, na.rm = TRUE)

#crea una funzione che calcola roll solo per valori negativi di covarianza altrimenti 0
roll <- function(x) {
    if (x < 0) {
        return(2 * sqrt(-x))
    } else {
        return(0)
    }
}
#bhooooo ho dovuto creare una funzione

roll_amazon <- monthly_covariance$cov_AMAZON.COM %>%
        sapply(roll)


        # Crea una funzione che calcola roll solo per valori negativi di covarianza, altrimenti restituisce 0
     roll <- function(x) {
        if (is.na(x)) {  # Verifica NA per primo
                return(NA)
        } else if (x < 0) {  # Applica la condizione solo se non-NA e negativo
                return(2 * sqrt(-x))
        } else {  # Gestisci tutti gli altri casi non negativi
                return(0)
        }
}


# Applica la funzione ai valori di covarianza eccetto per la colonna 'month'
roll <- monthly_covariance %>%
        mutate(across(-month, ~ sapply(., roll))) %>%
        select(-month)
