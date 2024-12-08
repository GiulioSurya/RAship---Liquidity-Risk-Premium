###############################################################################
# 1. Modello della formazione dei prezzi con componenti di spread effettivo.
# 2. Considera cluster di prezzi (trade) e cluster di midpoint (no-trade).
# 3. Parametrizzazione dei parametri per rispettare i vincoli (0≤mu,alpha≤1, 
#    somme gamma=1).
# 4. Calcolo della log-likelihood e stima dei parametri.
#
# Riferimenti al paper:
# Nel paper di Holden si illustra un modello di prezzo con:
# - Una componente fondamentale (V_t).
# - Un half-spread H_t = (S_t/2)*Q_t, dove Q_t indica la tipologia di operazione 
#   (buy = 1, sell = -1, no-trade = 0). L'half-spread rappresenta metà dello spread 
#   effettivo, che viene aggiunto o sottratto al midpoint a seconda del tipo 
#   di operazione.
# - Nei giorni di no-trade (probabilità = 1 - μ), il prezzo finale è un midpoint 
#   non influenzato da transazioni attive a bid/ask; al contrario, se c'è un trade
#   (con probabilità μ), il prezzo finale riflette l'aggiunta o la sottrazione 
#   dell'half-spread.
#
# L'obiettivo è stimare i parametri: 
# - μ (prob. di trade), 
# - α (percentuale dello spread attribuibile ad adverse selection),
# - γ_j (probabilità di ciascun spread effettivo possibile),
# - σ_e (deviazione standard dell'errore),
# - e_bar (media dell'errore).
#
# La log-likelihood usa triplette (P_t, P_{t+1}, P_{t+2}) per catturare 
# la dipendenza temporale. Per ogni tripletta di prezzi osservati e cluster 
# osservati, si integrano (sommando) tutte le possibili combinazioni di 
# half-spread H_t, H_{t+1}, H_{t+2} coerenti con i cluster stessi(gli spread possono essere generati da alcuni cluster, le tabelle A,B e D 
#del paper example spiegano come), pesandole 
# con le relative probabilità e la densità dell'errore.
#
# Nota sui cluster C_t:
# - I cluster identificano la "granularità" del prezzo. Ci sono J cluster 
#   "trade" (da 1 a J), ad esempio: 
#   1 = penny, 2 = nickel, 3 = dime, 4 = quarter, 5 = dollar.
# - Ci sono anche J cluster "midpoint" (da J+1 a 2J), che rappresentano 
#   giorni di no-trade. Ad esempio, se J=5, i cluster da 6 a 10 
#   rappresentano midpoint corrispondenti ai vari livelli.
#
# Il codice assegna a ogni giorno un cluster in base al prezzo finale 
# osservato. Se il giorno è no-trade, il cluster va da J+1 a 2J, 
# se è trade, da 1 a J. In questo modo "c" (il cluster) varia a seconda 
# del giorno osservato. Nel loop nella log-likelihood si pescano i cluster 
# osservati direttamente dal dataset (Ct_triplet), quindi la "c" è data 
# dall'osservazione, non da un calcolo interno.
###############################################################################

rm(list = ls())

# Carichiamo le librerie necessarie:
library(dplyr)   
library(optimx)  

load("data.RData")

###############################################################################
# PREPARAZIONE DEI DATI
###############################################################################
# Qui prepariamo i dati per l'analisi.
# - Ordiniamo per data
# - Calcoliamo la variazione giornaliera di prezzo (Delta_P)
# - Identifichiamo i cluster per i giorni di trade e no-trade.
###############################################################################
{
data <- data[,c("data","AMAZON.COM")]  
data <- data %>% arrange(data)         

# Calcoliamo ΔP_t = P_t - P_{t-1}, la variazione giornaliera del prezzo
data <- data %>%
  mutate(Delta_P = c(NA, diff(AMAZON.COM))) %>%
  filter(!is.na(Delta_P))

# Identifichiamo i cluster di prezzo "trade":
# Convertiamo il prezzo in centesimi e determiniamo la granularità.
cents <- (data$AMAZON.COM - floor(data$AMAZON.COM))*100
cents <- round(cents)

# Assegnazione del cluster trade in base alla divisibilità del prezzo in centesimi:
data$Price_Cluster_Trade <- ifelse(cents %% 100 == 0, 5,
                            ifelse(cents %% 25 == 0, 4,
                            ifelse(cents %% 10 == 0, 3,
                            ifelse(cents %% 5 == 0, 2, 1))))

# Numero di possibili spread J = 5
J <- 5
s_values <- c(0.01, 0.05, 0.10, 0.25, 1.00)   # possibili spread
h_values <- s_values / 2                     # half-spread = spread/2

# Identificazione dei giorni no-trade:
# Semplice euristica: se ΔP=0 => no-trade, quindi nessun cambiamento di prezzo.
#holden non mi pari evidenzi un metodo particolare per identificare i giorni no-trade, riprendendo il discorso
#che abbiamo fatto l'altra volta, ovvero che nei giorni no trade vengono riportati i prezzi del giorno precedente
#quest mi è sembrata l'idea più semplice. va bene ora, quando dovremo fare l'anallisi su mesi dovremmo vedere se
#come metodo è robusto o meno
no_trade_days <- data$Delta_P == 0

# Per i midpoint cluster, aggiungiamo un offset di mezzo tick:
cents_mid <- ((data$AMAZON.COM + 0.005) - floor(data$AMAZON.COM + 0.005))*100
cents_mid <- round(cents_mid)
data$Price_Cluster_Mid <- ifelse(cents_mid %% 100 == 0, 5,
                          ifelse(cents_mid %% 25 == 0, 4,
                          ifelse(cents_mid %% 10 == 0, 3,
                          ifelse(cents_mid %% 5 == 0, 2, 1))))

# Cluster finale:
# Se no-trade: cluster = C_{J + cluster_mid} (ovvero aggiungiamo J, 
# per mappare i cluster midpoint da J+1 a 2J).
# Se trade: cluster = cluster_trade (da 1 a J).
data$Price_Cluster <- ifelse(no_trade_days,
                             data$Price_Cluster_Mid + J,   
                             data$Price_Cluster_Trade)     

# Nota: In questo modo, ogni giorno ha un cluster compreso tra 1 e 2J:
# 1...J = trade clusters, J+1...2J = midpoint clusters.


###############################################################################
# DEFINIZIONE DI A_j E D_jk PER TRADE E MIDPOINT
#
# Queste matrici e vettori (A_j, D_jk, A_j_mid, D_jk_mid) 
# derivano dal paper e descrivono la distribuzione dei possibili incrementi 
# di prezzo e midpoint per i vari spread.
###############################################################################

A_j <- c(100, 20, 10, 4, 1)
D_jk <- matrix(0, nrow = J, ncol = J)
D_jk[1,1] <- 80
D_jk[2,1:2] <- c(8,8)
D_jk[3,1:3] <- c(8,8,8)
D_jk[4,1:4] <- c(3,3,1,3)
D_jk[5,1:5] <- c(1,1,1,1,1)

A_j_mid <- c(100, 20, 10, 4, 1)
D_jk_mid <- matrix(0, nrow = J, ncol = J)
D_jk_mid[1,1] <- 80
D_jk_mid[2,1:2] <- c(16,16)
D_jk_mid[3,3] <- 10
D_jk_mid[4,1] <- 4
D_jk_mid[4,2] <- 4
D_jk_mid[4,4] <- 4
D_jk_mid[5,5] <- 1

###############################################################################
# PARAMETRIZZAZIONE DEI PARAMETRI
#
# Trasformiamo i parametri raw in gamma, mu, alpha, sigma_e, e_bar 
# con le appropriate funzioni (logistiche, softmax(TY prof Farne), exp).
###############################################################################

transform_params <- function(params) {
  # params = c(gamma_raw(J-1), mu_raw, alpha_raw, sigma_e_raw, e_bar)
  gamma_raw <- params[1:(J-1)]
  mu_raw    <- params[J]
  alpha_raw <- params[J+1]
  sigma_e_raw <- params[J+2]
  e_bar    <- params[J+3]

  # Softmax standard per gamma
  gamma_raw_full <- c(gamma_raw, 0)
  exp_values <- exp(gamma_raw_full)
  gamma <- exp_values / sum(exp_values)

  # mu e alpha con logistic
  mu <- 1/(1+exp(-mu_raw))
  alpha <- 1/(1+exp(-alpha_raw))
  
  # sigma_e con exp
  sigma_e <- exp(sigma_e_raw)

  return(list(gamma=gamma, mu=mu, alpha=alpha, sigma_e=sigma_e, e_bar=e_bar))
}

###############################################################################
# CALCOLO PROBABILITA' DI CLUSTER C_t
#
# Pr(C_t=j) per i cluster trade e Pr(C_t=J+j) per i cluster midpoint.
###############################################################################

calculate_P_Ct_trade <- function(gamma, mu) {
  # Calcoliamo Pr(C_t=j) per j=1,...,J (trade)
  Pr_Ct <- numeric(J)
  for (j in 1:J) {
    sum_k <- 0
    for (k in 1:J) {
      # Somma su tutti gli spread k la prob gamma_k * mu * (D_jk/A_j[k])
      sum_k <- sum_k + gamma[k]*mu*(D_jk[j,k]/A_j[k])
    }
    Pr_Ct[j] <- sum_k
  }
  return(Pr_Ct)
}

calculate_P_Ct_mid <- function(gamma, mu) {
  # Calcoliamo Pr(C_t=J+j) per j=1,...,J (midpoint)
  Pr_Ct_mid <- numeric(J)
  for (j in 1:J) {
    sum_k <- 0
    for (k in 1:J) {
      # Per midpoint usiamo 1-mu invece di mu
      sum_k <- sum_k + gamma[k]*(1 - mu)*(D_jk_mid[j,k]/A_j_mid[k])
    }
    Pr_Ct_mid[j] <- sum_k
  }
  return(Pr_Ct_mid)
}

###############################################################################
# CALCOLO Pr(H_t | C_t) PER I TRADE
#
# Per i cluster trade calcoliamo la matrice Pr(H_t=h_k | C_t=j).
# Per i midpoint cluster, invece, sappiamo che H_t=0 con prob 1, e H_t!=0 con prob 0.
# Questo non lo facciamo qui, ma nella log-likelihood.
###############################################################################

calculate_P_Ht_given_Ct_trade <- function(gamma, mu, Pr_Ct) {
  Pr_Ht_given_Ct <- matrix(0, nrow=J, ncol=J)
  for (j in 1:J) {
    for (k in 1:J) {
      # Solo se Pr(C_t=j)>0 e k ≤ j (cioè non consideriamo half-spread più grandi del cluster)
      if (Pr_Ct[j]>0 && k<=j) {
        numerator <- gamma[k]*(mu/2)*(D_jk[j,k]/A_j[k])
        # Normalizziamo dividendo per Pr(C_t=j)
        Pr_Ht_given_Ct[j,k] <- numerator/Pr_Ct[j]
      }
    }
  }
  return(Pr_Ht_given_Ct)
}

###############################################################################
# FUNZIONE DI SUPPORTO PER e_t DAL PAPER
#
# e_t = ΔP_t - [H_t - (1 - α)*H_{t-1}], con ΔP_t = P_t - P_{t-1}.
#
# Questa funzione calc_e restituisce e_t dati P_curr, P_prev, H_curr, H_prev, alpha.
###############################################################################

calc_e <- function(P_curr, P_prev, H_curr, H_prev, alpha) {
  return((P_curr - P_prev) - (H_curr - (1 - alpha)*H_prev))
}

###############################################################################
# FUNZIONE DI LOG-LIKELIHOOD PER UNA TRIPLETTA (P_t, P_{t+1}, P_{t+2})
#
# Questa è la parte cruciale:
# - Prendiamo i parametri raw, li trasformiamo (gamma, mu, alpha, sigma_e, e_bar).
# - Calcoliamo Pr(C_t) per trade e midpoint.
# - Uniamo queste probabilità in un unico vettore Pr_Ct_all.
# - Dal dataset abbiamo Ct_triplet = (C_t, C_{t+1}, C_{t+2}), i cluster osservati 
#   per i tre giorni considerati.
# - Ricaviamo le probabilità osservate Pr(C_t), Pr(C_{t+1}), Pr(C_{t+2}) 
#   da Pr_Ct_all in base ai cluster del dataset.
# - Calcoliamo Pr(H_t|C_t) per i cluster trade (Pr_Ht_given_Ct_trade).
#   Per i midpoint non serve una matrice perché è fissata: 
#   H_t=0 con prob=1, H_t≠0 prob=0.
#
# Nel loop finale consideriamo tutte le combinazioni di H_t, H_{t+1}, H_{t+2} 
# compatibili con i cluster osservati:
# - Se cluster è trade (c ≤ J): H può assumere i valori h_values fino a j_ind.
# - Se cluster è midpoint (c > J): H_t=0 unica opzione.
#
# Per ogni combinazione:
# - Calcoliamo Pr(H_t|C_t), Pr(H_{t+1}|C_{t+1}), Pr(H_{t+2}|C_{t+2}).
# - Calcoliamo e_t1, e_t2 usando la formula del paper, con i P_t dati e i H_t combinati.
# - Calcoliamo la densità normale degli errori n_e_t1, n_e_t2.
#
# La likelihood per quella combinazione è:
# Pr(C_t)*Pr(C_{t+1})*Pr(C_{t+2}) * Pr(H_t|C_t)*Pr(H_{t+1}|C_{t+1})*Pr(H_{t+2}|C_{t+2}) * n_e_t1 * n_e_t2
#
# In log:
# ll_combination = log(Pr(C_t)) + log(Pr(C_{t+1})) + ... + log(n_e_t2)
#
# Infine sommiamo (in spazio normale) su tutte le combinazioni. In log, usiamo log(sum(exp(...)))
#
#perchè questo exp? mi veniva dei nans senza, chat gpt mi ha detto che pò essere "overflow" che ho capito essere dei log potenzialmente troppo piccoli
#mi sembra con o senza le stime vengano sepre uguali ma ci sono questi nans, problema? rallenta moltissimo la parte computazionale, non ho capito perchè
#
# Dove varia c?
# c è ottenuto da Ct_triplet, che proviene dal dataset. Quindi c è il cluster osservato 
# in quel giorno. Nel dataset, "Price_Cluster" assegna un numero da 1 a 2J a ciascun giorno:
# - Da 1 a J = trade cluster
# - Da J+1 a 2J = midpoint cluster
#
# Quindi "c" varia in base ai dati. Ogni giorno ha un cluster assegnato, 
# prelevato con Ct_triplet[t], Ct_triplet[t+1], Ct_triplet[t+2].
# Non c'è un "calcolo" interno della c, è semplicemente letta dal vettore Ct_triplet 
# che contiene l'osservazione del cluster per quei giorni.
###############################################################################

log_likelihood_triplet <- function(raw_params, Pt_triplet, Ct_triplet) {
  
  pars <- transform_params(raw_params)
  gamma <- pars$gamma
  mu <- pars$mu
  alpha <- pars$alpha
  sigma_e <- pars$sigma_e
  e_bar <- pars$e_bar

  # Calcolo delle probabilità di cluster
  Pr_Ct_trade <- calculate_P_Ct_trade(gamma, mu)
  Pr_Ct_mid <- calculate_P_Ct_mid(gamma, mu)

  # Uniamo i due vettori: i primi J sono trade, i successivi J sono midpoint
  Pr_Ct_all <- c(Pr_Ct_trade, Pr_Ct_mid)

  # Estraiamo Pr(C_t), Pr(C_{t+1}), Pr(C_{t+2}) dall'indice dei cluster 
  # Ct_triplet = (C_t, C_{t+1}, C_{t+2})
  # Quindi se C_t = 2, p_ct = Pr_Ct_all[2], se C_t1=7, p_ct1=Pr_Ct_all[7], etc.
  Pr_Ct_obs <- Pr_Ct_all[Ct_triplet]

  # Matrice per Pr(H_t|C_t) per i cluster trade
  Pr_Ht_given_Ct_trade <- calculate_P_Ht_given_Ct_trade(gamma, mu, Pr_Ct_trade)

  # Identifichiamo i cluster correnti:
  c_t <- Ct_triplet[1]   # cluster del giorno t
  c_t1 <- Ct_triplet[2]  # cluster del giorno t+1
  c_t2 <- Ct_triplet[3]  # cluster del giorno t+2

  # p_ct, p_ct1, p_ct2 sono le probabilità dei cluster osservati
  p_ct <- Pr_Ct_obs[1]
  p_ct1 <- Pr_Ct_obs[2]
  p_ct2 <- Pr_Ct_obs[3]

  # Funzione per ottenere i possibili H_t in base al cluster c
  get_H_combinations <- function(c) {
    # Se c ≤ J (trade cluster), 
    # possiamo avere h_values[1:j_ind] come possibili H_t
    # dove j_ind = c, poiché c stesso indica il cluster che determina quanti h sono possibili.
    if (c <= J) {
      j_ind <- c
      return(h_values[1:j_ind])
    } else {
      # Se c > J (midpoint cluster), 
      # l'unico H_t possibile è 0 (no trade = no half-spread).
      return(0)
    }
  }

  # Otteniamo i set possibili di H per i tre giorni:
  H_set_t <- get_H_combinations(c_t)
  H_set_t1 <- get_H_combinations(c_t1)
  H_set_t2 <- get_H_combinations(c_t2)

  ll_values <- c() # memorizzerà la log-likelihood per ogni combinazione di H_t

  # Funzione per Pr(H_t|C_t)
  get_Pr_Ht <- function(c, H_val) {
    # Se cluster trade:
    if (c <= J) {
      j_ind <- c
      k_found <- which(h_values == H_val)
      if (length(k_found)==0) return(0)
      return(Pr_Ht_given_Ct_trade[j_ind, k_found])
    } else {
      # cluster midpoint: H=0 con prob=1, altrimenti 0
      if (H_val==0) return(1) else return(0)
    }
  }

  # Ora iteriamo su tutte le possibili combinazioni di (H_t, H_{t+1}, H_{t+2}) 
  # coerenti con i cluster.
  # Questo loop è l'integrazione sulle variabili non osservate , avere avuto Q_t ci avrebbe semplificato la vita
  for (H_t in H_set_t) {
    for (H_t1 in H_set_t1) {
      for (H_t2 in H_set_t2) {

        # Calcoliamo Pr(H_t|C_t), Pr(H_{t+1}|C_{t+1}), Pr(H_{t+2}|C_{t+2})
        Pr_Ht_Ct_t  <- get_Pr_Ht(c_t, H_t)
        Pr_Ht_Ct_t1 <- get_Pr_Ht(c_t1, H_t1)
        Pr_Ht_Ct_t2 <- get_Pr_Ht(c_t2, H_t2)

        # Se una di queste è 0, combinazione non possibile
        if (Pr_Ht_Ct_t==0 || Pr_Ht_Ct_t1==0 || Pr_Ht_Ct_t2==0) next

        # Calcoliamo gli errori e_{t+1} ed e_{t+2}:
        # e_{t+1} = (P_{t+1}-P_t) - [H_{t+1}-(1-α)*H_t]
        e_t1 <- calc_e(Pt_triplet[2], Pt_triplet[1], H_t1, H_t, alpha)
        # e_{t+2} = (P_{t+2}-P_{t+1}) - [H_{t+2}-(1-α)*H_{t+1}]
        e_t2 <- calc_e(Pt_triplet[3], Pt_triplet[2], H_t2, H_t1, alpha)

        # Densità normale degli errori:
        n_e_t1 <- dnorm(e_t1, mean = e_bar, sd = sigma_e)
        n_e_t2 <- dnorm(e_t2, mean = e_bar, sd = sigma_e)

        # Se densità zero, saltare
        if (n_e_t1==0 || n_e_t2==0) next

        # Componiamo la log-likelihood della combinazione:
        # ll_combination = log(Pr(C_t)) + log(Pr(C_{t+1})) + log(Pr(C_{t+2}))
        #                 + log(Pr(H_t|C_t)) + log(Pr(H_{t+1}|C_{t+1})) + log(Pr(H_{t+2}|C_{t+2}))
        #                 + log(n_e_t1) + log(n_e_t2)
        # Questa è la decomposizione del prodotto di tutte le componenti di probabilità.
        ll_combination <- log(p_ct) + log(p_ct1) + log(p_ct2) +
                          log(Pr_Ht_Ct_t) + log(Pr_Ht_Ct_t1) + log(Pr_Ht_Ct_t2) +
                          log(n_e_t1) + log(n_e_t2)

        # Aggiungiamo questa log-likelihood parziale al vettore
        ll_values <- c(ll_values, ll_combination)
      }
    }
  }

  # Se nessuna combinazione valida, ritorna -Inf
  if (length(ll_values)==0) return(-Inf)

  # Sommiamo le likelihood di tutte le combinazioni in modo numericamente stabile:
  ll_total <- log(sum(exp(ll_values)))
  return(ll_total)
}

###############################################################################
# LOG-LIKELIHOOD TOTALE
#
# Sommiamo la log-likelihood su tutte le triplette (t, t+1, t+2) osservabili nel dataset.
# Qui "c" varia perché per ogni tripletta consideriamo Ct_triplet dal dataset.
# Il dataset assegna a ogni giorno un cluster tra 1 e 2J. Così per ogni tripletta 
# abbiamo tre cluster: C_t, C_{t+1}, C_{t+2}, letti direttamente da data$Price_Cluster.
###############################################################################

log_likelihood_total <- function(params, data) {
  ll_total <- 0
  n <- nrow(data)

  # Cicliamo sulle triplette: t va da 1 a n-2
  # Per ogni tripletta (t, t+1, t+2), estraiamo Pt_triplet e Ct_triplet 
  # (tre prezzi e tre cluster corrispondenti).
  
  for (t in 1:(n - 2)) {
    # Pt_triplet = (P_t, P_{t+1}, P_{t+2})
    Pt_triplet <- data$AMAZON.COM[t:(t+2)]
    # Ct_triplet = (C_t, C_{t+1}, C_{t+2})
    # Questi cluster li abbiamo già calcolati nella fase di preparazione, 
    # sono nella colonna data$Price_Cluster
    Ct_triplet <- data$Price_Cluster[t:(t+2)]

    # Calcoliamo la log-likelihood per questa tripletta
    ll_triplet <- log_likelihood_triplet(params, Pt_triplet, Ct_triplet)
    if (is.infinite(ll_triplet)) next
    ll_total <- ll_total + ll_triplet
  }

  # Restituiamo il negativo della log-likelihood totale 
  # perché la funzione di ottimizzazione minimizza, 
  # mentre noi vogliamo massimizzare la likelihood.
  return(-ll_total)
}

###############################################################################
# STIMA DEI PARAMETRI
#
# Usando optim cerchiamo i parametri che massimizzano la log-likelihood 
###############################################################################

initial_params <- c(
  gamma_raw = rep(0, J-1),
  mu_raw = 0,
  alpha_raw = 0,
  sigma_e_raw = log(sd(data$Delta_P, na.rm=TRUE)),
  e_bar = 0
)
}


optim_result <- optim(initial_params, log_likelihood_total, , data)
optim_result

pars <- transform_params(optim_result$par)
gamma_est <- pars$gamma
mu_est <- pars$mu
alpha_est <- pars$alpha
sigma_e_est <- pars$sigma_e
e_bar_est <- pars$e_bar

average_price <- mean(data$AMAZON.COM, na.rm=TRUE)
holden_spread <- sum(gamma_est * s_values)
holden_proxy <- holden_spread / average_price


