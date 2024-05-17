# Importation des librairies nécessaires 
chooseCRANmirror()
install.packages("gss")
install.packages("tseries")
install.packages("urca")
install.packages("fUnitRoots")

require(forecast)
require(zoo)
require(tseries)
require(fUnitRoots)
require(ggplot2)
library(urca) 
library(tseries)
library(astsa)
library(stats)
library(lmtest)
library(forecast)

# Démarre le serveur graphique httpgd
hgd()


## PARTIE 1 : Les données

## Extraction de la série sur la production de vin (de raisin) en France
data <- read.csv("/Users/tomrossa/Documents/ENSAE/Série Temporelles linéaires/Projet STL/Linear_Times_Series_Project_ENSAE_M1/Séries/valeurs_mensuelles.csv", sep=";")
data <- data[-(1:4), ] #on se retire les 4 premières lignes


dates <- seq(as.Date("1990-01-01"), as.Date("2024-02-29"), by = "month") #pas forcément utile
values <- as.numeric(data[, 2]) #Extraction de la colonne qui nous intéresse


# Créer un data frame avec les dates et les valeurs -> on s'en fout du dataframe du coup
time_series_data <- data.frame(Date = dates, Value = values)


#On permute la série pour ailleurs avoir les dates dans l'ordre croissant

values_rev <- rev(values)
ts_data <- ts(values_rev, frequency=12, start=c(1990,1)) #Création de l'objet série temporelle


#Représentation graphique de la série brute
par(mar = c(2, 2, 2, 2) + 0.1) #Modification des marges
plot.ts(ts_data, type="l",lwd=1, col="red", xlab="Dates", ylab="Valeur de la production", main = "Série brute de la production de vin")

#On régresse la série sur t afin de voir s'il y a une tendance -> pas l'air au vu des coefficients (bien que l'on n'ait pas montré la non autocorrélation des résidus)
summary(lm(ts_data ~ time(ts_data)))

# Initialisation du lag
lag <- 1

# Boucle pour effectuer le test de Ljung-Box et vérifier la p-value
while (TRUE) {
  #Calcul des résidus du test ADF avec le nouveau lag
  # Effectuer le test de Dickey-Fuller augmenté avec une constante non nulle et sans tendance
  adf_test <- ur.df(ts_data, type = "trend", lags = lag)
  
  # Afficher le résumé des résultats du test
  summary(adf_test)
  
  #On prend les résidus
  residuals_adf <- residuals(adf_test@testreg)
  
  # Calcul de la p-value pour le lag actuel
  p_value <- Box.test(residuals_adf, lag = lag, type = "Ljung-Box")$p.value
  
  # Afficher le lag et la p-value
  cat("Lag:", lag, "p-value:", p_value, "\n")
  
  # Vérifier si la p-value est supérieure à 0.05
  if (p_value > 0.05) {
    cat("La p-value n'est plus significative au niveau de 5% pour le lag =", lag, "\n")
    break
  }
  
  # Incrémenter le lag
  lag <- lag + 1
}

# Effectuer le test de Dickey-Fuller augmenté avec une constante non nulle et sans tendance
adf_test <- ur.df(ts_data, type = "trend", lags = 1)

# Afficher le résumé des résultats du test
summary(adf_test)


# Effectuer le test de Dickey-Fuller augmenté avec une constante non nulle et sans tendance
kpss_test <- ur.kpss(ts_data, type = "mu")

# Afficher le résumé des résultats du test
summary(kpss_test)

#On peut regarder l'ACF pour voir que pas de unit root à priori quoique le premier coef est assez proche de 1
par(mar = c(2, 4, 4, 2) + 0.1)
#Le nombre de lags est peut-être trop élevé et c'est bizarrement affiché en abscisses sur le graphique
acf(ts_data,lag.max=30, main = "Autocorrelation Function")


##Série différenciée

#création de la série différenciée qui semble plus stationnaire
ts_data1 <- diff(ts_data, differences=1)
#Représentation graphique de la série différenciée
plot.ts(ts_data1,type="l",lwd=1, col="red", xlab="Time", ylab="Indice")

acf(ts_data1,lag.max=150) #Hypothèse de racine unitaire beaucoup moins plausible au vu de l'ordre 1

# Initialisation du lag
lag <- 1

# Boucle pour effectuer le test de Ljung-Box et vérifier la p-value
while (TRUE) {
  #Calcul des résidus du test ADF avec le nouveau lag
  # Effectuer le test de Dickey-Fuller augmenté avec une constante non nulle et sans tendance
  adf_test <- ur.df(ts_data1, type = "drift", lags = lag)
  
  # Afficher le résumé des résultats du test
  summary(adf_test)
  
  #On prend les résidus
  residuals_adf <- residuals(adf_test@testreg)
  
  # Calcul de la p-value pour le lag actuel
  p_value <- Box.test(residuals_adf, lag = lag, type = "Ljung-Box")$p.value
  
  # Afficher le lag et la p-value
  cat("Lag:", lag, "p-value:", p_value, "\n")
  
  # Vérifier si la p-value est supérieure à 0.05
  if (p_value > 0.05) {
    cat("La p-value n'est plus significative au niveau de 5% pour le lag =", lag, "\n")
    break
  }
  
  # Incrémenter le lag
  lag <- lag + 1
}

# Effectuer le test de Dickey-Fuller augmenté avec une constante non nulle et sans tendance
adf_test <- ur.df(ts_data1, type = "drift", lags = 1)

# Afficher le résumé des résultats du test
summary(adf_test)

#On conclut sur la stationnarité vraisemblable de la série différenciée

## PARTIE II

# ACF et PACF pour trouver p* = 7 et q* = 14 sachant que d* = 1
acf(ts_data1,lag.max=30, main = "Autocorrelation Function") 
pacf(ts_data,lag.max=30, main = "Partial-Autocorrelation Function") 

##1ère sélection de candidats sur les AIC et BIC car on en a beaucoup : 

# Initialiser les vecteurs pour stocker les valeurs de AIC et BIC
AIC_values <- c()
BIC_values <- c()

# Initialiser une liste pour stocker les modèles ARIMA
arima_models <- list()

# Boucle pour ajuster les modèles ARIMA avec différentes valeurs de p et q
for (p in 1:7) {
  for (q in 1:14) {
    # Ajuster le modèle ARIMA
    arima_model <- arima(ts_data, order = c(p, 1, q)) #Attention on fit le modèle non-différencié sur le ARIMA avec d=1
    
    # Calculer les valeurs de AIC et BIC
    AIC_values <- c(AIC_values, AIC(arima_model))
    BIC_values <- c(BIC_values, BIC(arima_model))
    
    # Stocker le modèle ARIMA dans la liste
    model_name <- paste("ARIMA(p=", p, ", d=1, q=", q, ")", sep="")
    arima_models[[model_name]] <- arima_model
  } 
}

# Trouver les trois modèles minimisant AIC et BIC
min_AIC_indices <- order(AIC_values)[1:2]
min_BIC_indices <- order(BIC_values)[1:2]

# Accéder aux noms des modèles ARIMA minimisant AIC
min_AIC_models <- names(arima_models)[min_AIC_indices]
min_BIC_models <- names(arima_models)[min_BIC_indices]

# Afficher les noms des modèles ARIMA minimisant AIC
print("Modèles ARIMA minimisant AIC :")
print(min_AIC_models)

# Afficher les noms des modèles ARIMA minimisant BIC
print("Modèles ARIMA minimisant BIC :")
print(min_BIC_models)


## On a 4 candidates et on va vérifier que leurs résidus ne sont pas autocorrélés et que leurs coefficients max sont significativement non nuls

#fonction de test des significativite ́s individuelles des coefficients
signif <- function(estim){
  coef <- estim$coef
  se <- sqrt(diag(estim$var.coef))
  t <- coef/se
  pval <- (1-pnorm(abs(t)))*2
  return(rbind(coef,se,pval))
}

ARMA_5_3 <- arima(ts_data, order = c(5, 1, 3))
ARMA_6_3 <- arima(ts_data, order = c(6, 1, 3))
ARMA_1_2 <- arima(ts_data, order = c(1, 1, 2))
ARMA_2_1 <- arima(ts_data, order = c(2, 1, 1))

signif(ARMA_5_3) #test de significativité des coeffs
signif(ARMA_6_3)
signif(ARMA_1_2) 
signif(ARMA_2_1)

#Je crois qu'on garde juste les deux derniers

#Test des autocorrel pour 50 lags max
test_ljung_box <- function(y, p, d, q) {
  # Initialiser le lag
  lag <- p+q
  
  # Boucle pour effectuer le test de Ljung-Box et vérifier la p-value
  while (lag < 50) {
    # Effectuer le modèle ARIMA avec les paramètres spécifiés
    arima_model <- arima(y, order = c(p, d, q))
    
    # Extraire les résidus
    residuals <- residuals(arima_model)
    
    # Calculer la p-value pour le lag actuel
    p_value <- Box.test(residuals, lag = lag, type = "Ljung-Box")$p.value
    
    # Afficher le lag et la p-value
    cat("Lag:", lag, "p-value:", p_value, "\n")
    
    # Vérifier si la p-value est supérieure à 0.05
    if (p_value < 0.05) {
      cat("L'abscence d'autocorrélation des résidus est rejetée à 5% pour le lag :", lag, "\n")
      break
    }
    
    # Incrémenter le lag
    lag <- lag + 1
  }
}

# Utilisation de la fonction avec des valeurs spécifiques de p, d et q
test_ljung_box(ts_data, 1, 1, 2)
test_ljung_box(ts_data, 2, 1, 1)

#On garde que le dernier super



