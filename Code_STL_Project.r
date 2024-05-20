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

#Certaines librairies sont inutiles

# Démarre le serveur graphique httpgd
#hgd()


## PARTIE 1 : Les données

## Extraction de la série sur la production de vin (de raisin) en France

data <- read.csv("/Users/tomrossa/Documents/ENSAE/Série Temporelles linéaires/Projet STL/Linear_Times_Series_Project_ENSAE_M1/Séries/valeurs_mensuelles.csv", sep=";")

data <- data[-(1:4), ] #on se retire les 4 premières lignes

values <- as.numeric(data[, 2]) #Extraction de la colonne qui nous intéresse


#On permute la série pour ailleurs avoir les dates dans l'ordre croissant

values_rev <- rev(values)

ts_data <- ts(values_rev, frequency=12, start=c(1990,1)) #Création de l'objet série temporelle


#Représentation graphique de la série brute

png("série_brute.png", width = 800, height = 500)
par(mar = c(5, 4, 4, 2) + 0.1) #Modification des marges
plot.ts(ts_data, type="l",lwd=1, col="red", xlab="Dates", ylab="Valeur de la production", main = "Série brute de la production de vin")
dev.off()


#On peut regarder l'ACF pour voir que pas de unit root à priori quoique le premier coef est assez proche de 1

png("ACF_nondiff.png", width = 800, height = 500)
acf(ts_data,lag.max=30, main = "Autocorrelation Function")
dev.off()

#On régresse la série sur t afin de voir s'il y a une tendance à titre informatif -> il faudrait vérifier l'absence d'autocorrélation entre les résidus pour que le test
#soit valide

summary(lm(ts_data ~ time(ts_data)))


# On met en place une procédure pour tester l'hypothèse de racine unitaire sur notre série avec le test ADF (avec constante et tendance au début)
#Si l'hypothèse d'autocorrélation des résidus obtenus n'est pas rejetée à 95%, on ajoute des lags à notre test
lag <- 1

# Boucle pour effectuer le test de Ljung-Box et vérifier la p-value
while (TRUE) {
  
  #Calcul des résidus du test ADF avec le nouveau lag
  adf_test <- ur.df(ts_data, type = "trend", lags = lag)
  
  summary(adf_test)
  
  #On prend les résidus
  residuals_adf <- residuals(adf_test@testreg)
  
  # Calcul de la p-value pour le lag actuel
  p_value <- Box.test(residuals_adf, lag = lag, type = "Ljung-Box")$p.value
  
  cat("Lag:", lag, "p-value:", p_value, "\n")
  
  # Vérifier si la p-value est supérieure à 0.05
  if (p_value > 0.05) {
    cat("La p-value n'est plus significative au niveau de 5% pour le lag =", lag, "\n")
    break
  }
  
  # Update du lag
  lag <- lag + 1
}


# On peut donc effectuer le test de Dickey-Fuller augmenté avec une constante non nulle et tendance
adf_test <- ur.df(ts_data, type = "trend", lags = 1)

# Afficher le résumé des résultats du test
summary(adf_test)

#l'hypothèse de racine unitaire et de tendance déterministe 
#sont rejetées à tous les niveaux de significativité. Cela va donc dans le sens de l'hypothèse de stationnarité de la série


# On effectue le test de KPSS
kpss_test <- ur.kpss(ts_data, type = "mu")

# Afficher le résumé des résultats du test
summary(kpss_test)


##Série différenciée

#création de la série différenciée qui semble plus stationnaire
ts_data1 <- diff(ts_data, differences=1)


#Représentation graphique de la série différenciée

png("Serie_diff.png", width = 1000, height = 500)
plot.ts(ts_data1,type="l",lwd=1, col="red", xlab="Time", ylab="Valeur", main = "Série de la production brute de vin différenciée")
dev.off()

#Représentation des auto-corrélogrammes et auto-corrélogrammes partiels -> hypothèse de racine unitaire beaucoup moins probable

png("pacf après différentiation.png", width=800, height = 600)
pacf(ts_data1)
dev.off()

png("acf après différentiation.png", width=800, height = 600)
acf(ts_data1)
dev.off()


#On régresse la série sur t afin de voir s'il y a une tendance à titre informatif -> il faudrait vérifier l'absence d'autocorrélation entre les résidus pour que le test
#soit valide

summary(lm(ts_data1 ~ time(ts_data1)))


# Même procédure -> test ADF en vérifiant la non-autocorrel des résidus
lag <- 1
  
#Calcul des résidus du test ADF avec le nouveau lag

adf_test <- ur.df(ts_data1, type = "drift", lags = 1)

summary(adf_test)

#On prend les résidus
residuals_adf <- residuals(adf_test@testreg)

# Boucle for sur le lag

for (lag in 1:10) {
  
  #p-value pour le lag 
  p_value <- Box.test(residuals_adf, lag = lag, type = "Ljung-Box")$p.value

  cat("Lag:", lag, "p-value:", p_value, "\n")
  
  # Vérifier si la p-value est inférieure à 0.05
  if (p_value < 0.05) {
    cat("L'absence d'autocorrélation des résidus est rejetée pour le lag =", lag, "\n")
    break
  }
}


# Calcul de la p-value pour le lag actuel
p_value <- Box.test(residuals_adf, lag = lag, type = "Ljung-Box")$p.value

cat("Lag:", lag, "p-value:", p_value, "\n")

# p-value est supérieure à 0.05 ?
if (p_value < 0.05) {
  cat("L'absence d'autocorrélation des résidus est rejetée pour le lag =", lag, "\n")
  break
}



# Test de Dickey-Fuller augmenté avec une constante non nulle et sans tendance
adf_test <- ur.df(ts_data1, type = "drift", lags = 1)

summary(adf_test)

#On conclut sur la stationnarité vraisemblable de la série différenciée

png("joint graph.png", width=1800, height = 800)
plot(cbind(ts_data,ts_data1), main = "Représentation de la série avant et après différenciation") 
dev.off()



## PARTIE II

# ACF et PACF pour trouver p* = 7 et q* = 14 sachant que d* = 1 
acf(ts_data1,lag.max=30, main = "Autocorrelation Function") #déjà enregistrés en png
pacf(ts_data,lag.max=30, main = "Partial-Autocorrelation Function") 


##1ère sélection de candidats sur les AIC et BIC car on en a beaucoup : 

AIC_values <- c()
BIC_values <- c()
arima_models <- list()

# Boucle pour ajuster les modèles ARIMA avec différentes valeurs de p et q
for (p in 1:7) {
  for (q in 1:14) {
    
    arima_model <- arima(ts_data, order = c(p, 1, q)) #Attention on fit le modèle non-différencié sur le ARIMA avec d=1
    
    # Calcul de AIC et BIC pour le modèle
    AIC_values <- c(AIC_values, AIC(arima_model))
    BIC_values <- c(BIC_values, BIC(arima_model))
    
    #On ajoute dans la liste
    model_name <- paste("ARIMA(p=", p, ", d=1, q=", q, ")", sep="")
    arima_models[[model_name]] <- arima_model
  } 
}

# On choisit les 2 modèles minimisant AIC et BIC
min_AIC_indices <- order(AIC_values)[1:2]
min_BIC_indices <- order(BIC_values)[1:2]

# on accède aux noms des modèles ARIMA minimisant AIC
min_AIC_models <- names(arima_models)[min_AIC_indices]
min_BIC_models <- names(arima_models)[min_BIC_indices]

print("Modèles ARIMA minimisant AIC :")
print(min_AIC_models)

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
  
  # on commence à p+q pour pouvoir converger vers un chi_2 adéquat
  lag <- p+q
  
  # test de Ljung-Box et vérifier la p-value
  while (lag < 50) {
    
    arima_model <- arima(y, order = c(p, d, q))
    
    residuals <- residuals(arima_model)
    
    # p-value pour le lag actuel
    p_value <- Box.test(residuals, lag = lag, type = "Ljung-Box")$p.value
    
    cat("Lag:", lag, "p-value:", p_value, "\n")
    
    # Vérifier si supérieure à 0.05
    if (p_value < 0.05) {
      cat("L'abscence d'autocorrélation des résidus est rejetée à 5% pour le lag :", lag, "\n")
      break
    }
    
    # update le lag
    lag <- lag + 1
  }
}

# Utilisation de la fonction avec des valeurs spécifiques de p, d et q
test_ljung_box(ts_data, 1, 1, 2)
test_ljung_box(ts_data, 2, 1, 1)

#On garde que le dernier benef



