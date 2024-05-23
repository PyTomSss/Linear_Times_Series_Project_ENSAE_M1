# Importation des librairies nécessaires 
chooseCRANmirror()
install.packages("gss")
install.packages("tseries")
install.packages("urca")
install.packages("fUnitRoots")
install.packages("httpgd")

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
library(httpgd)

#Certaines librairies sont inutiles

# Démarre le serveur graphique httpgd
#hgd()


## PARTIE 1 : Les données

## Extraction de la série sur la production de vin (de raisin) en France

data <- read.csv("/Users/eva-andreetiomo/Downloads/valeurs_mensuelles.csv", sep=";")

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


#On peut regarder l'ACF pour voir que pas de unit root à priori quoique le premier coefficient est assez proche de 1

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
#sont rejetées à tous les niveaux de significativité. Cela va donc dans le sens de l'hypothèse de stationnarité de la série, hypothèse qui nous semble erronnée.


# On effectue le test de KPSS pour réfuter ou affirmer la stationnarité de la série brute
kpss_test <- ur.kpss(ts_data, type = "mu")

# Afficher le résumé des résultats du test
summary(kpss_test)

# On obtient que la série n'est pas stationnaire, on procède à de la différenciation pour la rendre stationnaire :

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

# Nous avons obtenu, une série stationnaire. Nous passons désormais à la partie 2 qui a pour but de trouver les bons modèles ARMA pour la série


## PARTIE II

# En observant les ACF et PACF trouvés précédemment sur notre série différenciée, on suppose que p*=7 (car après le lag 7 le PACF prend des valeurs non significatives) et que q*=14 (car après le lag 15 l'ACF prend des valeurs non significatives). De plus, on a différencié une fois la série pour la rendre stationnaire donc on en déduit que d*=1
# Ainsi, on effectue des ACF et PACF pour trouver p* = 7 et q* = 14 sachant que d* = 1 
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

#fonction de test des significativités individuelles des coefficients
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

signif(ARMA_5_3) #test de significativité des coefficients
signif(ARMA_6_3)
signif(ARMA_1_2) 
signif(ARMA_2_1)

#Ces deux tests nous permettent de retenir les deux derniers ARMA. En effet les coefficients des retards les plus élevés AR(5) et AR(6) ne rejettent chacun par leur nullité à 95% (p-value > 0.05). Les modèles ARIMA (5,1,3) et ARIMA (6,1,3) sont donc mal ajustés.


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

# On remarque que pour l'ARIMA (1,1,2), l'absence d'autocorrélation n'est jamais rejetée à 95% sur les 49 premiers lags. On en déduit que ce modèle n'est pas valide.
# Ainsi, la sortie précédente élimine l'ARIMA (1,1,2). 
# L'absence d'autocorrélation n'est jamais rejetée à 95% jusqu'à 38 retards pour l'ARIMA (2,1,1). Ce modèle est donc satisfaisant et valide.
# Nous concluons la partie 2 en ayant le modèle ARIMA (2,1,1) choisi pour notre série traitant la production de vin. 

# Nous pouvons désormais passer à la phase prévision : PARTIE III

# QUESTION 8

#Travail sur les hypothèses nécessaires pour les intervalles de confiance : pour voir si le modèle est causal, on regarde les racines de notre polynôme et on voit si elles sont en dehors du cercle unité

ARMA_2_1$coef
ARMA_2_1$sigma
phi_1 <- as.numeric(ARMA_2_1$coef[1])
phi_2 <- as.numeric(ARMA_2_1$coef[2])
psi_1 <- as.numeric(ARMA_2_1$coef[3])
sigma2 <- as.numeric(ARMA_2_1$sigma)

#On affiche également les coefficiens des polynômes pour s'assurer qu'on a effectué les bonnes commandes
phi_1
phi_2
psi_1
sigma2

#On calcule désormais les racines de nos polynomes MA et AR et on vérifie qu'elles sont hors du cercle unité
ar_coefs <- c(phi_1, phi_2)
ma_coefs <- c(psi_1)

ar_roots <- polyroot(c(1, -ar_coefs))
ma_roots <- polyroot(c(1, ma_coefs))

abs(ar_roots)
abs(ma_roots)
all(abs(ar_roots) > 1) 

all(abs(ma_roots) > 1)

# On effectue un code pour obtenir un tableau récapitulatif des valeurs des coefficients des polynômes
# Charger les packages nécessaires
library(knitr)
library(kableExtra)

# Création du tableau des coefficients
tableau_coefficients <- data.frame(
  Coefficients = c("$\\varphi_1$", "$\\varphi_2$", "$\\psi_1$"),
  Valeurs = c(phi_1, phi_2, psi_1)
)
# Affichage du tableau en utilisant kable
kable(tableau_coefficients, caption = "Coefficients du modèle ARMA(2,1)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

install.packages("webshot")
install.packages("htmltools")
install_phantomjs()
library(webshot)
library(htmltools)

# Enregistrement du tableau en HTML
html_file <- "tableau_coefficients.html"
save_kable(kable(tableau_coefficients, caption = "Coefficients du modèle ARMA(2,1)") %>%
             kable_styling(bootstrap_options = c("striped", "hover", "condensed")), html_file)
# Capture de l'HTML en image
webshot(html_file, "tableau_coefficients.png")

# On récupère notre image pour l'incorporer dans notre rapport overleaf
install.packages("magick")
library(magick)
image_path <- "tableau_coefficients.png"
img <- image_read(image_path)
print(img)

# QUESTION 9 

# On cherche à obtenir l'intervalle de confiance sur une période plus restreinte
## Extraction de la série sur la production de vin (de raisin) en France

install.packages("dplyr")
library(dplyr)

head(data)

# on retire les lignes 64 à 411 pour avoir un dataframe qui commence en 2019
datavf1 <- data %>% slice(-(64:413))
print(datavf1)

values1 <- as.numeric(datavf[, 2]) #Extraction de la colonne qui nous intéresse


#On permute la série pour ailleurs avoir les dates dans l'ordre croissant

values_rev1 <- rev(values1)


ts_datavf <- ts(values_rev1, frequency=12, start=c(2019,01)) #Création de l'objet série temporelle


# Enregistrement du graphique en tant que fichier PNG : on a un zoom sur la production de vin de janvier 2019 à février 2024
png("série_raccourcie.png", width = 800, height = 500)
par(mar = c(5, 4, 4, 2) + 0.1) # Modification des marges
plot.ts(ts_datavf, type="l", lwd=1, col="green", xlab="Dates", ylab="Valeur de la production", main = "Série brute de la production de vin")
dev.off()

# Affichage du graphique dans RStudio
par(mar = c(5, 4, 4, 2) + 0.1) # Modification des marges
plot.ts(ts_datavf, type="l", lwd=1, col="green", xlab="Dates", ylab="Valeur de la production", main = "Série raccourcie de la production de vin")

# On retire les lignes 64 à 411 pour avoir un dataframe qui commence en 2019 et qui finit en 2023 afin de repérer si nos prédictions pour début 2024 peuvent être considérées comme pertinentes
datavf <- data %>% slice(-(c(1:2, 64:413)))
print(datavf)

values2 <- as.numeric(datavf[, 2]) #Extraction de la colonne qui nous intéresse


#On permute la série pour ailleurs avoir les dates dans l'ordre croissant

values_rev2 <- rev(values2)


ts_datavf <- ts(values_rev2, frequency=12, start=c(2019,01)) #Création de l'objet série temporelle


# Enregistrement du graphique en tant que fichier PNG
png("série_raccourcie.png", width = 800, height = 500)
par(mar = c(5, 4, 4, 2) + 0.1) # Modification des marges
plot.ts(ts_datavf, type="l", lwd=1, col="red", xlab="Dates", ylab="Valeur de la production", main = "Série brute de la production de vin 2019-2023")
dev.off()

# Normalement la fonction forecast permet d'obtenir directement l'intervalle de confiance à 95% mais on prend nos précautions et on précise le niveau
# Je fais désormais les prédictions à horizon 1 : 
# Ajustement du modèle ARIMA(2,1,1)
model_arima <- arima(ts_datavf, order = c(2, 1, 1))

# Afficher les résultats de l'ajustement du modèle
summary(model_arima)

# Prédiction à 1 mois avec intervalle de confiance
library(forecast)
library(zoo)
forecast_arima <- forecast(model_arima, h = 1, level=95)

# Afficher les prédictions
print(forecast_arima)

# Extraction des prédictions et des intervalles de confiance
preds <- zoo(matrix(NA, ncol = 1, nrow = 1), order.by = time(forecast_arima$mean))
preds[,1] <- forecast_arima$mean

# On affiche les prédictions et les intervalles de confiance graphiquement
plot(forecast_arima, main = "Prédiction ARIMA(2,1,1) à 1 mois", xlab = "Temps", ylab = "Valeur")

# On ajoute les observations historiques pour le contexte
lines(ts_datavf, col = "blue")

# On fait désormais les prédictions à horizon 2 : 
forecast_arima <- forecast(model_arima, h = 2, level=95)

# Afficher les prédictions
print(forecast_arima)

# Extraction des prédictions et des intervalles de confiance
preds <- zoo(matrix(NA, ncol = 1, nrow = 1), order.by = time(forecast_arima$mean))
preds[,1] <- forecast_arima$mean

# Afficher les prédictions et les intervalles de confiance graphiquement
plot(forecast_arima, main = "Prédiction ARIMA(2,1,1) à 1 et 2 mois", xlab = "Temps", ylab = "Valeur")

# Ajouter les observations historiques pour le contexte
lines(ts_datavf, col = "green")