# Importation des librairies nécessaires 
# library(urca)
# library(apt)
library(tseries)
# library(foreign)
# library(forecast)
library(fUnitRoots)
library(httpgd)

# Démarre le serveur graphique httpgd
hgd()

# PARTIE 1 : Les données
# Extraction de la série sur la production de vin (de raisin) en France
data <- read.csv("Séries/valeurs_mensuelles.csv")

#Transformation en objet "time series"
datats <- ts(data, frequency=12, start=c(1990,1))

#permutation de la serie, sinon elle est dans le mauvais sens
#hycarb <- rev(datats)
#hycarbts <- ts(hycarb, frequency=12, start=c(1990,1))

#Représentation graphique de la série brute
plot.ts(datats, type="l",lwd=1, col="red", xlab="Time", ylab="Indice", main = "Série brute de la production de vin")

#on prend le logarithme de la série pour écraser les pics de saisonnalité
#loghycarb <- log(hycarbts)
#loghycarbts <- ts(loghycarb, frequency = 12, start = c(1990,1))
#Représentation graphique de la série log-transformée
#plot.ts(loghycarbts, type="l",lwd=1, col="red", xlab="Time", ylab="Indice")

#désaisonnalisation de la série loghycarbts
#df_desais <- decompose(loghycarbts)
#plot(df_desais)
#hycarb_desais <- loghycarbts - df_desais$seasonal

#Représentation graphique de la série désaisonnalisée
#plot.ts(hycarb_desais,type="l",lwd=1, col="red", xlab="Time", ylab="Indice")

#Test ADF sur la série désaisonnalisée
#urdfTest(hycarb_desais)

#création de la série différenciée
#hycarbdiff1 <- diff(hycarb_desais, differences=1)
#Représentation graphique de la série différenciée
#plot.ts(hycarbdiff1,type="l",lwd=1, col="red", xlab="Time", ylab="Indice")

#Test ADF pour déterminer la stationnarité de la série différenciée
#urdfTest(hycarbdiff1)
#on obtient bien une stationnarité de la série différenciée (seuil de 1%)




