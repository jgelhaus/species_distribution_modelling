#----------------------------------------------------------
# Abschlussaufgabe Johanna Gelhaus
# Oekologische Modellierung - Habitatmodelle SoSe 2023
# Verbreitungsmodellierung der Goldammer in der Schweiz
#----------------------------------------------------------

#-----------------------------------------------------------
# Daten einlesen und einen ersten Ueberblick verschaffen:
#---------------------------------------------------------

rm(list=ls())
setwd("C:/Users/User/Documents/1.1_Uni_Master/Verbreitungsmodelle/Abschlussaufgabe_1")
data_goldammer <- read.csv("ch_goldammer.csv", header=TRUE)
head(data_goldammer)
names(data_goldammer)
summary(data_goldammer)
str(data_goldammer)

# Grafische Betrachtung: Scatterplotmatrix
# pairs(data_goldammer) --> figure margins too large


# Pakete laden
library(ellipse)
library(car)
library(PresenceAbsence)
library(corrplot)


#----------------------------------------------------------------------------
# Modellauswahl und Variablenselektion (Modelltyp + Prädiktorvariablen-Set):
#---------------------------------------------------------------------------

# Zunächst Verschaffen eines Ueberblicks der Korrelationen mit dem Spearmanplot

correlation <- cor(data_goldammer, method="spearman", use="complete.obs")
print(correlation)
colors <- c("#A50F15","#DE2D26","#FB6A4A","#FCAE91","#FEE5D9","white",
            "#EFF3FF","#BDD7E7","#6BAED6","#3182BD","#08519C") 
plotcorr(correlation, col=colors[5*correlation + 6], cex.lab=0.35)


# Hervorheben hoher Korrelationen (Spearman > 0.7)

plotcorr(correlation, col=ifelse(abs(correlation) > 0.7,"#A50F15", "peachpuff"), cex.lab=0.4)

# Keine besonders hohe Korrelation mit Vorkommen der Goldammer, sondern nur Prädiktorvariablen
# untereinander. (--> Niederschlaege fuer unterschiedliche Zeitraeume untereinander; 
# Alpweiden mit Temperatur, Hoehe; Hangneigung mit Wetnessindex; Hoehe mit Temp. Juli + Temp. Jahr)

# Daher: Sortiertes Auflisten der Korrelationen, um höchste Korrelation
# mit dem Vorkommen der Goldammer zu extrahieren:

correlations <- cor(data_goldammer[, 2], data_goldammer[, -2], method = "spearman", use = "complete.obs")

# Rangordnung der Korrelationen
sorted_correlations <- sort(abs(correlations), decreasing = TRUE)
sorted_indices <- order(abs(correlations), decreasing = TRUE)

# Rangordnung der Korrelationen ausgeben:
for (i in 1:length(sorted_indices)) {
  predictor <- names(data_goldammer)[-2][sorted_indices[i]]
  correlation <- correlations[sorted_indices[i]]
  cat(i, "\t", predictor, "\t", correlation, "\n")
}

# Höchste Korrelationen mit:
#1 	 Acker 	 0.5220136 
#2 	 Niederschlag.Jul 	 -0.4512081 
#3 	 Niederschlag.Jahr 	 -0.402372 
#4 	 Hoehe 	 -0.4006369 
#5 	 Vegetationslos 	 -0.3986145 
#6 	 Temperatur.Jul 	 0.3960407 
#7 	 Temperatur.Jahr 	 0.3881953 

#------------------------------------------------------------------
# Gibt es Kollinearitäten zwischen den Prädiktorvariablen, die in 
# das Modell aufgenommen werden sollen?
#-----------------------------------------------------------------

# Niederschlagswerte und Temperaturen korrelieren untereinander, daher jeweils nur einen der 
# Werte aufnehmen.
# Testen Korrelation zwischen Acker und vegetationslos
cor(data_goldammer$Acker, data_goldammer$Vegetationslos, method="spearman")
# -0.6089042 --> können beide genutzt werden

# Hoehe und Temperatur im Juli zeigten hohe Correlationen im Spearmanplot.
cor(data_goldammer$Hoehe, data_goldammer$Temperatur.Jul, method="spearman")
# -0.9475654 --> nur einen der beiden im Modell nutzen


#---------------------------------------------------------
#GLM bilden zur weiteren Auswahl an Praediktorvariablen
#-------------------------------------------------------


glm.all <- glm(Goldammer~Acker+Niederschlag.Jul+ Hoehe+Vegetationslos+Temperatur.Jul,family="binomial", data=data_goldammer)
summary(glm.all) #AIC: 2492.7

model.stepwise.bw <- step (glm.all, direction = "backward") # Schrittweises Vereinfachen des Modells
# Weglassen der Hoehe verringert den AIC

# geringster AIC mit diesen 4 Prädiktorvariablen
glm.all2 <- glm(Goldammer~Acker+Niederschlag.Jul+Vegetationslos+Temperatur.Jul,family="binomial", data=data_goldammer)
summary(glm.all2) #AIC: 2490.8

model.stepwise.bw <- step (glm.all2, direction = "backward")
# Keine weitere Vereinfachung

# Geringster AIC bei nur 3 Praediktoren bei folgender Auswahl: Acker, Niederschlag.Jul, Vegetationslos

#Endergebnis des Modells lautet also: 
glm.end <- glm(Goldammer~Acker+Niederschlag.Jul+Vegetationslos,family="binomial", data=data_goldammer)
summary(glm.end) #AIC: 2495.3


# --------------------------------------------------------- 
#Vorhersagen der Vorkommenswahrscheinlichkeiten 
#---------------------------------------------------------- 

pred.training.data <- predict(glm.end,data_goldammer, type="response") # type= response -->
# direkte Umrechnung in Vorkommenswahrscheinlichkeiten statt Ausgabe von logit(y).

hist(pred.training.data)

#--------------------------------------------------------------------------
# Unimodale Responsekurven für alle Prädiktorvariablen und deren Prüfung auf 
# ökologische Plausibilität.
#-----------------------------------------------------------------------------

par(mfrow=c(1,3))

#Acker
newdat <- expand.grid(Acker=seq(min(data_goldammer$Acker),max(data_goldammer$Acker), length.out=100), 
                      Niederschlag.Jul=mean(data_goldammer$Niederschlag.Jul), Vegetationslos=mean(data_goldammer$Vegetationslos))
prednew <- predict(glm.end,newdat,type="response")
plot(prednew~newdat$Acker,type="l", ylim=c(0,1), las=1,
     ylab="W'keit Vorkommen Goldammer", xlab="Acker", main="Responsekurve Acker")

#Niederschlag Juli
newdat1 <- expand.grid(Acker=mean(data_goldammer$Acker), 
                       Niederschlag.Jul=seq(min(data_goldammer$Niederschlag.Jul),max(data_goldammer$Niederschlag.Jul), length.out=100), Vegetationslos=mean(data_goldammer$Vegetationslos))
prednew1 <- predict(glm.end,newdat1,type="response")
plot(prednew1~newdat1$Niederschlag.Jul,type="l", ylim=c(0,1), las=1,
     ylab="W'keit Vorkommen Goldammer", xlab="Niederschlag Juli", main="Responsekurve Niederschlag Juli")

# Vegetationslosigkeit
newdat2 <- expand.grid(Acker=mean(data_goldammer$Acker), 
                       Niederschlag.Jul=mean(data_goldammer$Niederschlag.Jul), Vegetationslos=seq(min(data_goldammer$Vegetationslos),max(data_goldammer$Vegetationslos), length.out=100))
prednew2 <- predict(glm.end,newdat2,type="response")
plot(prednew2~newdat2$Vegetationslos,type="l", ylim=c(0,1), las=1,
     ylab="W'keit Vorkommen Goldammer", xlab="Vegetationslosigkeit", main="Responsekurve Vegetationslosigkeit")

# Oekologische Plausibilitaet?

# Acker:
# Goldammern suchen im Winter oft in groeßeren Trupps haeufig auf Aeckern oder in Gaerten
# Nahrung. Außerdem bewohnt die Goldammer in der Regel offene und halboffene
# Lebensraeume wie z.B. Feldraender. Profitiert v. A. von strukturreicher, traditioneller
# Landwirtschaft --> bewohnt Rand- und Saumbiotope (Quelle: NABU)

# Niederschlag Juli:
# Die Goldammer scheint Gebiete mit zu viel Niederschlag im Juli zu meiden.
# Im Sommer gibt es häufig starke Niederschlagsereignisse. Vielleicht
# könnten diese die Verbreitung, den Nestbau oder die Partnersuche negativ beeibflussen.
# Es koennte aber vielleicht auch einen versteckten Zusammenhang mit der Hoehe z. B. geben
# die aufgrund von Kollinearitaeten nicht in das Modell mit aufgenommen wurde.

# Vegetationslosigkeit:
# Bei erhöhter Vegetationslosigkeit könnten Strukturen fehelen, die als Zufluchtsort und zum
# Bau von Nestern relevant sind.

#------------------------------------------------------------------------------
#Karten relevanter Praediktoren mit dem Vorkommen der Goldammer.
#------------------------------------------------------------------------------
library(viridis)

par(mfrow=c(1,1))
#Acker

par(mar = c(5, 4, 4, 4) + 0.1)
plot(data_goldammer$x, data_goldammer$y,
     xlab = "X-Koordinaten", ylab = "Y-Koordinaten", col = "gray", pch = 19)

acker0 = data_goldammer$Acker == 0
acker25 = (data_goldammer$Acker > 0 & data_goldammer$Acker <= 25)
acker50 = data_goldammer$Acker > 25 & data_goldammer$Acker <= 50
acker75 = data_goldammer$Acker > 50 & data_goldammer$Acker <= 75
acker100 = data_goldammer$Acker > 75 & data_goldammer$Acker <= 100

points(data_goldammer[acker0, 3], data_goldammer[acker0, 4], col = viridis(6)[1], pch = 19)
points(data_goldammer[acker25, 3], data_goldammer[acker25, 4], col = viridis(6)[2], pch = 19)
points(data_goldammer[acker50, 3], data_goldammer[acker50, 4], col = viridis(6)[3], pch = 19)
points(data_goldammer[acker75, 3], data_goldammer[acker75, 4], col = viridis(6)[4], pch = 19)
points(data_goldammer[acker100, 3], data_goldammer[acker100, 4], col = viridis(6)[5], pch = 19)

points(data_goldammer[data_goldammer$Goldammer == 1, 3], data_goldammer[data_goldammer$Goldammer == 1, 4], col = viridis(6)[6], pch = 10, cex = 0.6)

legend("topleft", legend = c("0%", "> 0 bis 25%", "25 bis 50%", "50 bis 75%", "75 bis 100%", "Präs. Goldammer"),
       pch = c(16, 16, 16, 16, 16, 10),
       col = viridis(6),
       cex = 0.6, bty = "n", y.intersp = 0.8, x.intersp = 1)
title("Anteil Ackerfläche und Präsenz der Goldammer")
par(mar = c(5, 4, 4, 2) + 0.1)
# Die Goldammer findet sich besonders in Gebieten hohen Anteils von Ackerflaechen.


#Niederschlag im Juli
summary(data_goldammer$Niederschlag.Jul) # Maximum liegt bei 252.1
# Zur Visualisierung wird der Niederschlag in 3 Klassen aufgeteilt:

plot(data_goldammer$x, data_goldammer$y, xlab="x-Koordinate", ylab = "y-Koordinate", col= "gray", pch=19)
nieders90 <- data_goldammer$Niederschlag.Jul>=0 & data_goldammer$Niederschlag.Jul <= 90
nieders180 <- data_goldammer$Niederschlag.Jul>90 & data_goldammer$Niederschlag.Jul <= 180
nieders270 <- data_goldammer$Niederschlag.Jul>180

points(data_goldammer[nieders90,3], data_goldammer[nieders90,4], col="#FFA500", pch=16, cex=1)
points(data_goldammer[nieders180,3], data_goldammer[nieders180,4], col="#7CFC00", pch=16, cex=1)
points(data_goldammer[nieders270,3], data_goldammer[nieders270,4], col="#3BB9B9", pch=16, cex=1)
points(data_goldammer[data_goldammer$Goldammer==1,3], data_goldammer[data_goldammer$Goldammer==1,4], col='purple', pch=10, cex=0.6)
legend("topleft",  c("0-90 mm","90-180 mm", ">180 mm", "Praesenz Goldammer"), pch=16, col= c("#FFA500","#7CFC00", "#3BB9B9","purple"), cex = 0.8, bty = "n",y.intersp = 0.8, x.intersp = 1)
title("Niederschlagssumme im Juli und Präsenz der Goldammer")
# Die Goldammer findet sich selten dort, wo ganz hohe Niederschlaege gemessen wurden.


#Vegetationslosigkeit
summary(data_goldammer$Vegetationslos) # max: 80; mean 2.349
plot(data_goldammer$x, data_goldammer$y, 
     xlab = "x-Koordinate", ylab = "y-Koordinate", 
     main = "Anteil vegetationsloser Fläche und Präsenz der Goldammer", col = "gray", pch = 19)

veg2 <- data_goldammer$Vegetationslos <= 2
veg4 <- data_goldammer$Vegetationslos > 2 & data_goldammer$Vegetationslos <= 4
veg20 <- data_goldammer$Vegetationslos > 4 & data_goldammer$Vegetationslos <= 20
veg100 <- data_goldammer$Vegetationslos > 20

points(data_goldammer[veg2, 3], data_goldammer[veg2, 4], col = viridis(5)[1], pch = 16, cex = 1)
points(data_goldammer[veg4, 3], data_goldammer[veg4, 4], col = viridis(5)[2], pch = 16, cex = 1)
points(data_goldammer[veg20, 3], data_goldammer[veg20, 4], col = viridis(5)[3], pch = 16, cex = 1)
points(data_goldammer[veg100, 3], data_goldammer[veg100, 4], col = viridis(5)[4], pch = 16, cex = 1)
points(data_goldammer[data_goldammer$Goldammer == 1, 3], data_goldammer[data_goldammer$Goldammer == 1, 4], col = viridis(5)[5], pch = 10, cex = 0.45)

legend("topleft", c("0 bis 2 %", "> 2 bis 4 %", "> 4 bis 20 %", "> 60 %", "Praesenz Goldammer"), 
       pch = 16, col = viridis(5), cex = 0.8, bty = "n", y.intersp = 0.8, x.intersp = 0.5)
# Generell gibt es einen geringen Anteil vegetationsloser Fläche (mean = 2.349).
# Die Orte, an denen es weniger Vegetation gibt, zeigen ein selteneres Vorkommen der Goldammer.


#------------------------------------------------------------------
#Praesenz-Absenz-Karte der Goldammer ohne Prädiktoren.
#--------------------------------------------------------------------
par(mfrow=c(1,1))

plot(data_goldammer$x, data_goldammer$y, 
     xlab="X-Koordinaten", ylab="Y-Koordinaten", 
     main="Vorkommen der Goldammer in der Schweiz", col="gray",pch=19, cex=1)
pres.goldammer <- data_goldammer$Goldammer==1
points(data_goldammer[pres.goldammer,3], data_goldammer[pres.goldammer,4], col="purple", pch=19)
legend("topleft", c("Praesenz Goldammer", "Absenz Goldammer"), pch=16, col= c("purple", "grey"), cex = 0.8, bty = "n", y.intersp = 0.8, x.intersp = 0.5)

# Die Goldammer kommt tendentiell eher im Norwestlichen Teil der Schweiz vor.
# Seltener ist sie in den Bereichen der Alpen zu finden.

#----------------------------------------------------------------------
# Karte der vorhergesagten Vorkommenswahrscheinlichkeiten aus dem GLM. 
#-------------------------------------------------------------------

par(mfrow=c(1,1))
pred.training.data <- predict(glm.end,data_goldammer, type="response")

#Karte: Praesenz-Absenz-Vorhersage
plot(data_goldammer$x, data_goldammer$y,xlab="x-Koordinate", ylab = "y-Koordinate",main="Vorhergesagtes Vorkommen", col= "gray", pch=19)
data_goldammer.pred <- cbind(pred.training.data, data_goldammer)
pres.predicted <- data_goldammer.pred$pred > 0.5 
points(data_goldammer.pred[pres.predicted,4], data_goldammer.pred[pres.predicted,5], col="coral", pch=19)
legend("topleft", c("Praesenz Goldammer", "Absenz Goldammer"), pch=16, col= c("coral", "grey"), cex = 0.8, bty = "n", y.intersp = 0.8, x.intersp = 0.5)


#Vergleich Vorkommensvorhersage mit der Vorkommensdatenlage:

par(mar=c(5,6,4,8),xpd=TRUE)
plot(data_goldammer$x, data_goldammer$y,xlab="x-Koordinate", ylab = "y-Koordinate",main="Vergeleich Praesenz und Vorhersages Vorkommen", col= "gray", pch=19)
points(data_goldammer.pred[pres.predicted,4], data_goldammer.pred[pres.predicted,5], col="yellow2", pch=19, cex=1)
pres.goldammer <- data_goldammer$Goldammer==1
points(data_goldammer[pres.goldammer,3], data_goldammer[pres.goldammer,4], col="purple", pch=16, cex=0.6)
legend("topleft",  c("Praesenz Vorhersage", "Praesenz Daten"), pch=16, col= c("yellow2", "purple"), cex = 0.8, bty = "n", y.intersp = 0.5, x.intersp = 0.5)

# Die Daten stimmen im Nordwestlichen Teil des Landes relativ gut ueberein. In ein paar geclusterten
# Bereichen finden sich Stellen, in denen die Praesenz vorhergesagt wird, jedoch keine Goldammer vorkam.
# In dem Bereich der Alpen im suedoestlichen Teil des Landes gibt es hingegen einige Punkte, bei denen 
# das Vorkommen nicht prognostiziert wurde, die Goldammer jedoch vorkam.

#--------------------------------------------------------
#Unsicherheiten der Modellschaetzer
#--------------------------------------------------------

summary(glm.end)
# Coefficients:
#                     Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)       2.486090   0.213024  11.670  < 2e-16 ***
#  Acker             0.034979   0.002223  15.733  < 2e-16 ***
#  Niederschlag.Jul -0.028534   0.001623 -17.583  < 2e-16 ***
#  Vegetationslos   -0.247727   0.035692  -6.941  3.9e-12 ***

# Kleinere Std. Errors verglichen mit dem Estimate repraesentieren eine geringere Variabilität.
# Etwa im Bereich von 10%.

#--------------------------------------------------------------------
#Guetemasse des Modells
#-------------------------------------------------------------------

# Explained deviance 
glm.end$deviance # 2487.343 - gibt die residual deviance an
glm.end$df.residual # 2705 -> Freiheitsgrade assoziiert mit dem Residualmodell
glm.end$null      # 3714.927 - gibt die Devianz im Datensatz an/Nulldevianz;
glm.end$df.null   # 2708  -> Freiheitsgrade assoziiert mit dem Nullmodell
df <- glm.end$df.null - glm.end$df.residual
df # 3

# Selber ein Nullmodell bestimmen:
null.glm <- glm(Goldammer~1,data_goldammer,family="binomial")
null.glm$deviance   # sollte mit glm.end$null uebereinstimmen: 3714.927 -> tut es.
predict(null.glm,data_goldammer,type="response")
mean(data_goldammer$Goldammer)

# Das Nullmodell ist der Mittelwert der response Variable. Gemessen wird der
# Erklaerungswert entsprechend an dieser Referenz:
explDev <- (glm.end$null - glm.end$deviance)/glm.end$null 
explDev
# Die explained deviance liegt bei 0.330 bei einem Verlust von 3 Freiheitsgraden
# Der Anteil erklaerter Devianz liegt bei 33 Prozent --> relativ gering. 


# Diskriminanzmasse
# install.packages("rms")
library(rms) # Regression Modeling Strategies
# Set an schwellenwertabhaengigen und -unabhaengigen Guetemassen:
val.prob(pred.training.data, data_goldammer$Goldammer)
round(val.prob(pred.training.data, data_goldammer$Goldammer),3)


#Presence Absence Evaluierung des Modells:
library(PresenceAbsence)
roc.data <- data.frame(seq(1:length(data_goldammer$Goldammer)), data_goldammer$Goldammer, pred.training.data)
colnames(roc.data) <- c("ID", "Observation", "Pred.GLM")
cmx(roc.data, threshold=.47, which.model=1, na.rm=T)         
# Klassifikationsmatrix; veraendert sich je nach Schwellenwert:
#           observed
#predicted    1    0
#         1  955  334
#         0  234 1186

# ROC-plot und Error rate versus Threshold
presence.absence.summary(roc.data, na.rm=T, which.model=1)   

# Weitere Diskriminanzguetemasse:
presence.absence.accuracy(roc.data, na.rm=T, which.model=1)  
# ROC plot
auc.roc.plot(roc.data)

# AUC --> Area under ROC-curve
auc(roc.data) #AUC = 0.8625916
# --------------------------------------------------------------





