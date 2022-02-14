#ANALISI UNIVARIATA- FREQUENZA

setwd("/Users/filippo/Desktop")
read.csv("a.csv",header=TRUE)
a<-read.csv("a.csv",header=TRUE)
library(descr)
freq(a$CONOSC_CAPSULE)

#ANALISI UNIVARIATA - MISURE DI SINTESI 

setwd("/Users/filippo/Desktop")
read.csv("a.csv",header=TRUE)
a<-read.csv("a.csv",header=TRUE)
library(fBasics)
library(timeDate)
library(timeSeries)
basicStats(a$SPESA)
boxplot(a$SPESA)

#ANALISI UNIVARIATA - ISTOGRAMMA

setwd("/Users/filippo/Desktop")
read.csv("a.csv",header=TRUE)
a<-read.csv("a.csv",header=TRUE)
hist(a$SPESA)

#ANALISI UNIVARIATA - GRAFICO A TORTA

setwd("/Users/filippo/Desktop")
read.csv("a.csv",header=TRUE)
a<-read.csv("a.csv",header=TRUE)
pie(table(a$FIN_VIAGGI))
library(descr)
freq(a$FIN_VIAGGI)

#ANALISI BIVARIATA - ANALISI DI CONESSIONE 

setwd("/Users/filippo/Desktop")
read.csv("a.csv",header=TRUE)
a<-read.csv("a.csv",header=TRUE)
library(descr)
CrossTable(a$ALLOGG_DIVERSO,a$ALLOGG_CAPS,prop.chisq=FALSE)
CrossTable(a$ALLOGG_DIVERSO,a$ALLOGG_CAPS,prop.chisq=FALSE,chisq=TRUE)
library(DescTools)
CramerV(a$ALLOGG_DIVERSO,a$ALLOGG_CAPS)

#ANALISI BIVARIATA - ANALISI DI CONESSIONE 2

setwd("/Users/filippo/Desktop")
read.csv("a.csv",header=TRUE)
a<-read.csv("a.csv",header=TRUE)
library(descr)
CrossTable(a$FIN_VIAGGI,a$ALLOGG_CAPS,prop.chisq=FALSE)
CrossTable(a$FIN_VIAGGI,a$ALLOGG_CAPS,prop.chisq=FALSE,chisq=TRUE)
library(DescTools)
CramerV(a$FIN_VIAGGI,a$ALLOGG_CAPS)

#ANALISI BIVARIATA - ANALISI DI CORRELAZIONE LINEARE 

setwd("/Users/filippo/Desktop")
read.csv("a.csv",header=TRUE)
a<-read.csv("a.csv",header=TRUE)
cor.test(a$IMP_PREZZO,a$QNT_IMPEGNO)
plot(a$IMP_PREZZO,a$QNT_IMPEGNO)

#ANALISI BIVARIATA - ANALISI DI CORRELAZIONE LINEARE 2

setwd("/Users/filippo/Desktop")
read.csv("a.csv",header=TRUE)
a<-read.csv("a.csv",header=TRUE)
cor.test(a$SPESA,a$VIAGGI_ANNO)
plot(a$SPESA,a$VIAGGI_ANNO)

# ANALISI BIVARIATA - ANALISI DI ANOVA

setwd("/Users/filippo/Desktop")
read.csv("a.csv",header=TRUE)
a<-read.csv("a.csv",header=TRUE)
anova<-aov(VIAGGI_ANNO~ FIN_VIAGGI,data=a)
model.tables(anova,type="means")
library(lsr)
etaSquared(anova)
summary(anova)

#ANALISI FATTORIALE

setwd("/Users/filippo/Desktop")
read.csv("a.csv",header=TRUE)
a<-read.csv("a.csv",header=TRUE)
a2<-a[,c("IMP_ESTETICA","IMP_COMFORT","IMP_PRIVACY","IMP_POSIZIONE","IMP_PREZZO","IMP_BAGAGLI","IMP_PULIZIA","IMP_OPTIONAL","IMP_H24","IMP_DIMENSIONE","IMP_PASTI","INC_COVID","IMP_QNT_NOTTI","VIAGGI_ANNO")]
fit<-princomp(a2,cor=TRUE)
summary(fit)
library(ggrepel)
library(factoextra)
eig.val<-get_eigenvalue(fit)
eig.val
plot(fit,type="lines")
library(psych)
library(ggplot2)
z<-principal(a2,residuals=FALSE,nfactors=5,rotate="none")
z
z$communality
z3<-principal(a2,residuals=FALSE,nfactors=5,rotate="varimax")
print(z3$loadings,sort=TRUE,cutoff=0.4)
fa.diagram(z3)

#confronto tra possibilità a 4 o 5 fattori - è stata scelta quella a 5

z2<-principal(a2,residuals=FALSE,nfactors=4,rotate="none")
z2
z2$communality
cinque_fattori<-z$communality
quattro_fattori<-z2$communality
quattro_fattori
cinque_fattori
confronto<-cbind(cinque_fattori,quattro_fattori)
confronto

#MODELLO REGRESSIONE LINEARE

#importazione dei fattori trovati tramite l'analisi fattoriale

setwd("/Users/filippo/Desktop")
read.csv("a.csv",header=TRUE)
a<-read.csv("a.csv",header=TRUE)
a2<-a[,c("IMP_ESTETICA","IMP_COMFORT","IMP_PRIVACY","IMP_POSIZIONE","IMP_PREZZO","IMP_BAGAGLI","IMP_PULIZIA","IMP_OPTIONAL","IMP_H24","IMP_DIMENSIONE","IMP_PASTI","INC_COVID","IMP_QNT_NOTTI","VIAGGI_ANNO")]
fit<-princomp(a2,cor=TRUE)
summary(fit)
library(ggrepel)
library(factoextra)
eig.val<-get_eigenvalue(fit)
eig.val
library(psych)
library(ggplot2)
z<-principal(a2,residuals=FALSE,nfactors=5,rotate="none")
z
z$communality
z3<-principal(a2,residuals=FALSE,nfactors=5,rotate="varimax")
print(z3$loadings,sort=TRUE,cutoff=0.4)
z3<-principal(a2,residuals=FALSE,nfactors=5,rotate="varimax",score=TRUE)
z4<-cbind(a,z3$scores)
library(plyr)
z4<-rename(z4,c("RC5"="CLASSICO","RC1"="FORTE","RC2"="DEBOLE","RC3"="EXTRA","RC4"="QUANTITA"))

#verifico la presenza di multicollinearità tra i vari fattori (anche se è impossibile ci sia)

library(usdm)
library(sp)
library(raster)
library(terra)
s<-z4[,c("CLASSICO","FORTE","DEBOLE","EXTRA","QUANTITA")]
vif(s)

#analisi di regressione lineare

spesa<-lm(SPESA~CLASSICO+FORTE+DEBOLE+EXTRA+QUANTITA,data=z4)
summary(spesa)
p<-step(spesa,direction="both")
summary(p)
library(QuantPsyc)
lm.beta(p)

#confronto con gli altri 2 metodi di selezione automatica

v<-step(spesa2,direction="backward")
summary(v)
y<-step(spesa2,direction="forward")
summary(y)

#MODELLO DI REGRESSIONE LOGISTICA

#importazione dei fattori trovati tramite l'analisi fattoriale

setwd("/Users/filippo/Desktop")
read.csv("a.csv",header=TRUE)
a<-read.csv("a.csv",header=TRUE)
a2<-a[,c("IMP_ESTETICA","IMP_COMFORT","IMP_PRIVACY","IMP_POSIZIONE","IMP_PREZZO","IMP_BAGAGLI","IMP_PULIZIA","IMP_OPTIONAL","IMP_H24","IMP_DIMENSIONE","IMP_PASTI","INC_COVID","IMP_QNT_NOTTI","VIAGGI_ANNO")]
fit<-princomp(a2,cor=TRUE)
summary(fit)
library(ggrepel)
library(factoextra)
eig.val<-get_eigenvalue(fit)
eig.val
library(psych)
library(ggplot2)
z3<-principal(a2,residuals=FALSE,nfactors=5,rotate="varimax")
print(z3$loadings,sort=TRUE,cutoff=0.4)
z3<-principal(a2,residuals=FALSE,nfactors=5,rotate="varimax",score=TRUE)
z4<-cbind(a,z3$scores)
library(plyr)
z4<-rename(z4,c("RC5"="CLASSICO","RC1"="FORTE","RC2"="DEBOLE","RC3"="EXTRA","RC4"="QUANTITA"))
names(z4)
z5<-z4[,c("CLASSICO","FORTE","DEBOLE","EXTRA","QUANTITA")]

#verifico la presenza di multicollinearità tra i vari fattori (anche se è impossibile ci sia)

library(usdm)
library(sp)
library(raster)
library(terra)
vif(z5)

#analisi di regressione logistica

table(a$ALLOGG_CAPS)
mylogit<-glm(ALLOGG_CAPS~CLASSICO+FORTE+DEBOLE+EXTRA+QUANTITA,data=z4,family="binomial")
summary(mylogit)
a<-step(mylogit,direction="both")
summary(a)
library(QuantPsyc)
exp(a$coefficient)
library(lmtest)
library(zoo)
library(terra)
waldtest(a)
CalculateConcordance <- function (myMod){
  fitted <- data.frame (cbind (myMod$y, myMod$fitted.values)) # actuals and fitted
  colnames(fitted) <- c('response','score') # rename columns
  ones <- fitted[fitted$response==1, ] # Subset ones
  
  zeros <- fitted[fitted$response==0, ] # Subsetzeros
  
  totalPairs <- nrow (ones) * nrow (zeros) # calculate total number of pairs to check
  
  conc <- sum (c (vapply (ones$score, function(x) {((x > zeros$score))}, FUN.VALUE=logical(nrow(zeros)))))
  
  disc <- totalPairs - conc
  
  # Calc concordance, discordance and ties
  
  concordance <- round(conc/totalPairs,digit=4)
  
  discordance <- round(disc/totalPairs,digit=4)
  
  tiesPercent <- round((1-concordance-discordance),digit=4)
  
  return(list("Concordance"=concordance, "Discordance"=discordance,"Tied"=tiesPercent, "Pairs"=totalPairs))
}
CalculateConcordance(a)
lm.beta(a)
exp(a$coefficient)









