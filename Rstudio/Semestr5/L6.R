#Zad1

install.packages("MASS")
library(MASS)

hist(DDT)
boxplot(DDT)
#Wartość średnia
mean(DDT)
#Odchylenie standardowe
sd(DDT)
abline(v=mean(DDT)) 
arrows(mean(DDT),0,mean(DDT)+sd(DDT),0)
arrows(mean(DDT),0,mean(DDT)-sd(DDT),0)



#Zad 4
dotarcie<-c(9, 6, 9, 9, 6, 8, 5, 5, 8, 5, 5, 7, 8, 9, 8, 13, 12, 14, 15, 13, 13, 13, 16, 15, 15,
            13, 15, 15, 15, 15, 15, 16, 14, 12, 16, 12, 13, 16, 14, 12, 15, 14, 15, 16, 13, 19,
            20, 21, 21, 18, 21, 20, 22, 18, 21, 20, 21, 22, 21, 22, 20, 19, 19, 22, 21, 18, 20,
            19, 19, 22, 21, 21, 19, 18, 21, 20, 20, 18, 19, 19, 21, 19, 22, 20, 20)
hist(dotarcie)
dotarcie_sposob1<-dotarcie[dotarcie<10]
dotarcie_sposob2<-dotarcie[dotarcie>10 & dotarcie<17]
dotarcie_sposob3<-dotarcie[dotarcie>=17]

#Zad 5
library(UsingR)
par(mfrow=c(2,2))
hist(aid)
hist(crime$y1983)
hist(crime$y1993)
hist(south)

boxplot(aid)
boxplot(crime$y1983)
boxplot(crime$y1993)
boxplot(south)

par(mfrow=c(1,1))

#Zad 6
library(UsingR)
hist(bumpers)
mean(bumpers)
summary(bumpers)

hist(firstchi)
mean(firstchi)
summary(firstchi)

hist(math)
mean(math)
summary(math)

#Zad 7
library(DAAG)
possum
#statystyki porządkowe
stat_porz<-sort(possum$hdlngth)
stat_porz[115]
IQR(stat_porz)
install.packages("e1071")
library(e1071)
skewness(stat_porz)
kurtosis(stat_porz)

#Zad 8
library(UsingR)
hist(slc) # rozkład asymetryczny dlatego lepsza miara - mediana
summary(slc)
median(slc)

#Zad 9
library(DAAG)
possum
dlg_samic<-possum$totlngth[possum$sex=="f"]
mean(dlg_samic, na.rm= TRUE)
mean(dlg_samic,trim = 0.1)
median(dlg_samic)
