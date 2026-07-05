#Zad 1
diet<-read.csv("C:/Users/Kamil/OneDrive/Pulpit/Semestr 6/Borecki R Studio/diet.csv")
diet$utrata<-diet$pre.weight-diet$weight6weeks

boxplot(diet$utrata~diet$Diet,ylab = "utrata wagi (wkg)",xlab = "typ diety")
abline(h=0,col="blue")
bartlett.test(diet$utrata~diet$Diet)
#barlett.text(diet$Diet~diet$utrata)

#H0 \sigma^2_1=\sigma^2_2=\sigma^2_3
#p=0.84 >\alpha Przyjmujemy H0 możemy sądzić że war są równe możemy przeprowadzić analizę wariancji
aov_diet<-aov(diet$utrata~diet$Diet)
summary(aov_diet)
#H0 \mi_1=\mi_2=\mi_3
#p=0.007 <\alpha Zatem odrzucamy H0 utrata wagi jest przecietnie rozna dla roznych rodzajow diety
#utrata wagi zależy od rodzaju diety

#Zad 2
PlantGrowth
boxplot(weight~group,data=PlantGrowth)
abline(h=mean(PlantGrowth$weight),col="red")
bartlett.test(weight~group,data = PlantGrowth)
#H0 \sigma^2_1=\sigma^2_2=\sigma^2_3
#p=0.2371 >\alpha=0.001 Przyjmujemy H0 możemy sądzić że war są równe możemy przeprowadzić analizę wariancji
mod_aov<-aov(weight~group,data = PlantGrowth)
summary(mod_aov)
#H0 \mi_1=\mi_2=\mi_3
#p=0.0159>\alpha=0.001 Nie ma podstaw do odrzucenia H0 mozemy sadzic ze wartosc przecietna 
#waga nie zalezy od rodzaju traktowania

#Zad 3

tyre<-read.csv("C:/Users/Kamil/OneDrive/Pulpit/Semestr 6/Borecki R Studio/tyre.csv")
boxplot(tyre$Mileag~tyre$Brands)
abline(h=mean(tyre$Mileage),col=1.4)
bartlett.test(tyre$Mileage~tyre$Brands)
#H0 \sigma^2_1=\sigma^2_2=\sigma^2_3
#p=0.5419 >\alpha=0.001 Przyjmujemy H0 możemy sądzić że war są równe możemy przeprowadzić analizę wariancji
tyre_aov<-aov(Mileage~Brands,data=tyre)
summary(tyre_aov)
#p=2.78*10^(-8)<\alpha należy odrzucić H0 mozemy sadzic ze wartosc przecietna przebieguoporu
#rozni sie w zaleznosci od marki opon

#Zad 4

InsectSprays
boxplot(count~spray,data=InsectSprays)
abline(h=mean(InsectSprays$count),col="red")
bartlett.test(count~spray,data = InsectSprays)
#H0 \sigma^2_1=\sigma^2_2=\sigma^2_3=\sigma^2_4=\sigma^2_5=\sigma^2_6
#p=9.085*10^(-5)<\alpha= Odrzucamy H0 nie możemy przeprowadzić analizę wariancji bo wariancje rozne

