#Zad 1
library(UsingR)
pi2000
hist(pi2000, breaks = -1:9,probability = TRUE)  #Zmiana jednostek przedziału oraz prawdopodobieństwo
lines(density(pi2000))  #Gęstość dla zbioru
sum(pi2000==1) #Ile jest jedynek w zbiorze #pi2000 ==1 :bool

for(i in 0:9)
{
  print(sum(pi2000==i)/length(pi2000))
}

#Zad 2
install.packages("DAAG")
library(DAAG)
#Trzeba zamienić gęstość na probability
 hist(possum$hdlngth,probability = TRUE);lines(density(possum$hdlngth))
stem(possum$hdlngth) 

#Zad 3
chips
boxplot(chips)

#Zad 4
chicken
boxplot(chicken)

#Zad 5
dane <- kid.weights
dane$age_y<-floor(kid.weights$age/12)
# 1 sposob
boxplot(dane$weight ~dane$age_y)
# 2 sposob
attach(dane)
boxplot(weight~ age_y)
# 3 sposob
boxplot(weight~age_y,data=dane)

#Zad 6
library(MASS)
typeof(survey$Smoke)
dane <- survey$Exer
plot(dane) # wykrs słupkowy
barplot(
  table(dane)) #wykres słupkowy
barplot(
  summary(dane))#wykres słupkowy
pie(
  summary(dane)) # wykres kołowy

plot(survey$Smoke)
barplot(table(survey$Smoke))
pie(table(survey$Smoke),col=1:3)

legend(locator(1),names(table(survey$Smoke)),fill=1:3)


#Zad 7
library(DAAG)
orings

# par(mfrow = c(1,2)) tryb wyświetlania jeden wiersz i dwie kolumny
# par(mfrow = c(2,1)) tryb wyświetlania dwa wiersze jedna kolumna


my.oring <- orings[c(1,2,4,11,13,16), ]
par(mfrow = c(1,2))
plot (my.oring$Temperature,my.oring$Total,xlim = c(50,85),ylim = c(0,5))  #dodajemy zakresy na osi OX
plot (orings$Temperature,orings$Total)
par(mfrow = c(1,1))

#xlim oraz ylim służą do zakresów osi na wykresie

#Zad 8
Manitoba.lakes
plot(Manitoba.lakes$elevation,log(Manitoba.lakes$area),xlim = c(170,280))
text(Manitoba.lakes$elevation,log(Manitoba.lakes$area),labels = rownames(Manitoba.lakes),pos = 4)


# pos = 1 -dół pos = 3 - góra , pos = 4 -prawo , pos = =2 - lewo

#Zad 9
install.packages("LearnBayes")
library(LearnBayes)
studentdata
hist(studentdata$Dvds, breaks = 200 )
table(studentdata$Dvds)
boxplot(table(studentdata$Dvds))
summary(studentdata$Dvds)

boxplot(studentdata$Height ~ studentdata$Gender , horizontal = TRUE)

mean(studentdata[studentdata$Gender=="male","Height"],na.rm = TRUE) 
mean(studentdata[studentdata$Gender =="female","Height"],na.rm = TRUE)

#Zad 10
install.packages("aplpack")
library(aplpack)
stem.leaf(UKDriverDeaths,depths = FALSE)
