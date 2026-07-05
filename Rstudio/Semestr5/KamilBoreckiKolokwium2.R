#Zad 2
install.packages("HistData")
library(HistData)
GaltonFamilies
data("GaltonFamilies")

mean(GaltonFamilies$father)
mean(GaltonFamilies$mother)
mean(GaltonFamilies$midparentHeight)
mean(GaltonFamilies$children)
mean(GaltonFamilies$childNum)
mean(GaltonFamilies$childHeight)

quantile(GaltonFamilies$father)
quantile(GaltonFamilies$mother)
quantile(GaltonFamilies$midparentHeight)
quantile(GaltonFamilies$children)
quantile(GaltonFamilies$childNum)
quantile(GaltonFamilies$childHeight)


sd(GaltonFamilies$father)
sd(GaltonFamilies$mother)
sd(GaltonFamilies$midparentHeight)
sd(GaltonFamilies$children)
sd(GaltonFamilies$childNum)
sd(GaltonFamilies$childHeight)


var(GaltonFamilies$father)
var(GaltonFamilies$mother)
var(GaltonFamilies$midparentHeight)
var(GaltonFamilies$children)
var(GaltonFamilies$childNum)
var(GaltonFamilies$childHeight)



IQR(GaltonFamilies$father)
IQR(GaltonFamilies$mother)
IQR(GaltonFamilies$midparentHeight)
IQR(GaltonFamilies$children)
IQR(GaltonFamilies$childNum)
IQR(GaltonFamilies$childHeight)

install.packages("e1071")
library(e1071)


skewness(GaltonFamilies$father)
skewness(GaltonFamilies$mother)
skewness(GaltonFamilies$midparentHeight)
skewness(GaltonFamilies$children)
skewness(GaltonFamilies$childNum)
skewness(GaltonFamilies$childHeight)

kurtosis(GaltonFamilies$father)
kurtosis(GaltonFamilies$mother)
kurtosis(GaltonFamilies$midparentHeight)
kurtosis(GaltonFamilies$children)
kurtosis(GaltonFamilies$childNum)
kurtosis(GaltonFamilies$childHeight)

#moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(GaltonFamilies$father)
getmode(GaltonFamilies$mother)
getmode(GaltonFamilies$midparentHeight)
getmode(GaltonFamilies$children)
getmode(GaltonFamilies$childNum)
getmode(GaltonFamilies$childHeight)

range(GaltonFamilies$father)
range(GaltonFamilies$mother)
range(GaltonFamilies$midparentHeight)
range(GaltonFamilies$children)
range(GaltonFamilies$childNum)
range(GaltonFamilies$childHeight)


#Zad 3
library(DAAG)
possum

sd(possum$taill)
var(possum$taill)
IQR(possum$taill)
skewness(possum$taill)
kurtosis(possum$taill)

plot(density(possum$skullw))

#Zad 4

rzut<-sample(1:12,12000,replace = TRUE)
table(rzut)

licznik<-0
for(i in 1:10000)
{
  rzut<-sample(1:12,12000,replace=TRUE)
  if(table(rzut)[12]>1030)
    licznik<-licznik+1
}
licznik


#Zad 5
x<-rnorm(1000)
sum(abs(x-0)<=3)

#Zad 6
y1<-rnorm(10)
y2<-rnorm(100)
par(mfrow=c(1,2))
hist(y1,20)
hist(y2,20)
par(mfrow=c(1,1))

#Zad 7
#P(Z<=z) = F(z) = 0.001  jeśli F^_1 istnieje to F^-1(F(z))=F^-1(0.001) czyli Z=Q(0.001)
qt(0.001,df=10)

#Zad 1
library(MASS)
survey

boxplot(survey$Pulse ~ survey$Smoke , horizontal = TRUE)

dane1<-survey[survey$Sex=="Female",]
dane2<-survey[survey$Sex=="Male",]
par(mfrow=c(1,2))
hist(dane1$Height)
hist(dane2$Height)
par(mfrow=c(1,1))

table(survey$Exer)

pie(summary(survey$Wr.Hnd))
