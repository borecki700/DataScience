iris
summary(iris)

var(iris$Sepal.Length)
var(iris$Sepal.Width)
var(iris$Petal.Length)
var(iris$Petal.Width)

sd(iris$Sepal.Length)
sd(iris$Sepal.Width)
sd(iris$Petal.Length)
sd(iris$Petal.Width)

var(iris[,1:4]) #Na przekątnej znajdujemy wariację

IQR(iris$Sepal.Length)
IQR(iris$Sepal.Width)
IQR(iris$Petal.Length)
IQR(iris$Petal.Width)

install.packages("e1071")
library(e1071)
skewness(iris$Sepal.Length)
skewness(iris$Sepal.Width)
skewness(iris$Petal.Length)
skewness(iris$Petal.Width)

kurtosis(iris$Sepal.Length)
kurtosis(iris$Sepal.Width)
kurtosis(iris$Petal.Length)
kurtosis(iris$Petal.Width)

#moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode((iris$Sepal.Length))
getmode((iris$Sepal.Width))
getmode((iris$Petal.Length))
getmode((iris$Petal.Width))

range(iris$Sepal.Length)
range(iris$Sepal.Width)
range(iris$Petal.Length)
range(iris$Petal.Width)


pie(table(iris$Species))
barplot(table(iris$Species))

par(mfrow=c(2,2))
hist((iris$Sepal.Length))
hist((iris$Sepal.Width))
hist((iris$Petal.Length))
hist((iris$Petal.Width))

boxplot((iris$Sepal.Length))
boxplot((iris$Sepal.Width))
boxplot((iris$Petal.Length))
boxplot((iris$Petal.Width))

plot(density(iris$Sepal.Length))
plot(density(iris$Sepal.Width))
plot(density(iris$Petal.Length))
plot(density(iris$Petal.Width))

par(mfrow=c(1,1))

pairs(iris[,1:4],col=iris$Species)

#aggregate(dane,list(grupa),FUN=funkcja)
aggregate(iris[,1:4],list(iris$Species),FUN = mean)
