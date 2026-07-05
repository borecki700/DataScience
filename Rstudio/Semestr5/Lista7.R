#Lista 7
#Laboratorium statystyczne
#Analiza statystyczna
#Sprawdź, czego dotyczy zbiór danych iris i jakie zmienne zawiera. Sporządź analizę tego zbioru. Na analizę powinny się składać:
  #wskaźniki liczbowe:
 # – miary położenia - średnia, kwartyle, moda,
#– miary rozproszenia (zmienności) - rozstęp, rozstęp międzykwartylowy, maksimum, minimum, wariancja, odchylenie standardowe,
#współczynnik zmienności,
#– miary asymetrii - współczynnik skośności,
#– miary spłaszczenia (koncentracji) - kurtoza,
# prezentacja graficzna danych:
 # – wykres słupkowy i kołowy,
#– histogram,
#– łamaną częstości,
#– wykres pudełkowy,
#– wykres gęstości,
#wykres zależności między zmiennymi ilościowymi z podziałem według
#zmiennej jakościowej,
# średnie dla każdej zmiennej w podziale wg zmiennej jakościowej,
# opis danych sklasyfikowanych jako odstające.
#Zinterpretuj wszystkie elementy analizy

# Wczytanie danych i ich opis
data(iris)
summary(iris)  # Podstawowe statystyki opisowe dla wszystkich zmiennych

# Miary położenia: średnia, kwartyle, moda
mean(iris$Sepal.Length)
mean(iris$Sepal.Width)
mean(iris$Petal.Length)
mean(iris$Petal.Width)

quantile(iris$Sepal.Length)
quantile(iris$Sepal.Width)
quantile(iris$Petal.Length)
quantile(iris$Petal.Width)

# Funkcja do obliczania mody (wartość najczęściej występującą)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(iris$Sepal.Length)
getmode(iris$Sepal.Width)
getmode(iris$Petal.Length)
getmode(iris$Petal.Width)

# Miary rozproszenia: rozstęp, wariancja, odchylenie standardowe
range(iris$Sepal.Length)
range(iris$Sepal.Width)
range(iris$Petal.Length)
range(iris$Petal.Width)

var(iris$Sepal.Length)
var(iris$Sepal.Width)
var(iris$Petal.Length)
var(iris$Petal.Width)

sd(iris$Sepal.Length)
sd(iris$Sepal.Width)
sd(iris$Petal.Length)
sd(iris$Petal.Width)

IQR(iris$Sepal.Length)  # Rozstęp międzykwartylowy
IQR(iris$Sepal.Width)
IQR(iris$Petal.Length)
IQR(iris$Petal.Width)

# Miary asymetrii: współczynnik skośności
install.packages("e1071")
library(e1071)

skewness(iris$Sepal.Length)
skewness(iris$Sepal.Width)
skewness(iris$Petal.Length)
skewness(iris$Petal.Width)

# Miary spłaszczenia: kurtoza
kurtosis(iris$Sepal.Length)
kurtosis(iris$Sepal.Width)
kurtosis(iris$Petal.Length)
kurtosis(iris$Petal.Width)

# Prezentacja graficzna:
par(mfrow=c(2,2))
hist(iris$Sepal.Length, main="Histogram Sepal Length")
hist(iris$Sepal.Width, main="Histogram Sepal Width")
hist(iris$Petal.Length, main="Histogram Petal Length")
hist(iris$Petal.Width, main="Histogram Petal Width")

par(mfrow=c(2,2))
boxplot(iris$Sepal.Length, main="Boxplot Sepal Length")
boxplot(iris$Sepal.Width, main="Boxplot Sepal Width")
boxplot(iris$Petal.Length, main="Boxplot Petal Length")
boxplot(iris$Petal.Width, main="Boxplot Petal Width")

# Wykresy gęstości
par(mfrow=c(2,2))
plot(density(iris$Sepal.Length), main="Density Plot Sepal Length")
plot(density(iris$Sepal.Width), main="Density Plot Sepal Width")
plot(density(iris$Petal.Length), main="Density Plot Petal Length")
plot(density(iris$Petal.Width), main="Density Plot Petal Width")

# Wykres zależności między zmiennymi ilościowymi z podziałem na zmienną jakościową (Species)
par(mfrow=c(1,1))
pairs(iris[,1:4], col=iris$Species)

# Obliczanie średnich dla każdej zmiennej w podziale wg zmiennej jakościowej
aggregate(iris[,1:4], by=list(Species=iris$Species), FUN=mean)

# Zidentyfikowanie i opisanie wartości odstających:
boxplot(iris$Sepal.Length, main="Boxplot Sepal Length", outline=TRUE)

