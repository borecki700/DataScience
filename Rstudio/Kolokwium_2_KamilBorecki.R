#Zad 1
install.packages("Przewodnik")
library(Przewodnik)

data("daneO")
dane<-daneO
dane <- dane[complete.cases(dane), ]
str(dane)

full_model <- glm(dane$Niepowodzenia ~ ., data = dane, family = binomial)
summary(full_model)

exp(coef(full_model)["Nowotwor"])
#5.504359 

step_aic <- step(full_model, direction = "backward", trace = 0)
summary(step_aic)
#model Niepowodzenia ~ Nowotwor + Okres.bez.wznowy + VEGF

new_patient <- data.frame(
  Wiek = mean(dane$Wiek),  # Uśredniamy brakujące zmienne
  Rozmiar.guza = mean(dane$Rozmiar.guza),
  Wezly.chlonne = 0,
  Receptory.estrogenowe = "(-)",
  Receptory.progesteronowe = "(+)",
  Okres.bez.wznowy = 24,
  Nowotwor = 2,
  VEGF = 2000,
)


#Zad 2

dane1 <- warpbreaks
boxplot(dane1$breaks~dane1$tension,ylab = "breaks",xlab = "tension")
summary(dane1)
abline(h=28.15,col="blue")

#H0 \sigma^2_1=\sigma^2_2=\sigma^2_3
bartlett.test(dane1$breaks~dane1$tension)
#p-value<alpha Odrzucamy H0 zatem sądzimy że wariancje nie są sobie równe a co za tym idzie
#wartości przeciętne też nie są sobie równe 
#Wniosek liczba zerwań zależy od napięcia

#Zad3
iris
library(datasets)
head(iris)
library(ggplot2)
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + 
  geom_point()
set.seed(20)
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
irisCluster
table(irisCluster$cluster, iris$Species)
irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(Sepal.Length, Sepal.Width, 
                 color = irisCluster$cluster,
                 shape=Species)) + geom_point()
#Analiza jest skuteczna błednie dopasowane na podstaie czułości i swoistości widać że nieporawnie 
#zostało zakwalifikowanych tylko 6 na 150 przypadków z powodu podobieśtwa szerokości kielicha
#w gatunka versicolor i virginica

#Graficznie widać że kwiat z gatunku setosa ma znadznie szerszy kielich pod pozostałych
#kwiaty gatunku virginica i versicolor mają podobną szerokość do siebie lecz długość inną
#versicolor ma przeciętnie krótszą dł kielicha niż virginica a najkrótszą ma setosa

#Zad 4
library(ggplot2)
library(factoextra)   # do fviz_cluster i fviz_dend
library(datasets) 

data("mtcars")
head(mtcars)
View(mtcars)
plot(mtcars)

dane2<-mtcars
summary(dane2)
my<-scale(dane2)

data_dist<-dist(dane2,method="manhattan")
clust_m<-hclust(data_dist,method="ward.D")
plot(clust_m)
fviz_dend(clust_m, k = 4, rect = TRUE, show_labels = TRUE)
grupy <- cutree(clust_m, k = 4)
fviz_cluster(list(data = my, cluster = grupy))

#Na podstawie dendrogramu można podzilić analizowane obiekty na 4 skupienia

#Zad 5
install.packages("psych")
library("psych")
install.packages("GSE")
library(GSE)
data("geochem")
dane3<-geochem
sum(is.na(dane3)) #Brak NA

boxplot(dane3)
summary(dane3)

install.packages("rstatix")
library(rstatix)

identify_outliers(data=dane3,variable="V2")
identify_outliers(data=dane3,variable="V3")
identify_outliers(data=dane3,variable="V4")
identify_outliers(data=dane3,variable="V6")
identify_outliers(data=dane3,variable="V7")
identify_outliers(data=dane3,variable="V8")
identify_outliers(data=dane3,variable="V9")
identify_outliers(data=dane3,variable="V10")
identify_outliers(data=dane3,variable="V12")
identify_outliers(data=dane3,variable="V13")
identify_outliers(data=dane3,variable="V14")
identify_outliers(data=dane3,variable="V15")
identify_outliers(data=dane3,variable="V16")
identify_outliers(data=dane3,variable="V17")
identify_outliers(data=dane3,variable="V18")
identify_outliers(data=dane3,variable="V19")
identify_outliers(data=dane3,variable="V20")
#1,2,3,4,5,6,7

dane3<-dane3[-c(1,2,3,4,5,6,7),]

library("psych")
cortest.bartlett(cor(dane3),nrow(dane3))
#p-value < alpha zatem macierz rózna od I można PCA

PCA_dane3<-princomp(dane3,cor=TRUE)
summary(PCA_dane3)


PCA_dane3$sdev
PCA_dane3$loadings



#Zatem 5 składowe główne wedlug wyjaśnianej wariancji

screeplot(PCA_dane3, type = "l")

#Zatem 2 składowe główne według osuwiska

abline(h = 1, col = "red") # Kryterium Kaisera (wartości własne > 1)

#Zatem 5 skladowych glownych wedlug Kraisera

biplot(PCA_dane3)
