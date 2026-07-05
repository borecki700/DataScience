
#Przykład Siedlaczek
#Taking the numeric part of the IRIS data
data_iris <- iris[1:4]
View(iris)

install.packages("psych")
library("psych")
cor.iris<-cor(data_iris)
cortest.bartlett(cor.iris,n=150)
#p-value<alpha zatem rozni się od jednostkowej można przeprowadzić PCA

#Obliczanie macierzy kowariancji
Cov_data <- cov(data_iris)

#Rozkład wartości własnych i wektorów własnych macierzy kowariancji
Eigen_data <- eigen(Cov_data)

#PCA na macierzy kowariancji (bez standaryzacji zmiennych)
PCA_data <- princomp(data_iris ,cor="False")

#Porówannie wariancji różnych metod
Eigen_data$values #The output is 4.22824171 0.24267075 0.07820950 0.02383509
PCA_data$sdev^2 #The output is 4.20005343 0.24105294 0.07768810 0.02367619

#Ładunki czynnikowe tzn wektory w przestrzeni skl.gł
PCA_data$loadings[,1:4]
Eigen_data$vectors

#Podsumowanie PCA
summary(PCA_data)

#Wykres wektorów w przestrzeni skl.gł
biplot(PCA_data)

#Wykres osuwiska
screeplot(PCA_data, type="lines")

install.packages("FactoMineR")
library("FactoMineR")
iris.pca <- PCA(data_iris,graph = FALSE)
iris.pca$eig #Wariancje i skumulowany % wariancji.
iris.pca$var$cor #Korelacje zmiennych ze składowymi.

install.packages("corrplot")
library("corrplot")
M<-iris.pca$var$cos2
colnames(M) <- c("Z.1","Z.2","Z.3","Z.4")
corrplot(M, is.corr=FALSE)

install.packages("factoextra")
library("factoextra")
fviz_eig(iris.pca, addlabels = TRUE,
         title="Wykres osypiska",xlab="Składowe główne",
         ylab="Procent wariancji",geom="line")
fviz_pca_biplot(iris.pca, repel = TRUE, col.var = "blue",
                col.ind = "black", title="Biplot", xlab="Składowa główna 1",
                ylab="Składowa główna 2")






##ZAD 1

data(mtcars)
head(mtcars)

# Sprawdzenie braków danych
sum(is.na(mtcars)) # Brak NA

# Test Bartletta na sferyczność
library(psych)
cor_mtcars <- cor(mtcars)
cortest.bartlett(cor_mtcars, n = nrow(mtcars)) 
# p-value < 0.05 → PCA jest uzasadnione

# Macierz korelacji vs. kowariancji
# Jeśli zmienne są w różnych jednostkach (np. mpg vs. hp), lepiej użyć macierzy korelacji (standaryzacja)

# PCA na macierzy korelacji (standaryzowane dane)
pca_mtcars <- princomp(mtcars, cor = TRUE) # cor=TRUE to odpowiednik scale.=TRUE w prcomp()

# Alternatywnie z prcomp() (bardziej zalecane):
# pca_mtcars <- prcomp(mtcars, scale. = TRUE)
summary(pca_mtcars)

# Wariancja wyjaśniona
variance <- pca_mtcars$sdev^2
prop_var <- variance / sum(variance)
cumsum(prop_var)

# Wykres osypiska
screeplot(pca_mtcars, type = "lines", main = "Wykres osypiska dla mtcars")
abline(h = 1, col = "red") # Kryterium Kaisera (wartości własne > 1)

# Kryteria:
# - Składowe 1-2 wyjaśniają ~90% wariancji (wystarczające dla wizualizacji)
# - Kryterium Kaisera: 3 składowe (wartości własne > 1)

# Ładunki czynnikowe (pierwsze 3 składowe)
loadings(pca_mtcars)[, 1:3]

# Interpretacja:
# - PC1: Silnie skorelowana z mpg (-), cyl (-), disp (-), hp (-) → "Wydajność vs. Moc"
# - PC2: Silnie skorelowana z gear (+), carb (+) → "Sportowość"
# - PC3: Silnie z qsec (+) → "Przyspieszenie"

# Podstawowy biplot
biplot(pca_mtcars, cex = 0.7, main = "Biplot dla mtcars (PC1 vs PC2)")

# Ulepszona wersja z factoextra
library(factoextra)
fviz_pca_biplot(pca_mtcars, 
                col.ind = "black", col.var = "blue",
                repel = TRUE, title = "Biplot dla mtcars",
                xlab = "PC1 (63%)", ylab = "PC2 (23%)")


library(FactoMineR)
mtcars_pca <- PCA(mtcars, graph = FALSE)
mtcars_pca$eig # Wariancje
mtcars_pca$var$cor # Korelacje zmiennych ze składowymi

# Koreloplot dla cos2
library(corrplot)
corrplot(mtcars_pca$var$cos2, is.corr = FALSE)






##Zad 2

# Ładowanie danych
data(USArrests)
head(USArrests)

# Sprawdzenie braków danych
sum(is.na(USArrests)) # Brak NA

# Test Bartletta na sferyczność
library(psych)
cor_USArrests <- cor(USArrests)
cortest.bartlett(cor_USArrests, n = nrow(USArrests)) 
# p-value < 0.05 → PCA jest uzasadnione

# Macierz korelacji vs. kowariancji
# Zmienne są w różnych jednostkach (np. Murder w liczbie na 100k, UrbanPop w %), 
# więc lepiej użyć macierzy korelacji (standaryzacja)

# PCA na macierzy korelacji (standaryzowane dane)
pca_USArrests <- princomp(USArrests, cor = TRUE) 

# Alternatywnie z prcomp() (bardziej zalecane):
# pca_USArrests <- prcomp(USArrests, scale. = TRUE)
summary(pca_USArrests)

# Wariancja wyjaśniona
variance <- pca_USArrests$sdev^2
prop_var <- variance / sum(variance)
cumsum(prop_var)

# Wykres osypiska
screeplot(pca_USArrests, type = "lines", main = "Wykres osypiska dla USArrests")
abline(h = 1, col = "red") # Kryterium Kaisera (wartości własne > 1)

# Kryteria:
# - Składowe 1-2 wyjaśniają ~87% wariancji (wystarczające dla wizualizacji)
# - Kryterium Kaisera: 1 składowa (tylko PC1 ma wartość własną > 1)

# Ładunki czynnikowe (pierwsze 2 składowe)
loadings(pca_USArrests)[, 1:2]

# Interpretacja:
# - **PC1 (62%)**: Silnie skorelowana z Murder (+), Assault (+), Rape (+) → "Przestępczość"
# - **PC2 (25%)**: Silnie skorelowana z UrbanPop (+) → "Urbanizacja"

# Podstawowy biplot
biplot(pca_USArrests, cex = 0.7, main = "Biplot dla USArrests (PC1 vs PC2)")

# Ulepszona wersja z factoextra
library(factoextra)
fviz_pca_biplot(pca_USArrests, 
                col.ind = "black", col.var = "blue",
                repel = TRUE, title = "Biplot dla USArrests",
                xlab = "PC1 (62%)", ylab = "PC2 (25%)")


library(FactoMineR)
USArrests_pca <- PCA(USArrests, graph = FALSE)
USArrests_pca$eig # Wariancje
USArrests_pca$var$cor # Korelacje zmiennych ze składowymi

# Koreloplot dla cos2
library(corrplot)
corrplot(USArrests_pca$var$cos2, is.corr = FALSE)






##ZAD 3

# Pobranie danych
wine_data <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", 
                        sep = ",", header = FALSE)
# Nadanie nazw kolumnom (zgodnie z dokumentacją)
colnames(wine_data) <- c("Class", "Alcohol", "Malic_acid", "Ash", "Alcalinity_of_ash", 
                         "Magnesium", "Total_phenols", "Flavanoids", "Nonflavanoid_phenols",
                         "Proanthocyanins", "Color_intensity", "Hue", "OD280_OD315", "Proline")

# Wybór tylko zmiennych numerycznych (pomijamy kolumnę 'Class')
wine_numeric <- wine_data[, -1]

# Sprawdzenie braków danych
sum(is.na(wine_numeric)) # Brak NA

# Test Bartletta na sferyczność
library(psych)
cor_wine <- cor(wine_numeric)
cortest.bartlett(cor_wine, n = nrow(wine_numeric)) 
# p-value < 0.05 → PCA jest uzasadnione

# Macierz korelacji vs. kowariancji
# Zmienne są w różnych jednostkach, więc używamy macierzy korelacji (standaryzacja)

# PCA z standaryzacją
pca_wine <- prcomp(wine_numeric, scale. = TRUE)

# Podsumowanie
summary(pca_wine)

# Wariancja wyjaśniona
variance <- pca_wine$sdev^2
prop_var <- variance / sum(variance)
cumsum(prop_var)

# Wykres osypiska
screeplot(pca_wine, type = "lines", main = "Wykres osypiska dla wine")
abline(h = 1, col = "red") # Kryterium Kaisera (wartości własne > 1)

# Kryteria:
# - Kryterium Kaisera: wartości własne > 1 → 3 składowe
# - Składowe 1-3 wyjaśniają ~66% wariancji
# - Punkt "łokcia" na wykresie osypiska sugeruje 3-4 składowe

# Ładunki czynnikowe dla pierwszych 3 składowych
loadings <- pca_wine$rotation[, 1:3]
print(loadings)

# Interpretacja:
# - **PC1 (36%)**: Silnie skorelowana z `Flavanoids` (+), `Total_phenols` (+), `OD280_OD315` (+) 
#   → "Polifenole i barwa wina"
# - **PC2 (19%)**: Silnie skorelowana z `Color_intensity` (+), `Proline` (+) 
#   → "Intensywność koloru i proline"
# - **PC3 (11%)**: Silnie skorelowana z `Ash` (+), `Alcalinity_of_ash` (+) 
#   → "Skład mineralny"

# Biplot dla PC1 i PC2
biplot(pca_wine, cex = 0.7, main = "Biplot dla wine (PC1 vs PC2)")






##ZAD 4
# Załadowanie pakietów i danych
library(factoextra)
data(decathlon2)

# Przygotowanie danych (wybór tylko aktywnych zawodników i zmiennych numerycznych)
decathlon_active <- decathlon2[1:23, 1:10] # 23 zawodników, 10 konkurencji
head(decathlon_active)

# Sprawdzenie braków danych
sum(is.na(decathlon_active)) # Brak NA

# Test Bartletta na sferyczność
library(psych)
cor_decathlon <- cor(decathlon_active)
cortest.bartlett(cor_decathlon, n = nrow(decathlon_active))
# p-value < 0.05 → PCA jest uzasadnione

pca_decathlon <- prcomp(decathlon_active, scale. = TRUE)

# Podsumowanie
summary(pca_decathlon)

# Wariancja wyjaśniona
variance <- pca_decathlon$sdev^2
prop_var <- variance / sum(variance)
cumsum(prop_var)

# Wykres osypiska
screeplot(pca_decathlon, type = "lines", main = "Wykres osypiska dla wine")
abline(h = 1, col = "red") # Kryterium Kaisera (wartości własne > 1)

# Kryteria:
# - Kryterium Kaisera: wartości własne > 1 → 2 składowe
# - Składowe 1-2 wyjaśniają ~60% wariancji
# - Punkt "łokcia" na wykresie sugeruje 2-3 składowe

# Ładunki czynnikowe dla pierwszych 2 składowych
loadings <- pca_decathlon$rotation[, 1:2]
print(loadings)

# Interpretacja:
# - **PC1 (40%)**: Silnie dodatnio skorelowana z `100m` (-), `400m` (-), `110m.hurdle` (-)  
#   → "Szybkość i zwinność" (im niższy wynik, tym lepiej)
# - **PC2 (20%)**: Silnie dodatnio skorelowana z `Shot.put` (+), `Discus` (+), `Javeline` (+)  
#   → "Siła i rzuty"

# Biplot dla PC1 i PC2
biplot(pca_decathlon, cex = 0.7, main = "Biplot dla wine (PC1 vs PC2)")

library(FactoMineR)
decathlon_pca <- PCA(decathlon_active, graph = FALSE)
decathlon_pca$eig # Wariancje
decathlon_pca$var$cor # Korelacje zmiennych ze składowymi

# Koreloplot dla cos2
library(corrplot)
corrplot(decathlon_pca$var$cos2, is.corr = FALSE)
