#ZADANIE 1

# Przeprowadź analizę składowych głównych na zbiorze danych mtcars.
data(mtcars)
head(mtcars) #sprawdzenie danych

View(mtcars)
apply(mtcars, 2, var) #sprawdzzenie czy zmienne mają różne skali

# 1) Sprawdź wymagane założenia: 
sum(is.na(mtcars)) #sprawdzenie braku danych
#Sprawdzenie założeń 
#(boxplot, summary, korelacje, test Bartletta)


boxplot(mtcars) #Szukaj kropek (outliers), nierównej długości wąsów
summary(mtcars) #	Sprawdź różnice między medianą a średnią, duże max/min

#„Na podstawie analizy boxplotów i statystyk opisowych z funkcji summary(), 
#zidentyfikowaliśmy potencjalne wartości odstające w zmiennych:
#hp, wt, qsec oraz carb. 
#Wartości te przekraczają zakres wyznaczony przez regułę 1.5×IQR, 
#co może wpływać na wynik analizy PCA. 
#W dalszej części pracy rozważamy usunięcie tych obserwacji lub 
#przynajmniej ich wpływ.”

#dla hp
which(mtcars$hp == 335) #chcemy znaleźć numer tej obserwacji
dane <-mtcars[(!mtcars$hp==335),] #usuwamy wartości odstające w zmiennej hp z danych.

#dla wt
w1w<-quantile(mtcars$wt,0.75)+1.5*IQR(mtcars$wt) #W_górna=Q3+1,5×IQR
w2w<-quantile(mtcars$wt,0.25)-1.5*IQR(mtcars$wt) #W_dolna=Q1-1,5×IQR

which(dane$wt > w1w|dane$wt < w2w)

#dla qsec
w1q<-quantile(mtcars$qsec,0.75)+1.5*IQR(mtcars$qsec) #W_górna=Q3+1,5×IQR
w2q<-quantile(mtcars$qsec,0.25)-1.5*IQR(mtcars$qsec) #W_dolna=Q1-1,5×IQR

which(dane$qsec > w1q|dane$qsec < w2q)

#dla carb
w1c<-quantile(mtcars$carb,0.75)+1.5*IQR(mtcars$carb) #W_górna=Q3+1,5×IQR
w2c<-quantile(mtcars$carb,0.25)-1.5*IQR(mtcars$carb) #W_dolna=Q1-1,5×IQR

which(dane$carb > w1c|dane$carb < w2c)


#usuwamy dla wt, qsec, carb
dane<-dane[!(dane$carb>w1c|
               dane$carb<w2c),]

dane<-dane[!(dane$qsec>w1q|
               dane$qsec<w2q),]

dane<-dane[!(dane$wt>w1w|
               dane$wt<w2w),]


boxplot(dane)

dane<-scale(dane) # standaryzację danych 
boxplot(dane)


#1. sprawdzić skorelowanie zmiennych przed PCA oraz 
#2. wybieramy macierz odpowiednią do analizy: to jest macierz kor.:

#*macierz korelacji (czy są duże korelacje)
#*test Bartletta (czy korelacje są statystycznie istotne)

#install.packages("psych")
library("psych")
cor.mtcars<-cor(dane)
cortest.bartlett(cor(dane),nrow(dane)) #liczba wierszy "nrow" #n <- nrow(dane)  # liczy liczbę obserwacji (wierszy)

#df stopnie swobody
#p.value < 0,05 odczucamy H_0 ( możemy sądzić,  że jest korelacja między zmienymi
#macierz kor.jest różna od macierzy Iedn.)
# chisq - jak bardzo macierz korelacji różni się od macierzy jednostkowej


#3. wyznacz składowe główne 
#wywołuje PCA
PCA_dane<-princomp(dane)


#4. określ ilość rozpatrywanych składowych według różnych kryteriów
#Wyświetlenie wyników:
#oraz 5. zinterpretuj wyniki:

#!Udział opisanej zmienności (wystarczającej proporcji)
summary(PCA_dane)

#	STANDARD DEVIATION: Odchylenie standardowe danej składowej głównej
#(czyli pierwiastek z wartości własnej). Im wyższe, tym ważniejsza składowa.

#PROPORTION OF VARIANCE: Ile procent całkowitej zmienności (informacji) w 
#danych wyjaśnia ta składowa.

#CUMULATIVE PROPORTION: Skumulowany procent wyjaśnionej zmienności – 
#sumuje się od Comp.1 do danej składowej.



#to też wyliczasz wartości i wektory własne macierzy kowariancji — czyli
#w pewnym sensie ręcznie robisz to, co princomp() robi automatycznie:

# Dla porównania i powiązania z teoretycznymi podstawami PCA wyznaczenie macierzy kowariancji:
cov_dane<-cov(dane)
# oraz jej wartości własnych i wektorów własnych:
eigen_dane<-eigen(cov_dane) #wylicza wartości własne (eigenvalues) i 
#wektory własne (eigenvectors)


#Wyświetlenie wyników uzyskanych za pomocą gotowej funkcji oraz tych z macierzy kowariancji i jej
#wartości i wektorów własnych:
eigen_dane$values #wektor wartości własnych

#!!Kryterium Kaisera 
PCA_dane$sdev^2 #sdev^2 = wariancja

PCA_dane$loadings #ładunki czynnikowe
eigen_dane$vectors #wektory własne





PCA_dane$loadings #zwraca ładunki czynnikowe



#!!!Wykres osypiska (scree plot)
screeplot(PCA_dane,type="l") #wykres osypiska” (scree plot) i robimy go, 
#aby zdecydować, ile składowych głównych warto zachować.

#Na podstawie wykresu screeplot zauważyłam, że pierwsze dwie składowe 
#mają wyraźnie największą wartość i tłumaczą większość zmienności w danych.

#Linia na wykresie wyraźnie „załamuje się” po drugiej składowej, co oznacza,
#że kolejne komponenty wnoszą już niewielką ilość informacji.

#Dlatego zdecydowałam się uwzględnić w dalszej analizie pierwsze dwie
#składowe główne, ponieważ one najlepiej opisują strukturę danych.


#!!!!Poglądowość (wybór 2 lub 3 pierwszych składowych dla wizualizacji)
biplot(PCA_dane, choices=1:2)  # wizualizacja na 2 pierwszych składowych

#6. narysuj i zinterpretuj biplot
#Opracowanie biplot do przeprowadzoenj analizy:
biplot(PCA_dane)





#ZADANIE 2
#1.Przeprowadź analizę składowych głównych na zbiorze danych USArrests.

data(USArrests)
head(USArrests) #sprawdzenie danych

View(mtcars)

# 2. Sprawdź wymagane założenia: Sprawdzenie założeń 
#(boxplot, summary, korelacje, test Bartletta)
boxplot(USArrests)
summary(USArrests)

#dla rape
w1r<-quantile(USArrests$Rape,0.75)+1.5*IQR(USArrests$Rape) #W_górna=Q3+1,5×IQR
w2r<-quantile(USArrests$Rape,0.25)-1.5*IQR(USArrests$Rape) #W_dolna=Q1-1,5×IQR

dane1 <-USArrests

which(dane1$Rape > w1r|dane1$Rape < w2r)

#usuwamy dla rape
dane1<-dane1[!(dane1$Rape>w1r|
               dane1$Rape<w2r),]

boxplot(dane1)

dane1<-scale(dane1)
boxplot(dane1)

library("psych")
cor.mtcars<-cor(dane1)
cortest.bartlett(cor(dane1),nrow(dane1))
#p<0.05=> możemy sądzić, że jest korelacja między zm.

#3. wyznacz składowe główne
PCA_dane1<-princomp(dane1)

#4. określ ilość rozpatrywanych składowych według różnych kryteriów
summary(PCA_dane1)



cov_dane1<-cov(dane1)
eigen_dane1<-eigen(cov_dane1)

eigen_dane1$values
PCA_dane1$sdev^2

eigen_dane1$vectors
PCA_dane1$loadings

#5. zinterpretuj wyniki
PCA_dane1$loadings

#6. narysuj i zinterpretuj biplot
biplot(PCA_dane1)


#wykres osypiska
screeplot(PCA_dane1,type="l")


#Dodatkowe możliwości wizualizacji:

#install.packages("FactoMineR")
library("FactoMineR")
pca2_dane1<-PCA(dane1)
pca2_dane1$eig
pca2_dane1$var$cor #korelacja między skl.pl. a zm.

install.packages("corrplot")
library("corrplot")

M<-pca2_dane1$var$cos2
corrplot(M,is.corr=FALSE)
colnames(M) <- c("Z.1","Z.2","Z.3","Z.4")

install.packages("factoextra")
corrplot(M, is.corr=FALSE) library("factoextra")
fviz_eig(pca2_dane1, addlabels = TRUE,
         title="Wykres osypiska",xlab="Składowe główne",
         ylab="Procent wariancji",geom="line")

fviz_pca_biplot(pca2_dane1, repel = TRUE, col.var = "blue",
                col.ind = "black", title="Biplot", xlab="Składowa główna 1",
                ylab="Składowa główna 2")

#np. Alabama:
#Pc1=0.536(13.2)

# PCA z FactoMineR (działa na nie-standaryzowanych danych, ale my mamy już zstandaryzowane)
pca2_dane1 <- PCA(as.data.frame(dane1), graph = FALSE)

# Wartości własne i procent wyjaśnionej wariancji
pca2_dane1$eig

# Korelacje między zmiennymi a składowymi głównymi
pca2_dane1$var$cor

# Wizualizacja cos2 (jakość reprezentacji zmiennych)
M <- pca2_dane1$var$cos2
colnames(M) <- c("Z.1", "Z.2", "Z.3", "Z.4")

corrplot(M, is.corr = FALSE)

# Wykres osypiska
fviz_eig(pca2_dane1, addlabels = TRUE,
         title = "Wykres osypiska",
         xlab = "Składowe główne",
         ylab = "Procent wariancji",
         geom = "line")

# Biplot PCA
fviz_pca_biplot(pca2_dane1, repel = TRUE, 
                col.var = "blue",
                col.ind = "black",
                title = "Biplot",
                xlab = "Składowa główna 1",
                ylab = "Składowa główna 2")




#ZADANIE 3 

#Przeprowadź analizę składowych głównych na zbiorze danych wine
#dostępny pod adresem zamieszczonym poniżej.

wine<-read.table("wine.txt",header=T)
View(wine)
boxplot(wine)

wine<-wine[,-1] #to było potrzebne patrz na dane w kolumne
summary(wine)
boxplot(wine)


install.packages("rstatix")
library("rstatix") #Pakiet rstatix zawiera różne narzędzia do 
#statystycznej analizy danych, np. testy statystyczne, wykrywanie
#wartości odstających (outliers), itp.

identify_outliers(data=wine,variable="V3")
identify_outliers(data=wine,variable="V4")
identify_outliers(data=wine,variable="V5")
identify_outliers(data=wine,variable="V6")
identify_outliers(data=wine,variable="V10")
identify_outliers(data=wine,variable="V11")
identify_outliers(data=wine,variable="V12")

#124 138  174 26 60 122 128 70 74 79 96 111 152 159 160 167 116
wine<-wine[-c(124,138,174, 26, 60, 122, 128, 70, 74, 79, 96, 111, 152, 159, 160, 167, 116),]
summary(scale(wine))
wine<-scale(wine)
boxplot(wine)

library("psych")
cortest.bartlett(cor(wine),nrow(wine))
#p<0.05=> możemy sądzić że jest korelacja i zmienny się korelują


PCA_wine<-princomp(wine)
summary(PCA_wine)
PCA_wine$loadings

#Wykres osypiska
#na podstawie kryterium wyjaśnianej zm. 5 skł.gł.
screeplot(PCA_wine, type = "l")
#kryt.Kaisera:3 skł. gł.
biplot(PCA_wine)
