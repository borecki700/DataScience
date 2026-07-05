#Metoda k-średnich:

#cluster analysis<- kmeans(data, k)
#as.factor(cluster analysis$cluster)

#Analiza skupien w R:

#dist(mydata, method = "...")
#hclust(distance matrix, method="...")
#plot(h analysis)
#rect.hclust(h analysis, k, border="red")
#cutree(h analysis, k)




#ZADANIE 1

#Przeprowadź analizę skupień niehierarchiczną metodą k-średnich na
#zbiorze danych uber załączonym w materiałach. 

install.packages("dplyr")
library("dplyr") #służy do pracy z danymi ramkowymi
apr <- read.csv("uber-raw-data-apr14.csv")
may <- read.csv("uber-raw-data-may14.csv")
jun <- read.csv("uber-raw-data-jun14.csv")
jul <- read.csv("uber-raw-data-jul14.csv")
aug <- read.csv("uber-raw-data-aug14.csv")
sep <- read.csv("uber-raw-data-sep14.csv")
#Date.Time
#lat
#lon
#Base


#Aby połączyć dane z wielu miesięcy w jeden wspólny zbiór (data_uber),
#na którym można łatwo pracować, analizować i wizualizowa:
data_uber<-bind_rows(apr,may,jun,jul,aug,sep)


##cluster analysis<- kmeans(data, k)

#polecenia w RStudio wykonują automatycznie cały algorytm k-średnich – 
#wszystko od kroku 1 do 6 – dzięki funkcji kmeans():
uber_clusters<-kmeans(data_uber[,2:3],5) #5-liczba skupeń (klastrów)
#[2:3] - wybieramy kolumny z liczbami

#zwraca numery przypisanych klastrów (Przypisanie obiektów do skupeń)
uber_clusters$cluster

#Ustalenie nowych środków skupień
uber_clusters$centers

#Sprawdzenie, czy środki się zmieniły, i ewentualne powtórzenie
uber_clusters$iter

#Dodatkowo możesz zobaczyć:
str(uber_clusters)
#To pokaże Ci:
#cluster – przypisania obiektów (czyli krok 4),
#centers – końcowe środki klastrów (krok 5),
#iter – ile razy algorytm powtórzył kroki 3–5 (czyli ile iteracji było do zbieżności – krok 6).



data_uber$Borough<-as.factor(uber_clusters$cluster)#przyczepia informację 
#o przynależności do klastra jako nową zmienną do Twoich danych.

View(data_uber)

#rzut kostka 10 razy
#sample(1:6,10,replace=T)

#oznacza, że tworzysz mniejszy zbiór danych uber_small, zawierający losowo 
#wybrane 10 000 wierszy z kolumn 2 i 3 zbioru data_uber.
uber_small<-data_uber[
  sample(1:nrow(data_uber),10000),2:3]

plot(uber_small)

# Wybieramy 10000 losowych indeksów
set.seed(123)  # dla powtarzalności
sample_indices <- sample(1:nrow(data_uber), 10000)

# Tworzymy próbkę wraz z kolumną Borough (klaster)
uber_small <- data_uber[sample_indices, c("Lat", "Lon", "Borough")]

library(ggplot2)
ggplot(uber_small, aes(x = Lat, y = Lon, color = Borough)) +
  geom_point()


#ZADANIE 2

#Przeprowadź analizę skupień na zbiorze danych ruspini z pakietu cluster 
#dwiema różnymi metodami hierarchicznymi:

# 1) Algorytm łączące
# 2) Algorytm dzielący

#z użyciem metody pojedynczego wiązania i pełnego wiązania.

# Wczytaj pakiety
library(cluster) #Pakiet do analizy skupień 
library(ggplot2)

# Załaduj dane
data(ruspini)
head(ruspini)
View(ruspini)

plot(ruspini)

my<-ruspini
summary(my)

my<-na.omit(ruspini) #usuwa z danych wszystkie wiersze, które mają brakujące
#wartości
my<-scale(my) #standaryzacja

#Dane (punkty) 
#↓ (metryka odległości np. Euclidean)
#Macierz odległości punktów 
#↓ (metoda wiązania np. single linkage)
#Odległości między klastrami → hierarchiczne grupowanie

data_dist<-dist(my,method="euclidean") #maczierz odleglosci

#1) Algorytm łączące
#metoda hierarchiczna

#metoda pojedynczego wiązania (single linkage).
data_hclust<-hclust(data_dist,method ="single" )

plot(data_hclust)
rect.hclust(data_hclust,k=4,border=2:5) #(podział na klastry)

#metoda:pełnego wiązania
data_hclust_c<-hclust(data_dist,method="complete")

plot(data_hclust_c)
rect.hclust(data_hclust,k=4,border=2:5)

c("navy","pink","red","blue")
group_s<-cutree(data_hclust,k=4) # przypisuje każdej obserwacji numer klastra,
#do którego należy.
group_c<-cutree(data_hclust_c,k=4)

#to trzeba zrobić żeby wszystko dziawało
my <- scale(my)
my <- as.data.frame(my)
colnames(my) <- c("x", "y")
my$group_s <- as.factor(group_s)
my$group_c <- as.factor(group_c)

ggplot(my, aes(x,y,color=group_s,
               shape=group_c))+
  geom_point()



#Zinterpretuj otrzymane wyniki np. za pomocą funkcji cluster.stats. 
install.packages("fpc")
library(fpc)
#Ocena jakości grupowania
cluster.stats(data_dist,group_s)
cluster.stats(data_dist,group_c)

#!!!statystyki oceniające jakość klasteryzacji

#Podstawowe informacje:
#$n	Liczba obserwacji 
#$cluster.number	Liczba klastrów
#$cluster.size	Liczba punktów w każdym klastrze
#$noisen	Liczba punktów uznanych za "szum" (np. poza klastrami):


#Rozmiary i spójność klastrów:
#diametr - średnica (największa odległość między dowolnymi dwoma punktami w
#klastrze. Im mniejsza, tym klaster bardziej zwarty.)

#$average.distance	Średnia odległość między wszystkimi parami punktów w
#klastrze. Też miara zwartej struktury.

#$median.distance	Mediana odległości w klastrze.
#Pozwala unikać wpływu ekstremów.


#Separacja między klastrami:
#separation	Najmniejsza odległość między punktami z danego klastra
#a punktami z innych klastrów.

#$average.toother	Średnia odległość punktów z danego klastra do punktów
#z innych klastrów.


#Zbiorcze miary między i wewnątrz klastrów:
#$average.between	Średnia odległość między klastrami. Im większa, tym lepiej.
#$average.within	Średnia odległość wewnątrz klastrów. Im mniejsza, tym lepiej.
#$wb.ratio	Wskaźnik wewnętrzne / międzygrupowe. Im mniejszy, tym lepiej.
#$within.cluster.ss	Suma kwadratów odległości punktów od środka klastra 
#(im mniejsza, tym bardziej zbity klaster).
#$n.between, $n.within	Liczba par punktów między i wewnątrz klastrów
#(do obliczeń statystyk).


#Miary jakości grupowania:
#$clus.avg.silwidths	Średnia szerokość sylwetki dla każdego klastra.
#Im bliżej 1, tym lepiej.

#$avg.silwidth	Średnia szerokość sylwetki dla wszystkich punktów.
#Powyżej 0.5 oznacza dobrą strukturę.

#$pearson.gamma	Miara zgodności odległości i przypisań do klastrów.
#Im bliżej 1, tym lepiej.

#$dunn, $dunn2	Miary separacji i zbicia klastrów. Im wyższe — tym lepiej.
#$sindex	Inna wersja wskaźnika siluetowego. Wysoka wartość = dobre klastry.
#$entropy	Entropia = niejednorodność. Niższa = lepiej.
#$ch (Calinski–Harabasz index)	Duża wartość oznacza dobrze rozdzielone 
#klastry.


#Dodatkowe dane:
#$ave.between.matrix	Średnie odległości między parami punktów z różnych klastrów
#$cwidegap	Dystanse największych luk wewnątrz klastrów
#$widestgap	Największa luka w którymkolwiek klastrze




# 2) Algorytm dzielący

# Załaduj pakiet cluster
#library(cluster)

# Załaduj dane
#data(ruspini)

# Usuń NA i wystandaryzuj
#my <- na.omit(ruspini)
#my <- scale(my)

# Oblicz macierz odległości (tu nie zawsze potrzebne, diana potrafi sama)
#my_dist <- dist(my)

#diana_result <- diana(my)
# Rysujemy dendrogram
#plot(diana_result, main = "Hierarchiczne dzielące grupowanie (diana)")

# Podział na klastry, np. 4 klastry
#group_diana <- cutree(as.hclust(diana_result), k = 4)

# Możesz też zobaczyć szczegóły
#print(diana_result)






#dodatkowa informacja
sink("stats_single.txt") #przekierowuje wyjście tekstowe
cluster.stats(data_dist,group_s)
sink()
sink("stats.txt") #drukuje do pliku txt wyniki z konsoli
cluster.stats(data_dist,group_s)
cluster.stats(data_dist,group_c)
print("hello world")
sink()


#ZADANIE 3
#Przeprowadź analizę skupień na zbiorze danych xclara z pakietu cluster
#wybraną metodą hierarchiczną z zastosowaniem różnych miar
#odległości- Euklidesową i Manhattan. 


library(cluster) 
data(xclara)
View(xclara)

#sprawdzenie braków
sum(is.na(xclara))

my_data<-data.frame(scale(xclara))

plot(my_data)

#odległości- Euklidesową 
dist_e<-dist(my_data,method="euclidean")
clust_e<-hclust(dist_e,method="ward.D") #metoda Warda

#wyswietlenie dwoch wykresów:
par(mfrow=c(1,2))
plot(clust_e)
rect.hclust(clust_e,k=3,border=2:4)
group_e<-cutree(clust_e,k=3)

#odległości- Manhattan
dist_m<-dist(my_data,method="manhattan")
clust_m<-hclust(dist_m,method="ward.D")
plot(clust_m)
rect.hclust(clust_m,k=3,border=2:4)
group_m<-cutree(clust_m,k=3)

my_data$dok<-rep(1:3,each=1000) 
#rep(1:3,1000) daje wektor 123 123 123...
par(mfrow=c(1,1))
plot(my_data$V1,my_data$V2,col=my_data$dok)
nrow(my_data)
my_data$dok2<-rep(1:3,c(900,1150,950))
plot(my_data$V1,my_data$V2,col=my_data$dok2)


table(group_m,group_e)
table(group_m,group_m)
table(group_e,group_m)



