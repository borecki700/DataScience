#Zad. 1 Narysuj histogram oraz wykres pudełkowy dla zbioru danych DDT z
#pakietu MASS. Na ich podstawie określ wartość średniej i odchylenia standardowego 
#dla tych danych. Sprawdź swoje przewidywania. Zaproponuj sposób
#zaznaczenia na histogramie średniej i odchylenia standardowego.

#zad.1
install.packages("MASS")
library(MASS)
boxplot(DDT)
hist(DDT)
#rozstęp(bez obs.odst.)~=1
#średnia za pomocy "mean"
mean(DDT)
#ODCHYLENIE standardowe "sd"
sd(DDT)
abline(v=mean(DDT))
abline(v=mean(DDT)-sd(DDT))
abline(v=mean(DDT)+sd(DDT))
arrows(mean(DDT),0,mean(DDT)+sd(DDT),0)
arrows(mean(DDT),0,mean(DDT)-sd(DDT),0)


#Zad. 2 Odnotowywano liczbę przejechanych kilometrów (spisywano z licznika)
#przy każdym tankowaniu. Zapisano następujące wartości:
#65 311, 65 624, 65 908, 66 219, 66 499, 66 821, 67 145, 67 447.
#Za pomocą funkcji diff znajdź maksymalną, minimalną oraz średnią odległość 
#przejechaną na jednym tankowaniu.

#zad.2
licznik<-c(65311, 65624, 65908, 66219, 66499, 66821, 67145, 67447)
odleglosc<-diff(licznik) #Funkcja diff() oblicza różnice pomiędzy kolejnymi 
#elementami wektora. W tym przypadku, obliczamy różnice w odległości
#przejechanej na każdym tankowaniu.
summary(odleglosc)
#Funkcja summary() daje podstawowe statystyki opisowe: minimum, 
#pierwszy kwartyl, mediana, średnia, trzeci kwartyl i maksimum. 

min(odleglosc)
max(odleglosc)
mean(odleglosc)


#Zad.3 Jan zapisywał czas swojego dojazdu do pracy (w min) przez ostatnie 10 dni:
#17,16,20,24,222,15,211,15,17,22.
#Oblicz najkrótszy, najdłuższy oraz średni czas dojazdu. Wartości powyżej 30
#min są błędne (cyfrę jedności wprowadzono dwukrotnie). Popraw te wartości.
#Znajdź średnią, minimalną i maksymalną wartość po poprawieniu danych. Ile
#razy dojazd do pracy zabrał mniej niż 20 min? Jaki procent dojazdów zabrał
#mniej niż 17 min?

#zad.3
czas<-c(17, 16, 20, 24, 222, 15, 211, 15, 17, 22)
summary(czas)
czas[which(czas>30)]<-c(22,21)

#czas[which(czas > 30)] <- c(22, 21):
#which(czas > 30) zwraca indeksy, gdzie czas dojazdu przekracza 30 minut.
#czas[...] <- c(22, 21): Podstawiamy poprawione wartości (22 i 21 minut) na te
#indeksy.

summary(czas)
sum(czas<20) 
#wek.log T(na dole 1) F(na dole zapisano 0)
sum(czas<17)/length(czas)*100


#Zad.4 Przeprowadzono ankietę, w której zapytano studentów jednego kierunku
#mieszkających w tym samym akademiku, ile czasu zajmuje im dotarcie
#z akademika na uczelnię. Uzyskano następujące odpowiedzi:
#9, 6, 9, 9, 6, 8, 5, 5, 8, 5, 5, 7, 8, 9, 8, 13, 12, 14, 15, 13, 13, 13, 16, 15, 15,
#13, 15, 15, 15, 15, 15, 16, 14, 12, 16, 12, 13, 16, 14, 12, 15, 14, 15, 16, 13, 19,
#20, 21, 21, 18, 21, 20, 22, 18, 21, 20, 21, 22, 21, 22, 20, 19, 19, 22, 21, 18, 20,
#19, 19, 22, 21, 21, 19, 18, 21, 20, 20, 18, 19, 19, 21, 19, 22, 20, 20.
#Narysuj histogram dla tych danych. Co ciekawego można zaobserwować i z
#czego to może wynikać? Czy możesz znaleźć kryterium według którego można
#podzielić te dane?

#zad.4
dotarcie<-c(9, 6, 9, 9, 6, 8, 5, 5, 8, 5, 5, 7, 8, 9, 8, 13, 12, 14, 15, 13, 13, 13, 16, 15, 15,
            13, 15, 15, 15, 15, 15, 16, 14, 12, 16, 12, 13, 16, 14, 12, 15, 14, 15, 16, 13, 19,
            20, 21, 21, 18, 21, 20, 22, 18, 21, 20, 21, 22, 21, 22, 20, 19, 19, 22, 21, 18, 20,
            19, 19, 22, 21, 21, 19, 18, 21, 20, 20, 18, 19, 19, 21, 19, 22, 20, 20)
hist(dotarcie)

dotarcie_sposob1<-dotarcie[dotarcie<10]
hist(dotarcie_sposob1)
dotarcie_sposob2<-dotarcie[dotarcie>10 & dotarcie<17]
hist(dotarcie_sposob2)
dotarcie_sposob3<-dotarcie[dotarcie>=17]
hist(dotarcie_sposob3)


#Zad.5 Wykonaj histogram i wykres pudełkowy dla danych aid, crime oraz
#south z pakietu UsingR. Wygeneruj wszystkie histogramy w jednym obszarze
#wyświetlania, a wykresy pudełkowe w drugim. Który z tych zbiorów danych
#jest symetryczny,a który asymetryczny? W którym z nich istnieją obserwacje
#odstające? Zaznacz na wykresach średnie i mediany.

#zad.5
library (UsingR)
par(mfrow=c(2,2))
hist(aid)
abline(v=mean(aid), col="red", lwd=2)  # Średnia
abline(v=median(aid), col="green", lwd=2)  # Mediana
hist(crime$y1983)
abline(v=mean(crime$y1983), col="red", lwd=2)
abline(v=median(crime$y1983), col="green", lwd=2)
hist(crime$y1993)
abline(v=mean(crime$y1993), col="red", lwd=2)
abline(v=median(crime$y1993), col="green", lwd=2)
hist(south)
abline(v=mean(south), col="red", lwd=2)
abline(v=median(south), col="green", lwd=2)

boxplot(aid)
abline(h=mean(aid), col="red", lwd=2)
abline(h=median(aid), col="green", lwd=2)
boxplot(crime$y1983)
abline(h=mean(crime$y1983), col="red", lwd=2)
abline(h=median(crime$y1983), col="green", lwd=2)

boxplot(crime$y1993)
abline(h=mean(crime$y1993), col="red", lwd=2)
abline(h=median(crime$y1993), col="green", lwd=2)
boxplot(south)
abline(h=mean(south), col="red", lwd=2)
abline(h=median(south), col="green", lwd=2)
par(mfrow=c(1,1))


#Zad.6 Dla zbiorów danych bumpers, firstchi oraz math z pakietu UsingR
#wykonaj histogram. Na jego podstawie spróbuj przewidzieć wartość średniej,
#mediany i odchylenia standardowego. Sprawdź swoje przewidywania za pomocą 
#obliczeń.

#zad.6
library(UsingR)
hist(bumpers)
mean(bumpers)
median(bumpers)
sd(bumpers)
summary(bumpers)

hist(firstchi)
mean(firstchi)
summary(firstchi)

hist(math)
mean(math)
summary(math)


#Zad.7 W pakiecie RcmdrPlugin.IPSUR znaduje się zbiór danych RcmdrTestDrive
#ze zmienną reduction. Dla tej zmiennej wyznacz: wszystkie statystyki 
#porządkowe i zapisz je w nowym wektorze, 115-tą wartość (co do wielkości),
#rozstęp międzykwartylowy, skośność, kurtozę.

#zad.7
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



#Zad.8 Opisz dane ze zbioru slc z pakietu UsingR. 
#Jaka miara jest odpowiednia do zbadania tendencji centralnej?

#zad.8
library(UsingR)
hist(slc) #r. asymetryczny
summary(slc)
#lepsza miara-mediana
median(slc)


#Zad.9  Załącz zbiór danych possum z pakietu DAAG. Dla samic z tego zbioru
#policz medianę, średnią oraz średnią uciętą (10%) całkowitej długości ciała
#(zmienna totlngth). Kiedy średnia ucięta istotnie różni się od średniej 
#arytmetycznej?

#zad.9
install.packages("DAAG")
library(DAAG)
possum
dlg_samic<-possum$totlngth[possum$sex=="f"]
mean(dlg_samic,na.rm=TRUE)
mean(dlg_samic,trim=0.1)#obciecie po 10% obserwacji najmniejszej: 10% najwiekszych
median(dlg_samic)

#Średnia ucięta obliczana jest za pomocą argumentu trim = 0.1 w funkcji mean()
#na.rm = TRUE – oznacza, że funkcja ignoruje brakujące dane (NA) i wykonuje obliczenia tylko na dostępnych wartościach.
  


