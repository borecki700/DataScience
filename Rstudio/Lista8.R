#Zad. 1 Wykonaj symulację rzutu kostką sześcienną do gry.
# 1) Wykonaj symulację 6000 rzutów kostką. Policz, ile było poszczególnych wyników.
# 2) Oczekujemy, że będzie ich po ok. 1000. Jak blisko wyników oczekiwanych były wyniki symulacji?
# 3) Jak często możemy się spodziewać, że otrzymamy ponad 1030 jedynek?
#Wykonaj symulacje w R, aby oszacować odpowiedź.


#zad.1
#1
rzut<-sample(1:6,6000,replace=TRUE)
table(rzut)

#3
licznik<-0
for(i in 1:10000)
{
  rzut<-sample(1:6,6000,replace=TRUE)
  if(table(rzut)[1]>1030)
    licznik<-licznik+1
}
licznik


#Zad. 2 Wprowadzono następujące wyrażenie
#y=rbinom(50,25,0.4); m1=mean(y); m2=sum(y)/25; m3=sum((y-m1)^2)/50
# 1) y jest liczbą, wektorem, czy macierzą?
# 2) Jaka jest przybliżona wartość m1 i m2?
# 3) Co oszacowuje wartość m3?

#zad.2
y=rbinom(50,25,0.4) 
m1=mean(y)
m2=sum(y)/25
m3=sum((y-m1)^2)/50

#1) Czy y jest liczbą, wektorem, czy macierzą?
#Wyrażenie rbinom(50, 25, 0.4) generuje wektor 50 losowych wartości z rozkładu dwumianowego, gdzie każda wartość reprezentuje liczbę sukcesów w 25 próbach z prawdopodobieństwem sukcesu 0.4.
#Zatem: y jest wektorem.

#2) Powinno być:
m2 = sum(y) / length(y)
#Wówczas:
#m₁ = mean(y)
#m₂ = sum(y) / 50
#Wynik: m₁ ≈ m₂

#3) Wartość m3 oszacowuje nieskorygowaną wariancję próby.



#Zad. 3 Wygeneruj dwie próby 100-elementowe z rozkładu N (0, 1) i wykonaj dla 
#nich histogramy. Czy wyglądają tak samo? Dlaczego?

#zad.3
x1<-rnorm(100)
x2<-rnorm(100)
par(mfrow=c(2,1))
hist(x1,breaks = 20)
hist(x2,breaks = 20)
par(mfrow=c(1,1))

#Histogramy nie będą wyglądały identycznie, ponieważ pochodzą z dwóch różnych prób losowych. Chociaż ich kształt będzie podobny (oba będą przypominać rozkład normalny), to różnice wynikają z losowości danych w próbach.



#Zad. 4 Jaki będzie efekt następującego wywołania:
#rnorm(5,mean=0, sd=1:5)?

rnorm(5,0:4,1:5) #srednia zmienia się od 0 do 4 a odchylenie standardowe od 1 do 5



#Zad. 5 Wygeneruj 10 obserwacji z rozkładu N (0, 1). Następnie wygeneruj
#10 obserwacji z rozkładu N (180, 16)

rnorm(10)
rnorm(10,180,4)


#Zad. 6 Wygeneruj 100 liczb z rozkładu N (100, 100). 
#Ile procent z nich znajduje się w odległości co najwyżej dwóch odchyleń 
#standardowych od średniej?

x<-rnorm(100,100,10)
sum(abs(x-100)<=20)

#Celem jest obliczenie, ile procent tych liczb znajduje się w odległości co 
#najwyżej dwóch odchyleń standardowych od średniej, czyli w przedziale 
#[100−2×10,100+2×10], czyli [80,120]

#abs(x - 100) <= 20: Liczy, ile liczb w wektorze x znajduje się w przedziale
#od 80 do 120 (czyli w odległości nie większej niż 20 od średniej).
#x - 100 oblicza różnicę każdej liczby w wektorze x od średniej 100.



#Zad. 7 Znajdź takie z, że P(Z <= z) = 0.05 jeśli Z ∼ N (0, 1).

#P(Z<=z) = F(z) = 0.05  jeśli F^-1 istnieje to F^-1(F(z))=F^-1(0.05) czyli Z=Q(0.05)
qnorm(0.05)



#Zad. 8 Jakie jest pole na prawo (prawdopodobieństwo) od 1.5 w rozkładzie N (0, 4).

# P(X>1.5)= 1-P(X<=1.5)=1-F(1.5)
pnorm(1.5,0,2,lower.tail = FALSE)
#domyślnie lower.tail = TRUE to P(X<=t) jeśli lower.tail = FALSE to P(X>t)



#Zad. 9 Znajdź takie z, że P(−z <=Z <=z) = 0.05 jeśli Z ∼ N (0, 1).

#P(-z<=Z<=z)=0.05
#F(z)- F(-z)=0.05
#F(z)-(1-F(z))=0.05
#2F(z)-1=0.05
#F(z)=0.525
#Z = Q(0.525)
qnorm(0.525)



#Zad. 10 W pewnym budynku miasta znajduje się 300 lamp. Prawdopodobieństwo 
#awarii każdej lampy w ciągu doby jest stałe i zostało oszacowane
#na 0.012. Oblicz prawdopodobieństwo tego, że w ciągu doby awarii ulegnie
#więcej niż 5 lamp. Porównaj otrzymany (dokładny) wynik z przybliżeniem
#uzyskanym za pomocą rozkładu Poissona i rozkładu normalnego.

pbinom(5,300,0.012,lower.tail = FALSE)#dlatego że wyznaczamy więcej niż 5 lamp
#lub 1-pbinom(5,300,0.012)
ppois(5,300*0.012,lower.tail = FALSE) # tutaj POISONA
pnorm((5-300*0.012)/sqrt(300*0.012*0.988),lower.tail = FALSE) # tutaj Bernulli

#σ= sqrt(n×p×(1−p))


#Zad. 11 Czas pomiędzy wypadkami na pewnym skrzyżowaniu może być modelowany
#rozkładem wykładniczym ze średnią 20 dni. Oblicz prawdopodobieństwo, że 
#następny wypadek zdarzy się w ciągu najbliższych trzech tygodni.

pexp(21,1/20) # bo lambda to 1/srednia



#Zad. 12 Podziel okno graficzne na 3 wiersze i 4 kolumny. W pierwszym
#wierszu umieść wykresy normalności (kwantyl-kwantyl) dla 4 losowych prób
#o wielkości 10 z rozkładu N (0, 1). W kolejnych dwóch wierszach zrób to samo
#tylko dla wielkości prób odpowiednio 100 i 1000. Dlaczego wygląd wykresu
#zmienia się wraz z wielkością próby?

a1<-rnorm(10)
a2<-rnorm(10)
a3<-rnorm(10)
a4<-rnorm(10)
b1<-rnorm(100)
b2<-rnorm(100)
b3<-rnorm(100)
b4<-rnorm(100)
c1<-rnorm(1000)
c2<-rnorm(1000)
c3<-rnorm(1000)
c4<-rnorm(1000)
par(mfrow=c(3,4))
qqnorm(a1)
qqnorm(a2)
qqnorm(a3)
qqnorm(a4)
qqnorm(b1)
qqnorm(b2)
qqnorm(b3)
qqnorm(b4)
qqnorm(c1)
qqnorm(c2)
qqnorm(c3)
qqnorm(c4)
par(mfrow=c(1,1))


#Zad. 13 Wykonaj poprzednie zadanie dla rozkładu U(0, 1), t(1) i χ^2(1).

#U(0, 1) (rozkład jednostajny), t(1) (rozkład t-Studenta) i χ²(1) (rozkład chi-kwadrat z 1 stopniem swobody).


# Tworzenie prób z różnych rozkładów
a1_U <- runif(10, 0, 1)
a2_U <- runif(10, 0, 1)
a3_U <- runif(10, 0, 1)
a4_U <- runif(10, 0, 1)

b1_t <- rt(100, df=1)
b2_t <- rt(100, df=1)
b3_t <- rt(100, df=1)
b4_t <- rt(100, df=1)

c1_chi <- rchisq(1000, df=1)
c2_chi <- rchisq(1000, df=1)
c3_chi <- rchisq(1000, df=1)
c4_chi <- rchisq(1000, df=1)

# Podział okna graficznego na 3 wiersze i 4 kolumny
par(mfrow=c(3,4))

# Wykresy kwantyl-kwantyl dla rozkładu U(0,1) (próba o rozmiarze 10)
qqnorm(a1_U)
qqnorm(a2_U)
qqnorm(a3_U)
qqnorm(a4_U)

# Wykresy kwantyl-kwantyl dla rozkładu t(1) (próba o rozmiarze 100)
qqnorm(b1_t)
qqnorm(b2_t)
qqnorm(b3_t)
qqnorm(b4_t)

# Wykresy kwantyl-kwantyl dla rozkładu χ²(1) (próba o rozmiarze 1000)
qqnorm(c1_chi)
qqnorm(c2_chi)
qqnorm(c3_chi)
qqnorm(c4_chi)

# Przywrócenie domyślnego układu okna graficznego
par(mfrow=c(1,1))



#Zad. 14 Wykonaj histogram dla 100 losowo wygenerowanych liczb z rozkładu 
#wykładniczego ze średnią 10. Oszacuj medianę. Wynosi ona mniej, czy
#więcej niż średnia?

x<-rexp(100,1/10)
hist(x)
mean(x)
median(x)



#Zad. 15 W rozkładzie N (0, 1) chcemy wybrać jako miarę tendencji centralnej 
#miarę o mniejszej wariancji spośród: średnia, mediana. Wykonaj symulację
#(1000 prób, 100 elementów w każdej) w celu stwierdzenia, którą miarę
#powinniśmy wybrać. Powtórz symulację dla rozkładu t(2) oraz Exp(1).

list1<-numeric(0)
list2<-numeric(0)
for(i in 1:1000)
{
  x<-rnorm(100)
  list1[i]<-mean(x)
  list2[i]<-median(x)
}
var(list1)/var(list2)

for(i in 1:1000)
{
  x<-rt(100,2)
  list1[i]<-mean(x)
  list2[i]<-median(x)
}
var(list1)/var(list2)

for(i in 1:1000)
{
  x<-rexp(100,1)
  list1[i]<-mean(x)
  list2[i]<-median(x)
}
var(list1)/var(list2)


#Jeśli wynik var(list1)/var(list2) dla danego rozkładu jest mniejszy niż 1, oznacza to, że średnia ma mniejszą wariancję.
#Jeśli wynik jest większy niż 1, oznacza to, że mediana ma mniejszą wariancję.



#Zad. 16 Napisz funkcję liczącą średnią, wariancję oraz odchylenie 
#standardowe dla 1000 losowo wygenerowanych liczb z rozkładu U(0, 1). 
#Porównaj wyniki z wartościami teoretycznymi.

rozjednostajny<-function(){
  x<-runif(1000)
  return(c(mean(x),var(x),sd(x)))
}
rozjednostajny()


#Zad. 17 Napisz funkcję, która generuje 100 niezależnych obserwacji z rozkładu
#U(3.7, 5.8). Dodatkowo funkcja ma znaleźć odchylenie przeciętne. 
#Następnie zmodyfikuj ją tak, aby można podać dowolny przedział, na którym
#określony jest rozkład.

rozjedno<-function(){
  x<=runif(100,3.7,5.8)
  return(mad(x))
}
rozjedno1<-function(a,b){
  x<=runif(100,a,b)
  return(mad(x))
}
rozjedno()



#Zad. 18 Zaproponuj kreatywne zilustrowanie znanych zależności między
#rozkładami zmiennych losowych z wykorzystaniem R.

#Np. X_i ∼ N (0, 1)
#SUM X^2_i ∼ χ^2_k

chi<-numeric(0)
for(i in 1:1000){
  x<-rnorm(100,0,1)
  chi[i]<-sum(x^2)
}
y<-rchisq(1000,100)
hist(chi,probability = TRUE)
lines(density(y))

#drugi wykres
plot(density(chi))
lines(density(y),color='red')
