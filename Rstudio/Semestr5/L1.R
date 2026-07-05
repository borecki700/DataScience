#Zad. 1 Wygeneruj liczby całkowite od 100 do 150 i zachowaj je we wektorze
# o nazwie x
  x<-100:150;
  x<-seq(100,150,1)
#Zad. 2 Wygeneruj ciąg liczb z przedziału od 0 do 1 o długości 101 i zachowaj go w wektorze o #nazwie y.
  y<-seq(0,1,1/100);
  y<-seq(0,1,length(100))
#Zad. 3 Wygeneruj liczby parzyste z przedziału 1 do 100.
  seq(2,100,2);
#Zad. 4 Wygeneruj 6 powtórzeń sekwencji liczb 2,3.
  rep(c(2,3),6);
#Zad. 5 Utwórz wektor zawierający jedną jedynkę, dwie dwójki, ... , dziewięć
#  dziewiątek.
  rep(1:9,1:9);
#  Zad. 6 Wygeneruj sekwencję zawierającą siedem jedynek, dwanaście dwójek i jedenaście trójek. Zapisz ją w macierzy o 5 wierszach i 6 kolumnach.
  m<-rep(1:3,c(7,12,11));
  matrix(m,nrow=1,ncol=6);
  #Zad. 7 Utwórz wektor 1,2,3,4,5 na trzy sposoby.
  1:5;
  rep(1:5,1)
  seq(1,5,1)
  c(1,2,3,4,5)
  #Zad. 8 Nie zapisując żadnej zmiennej wydrukuj kwadraty liczb od 1 do 10.
  (1:10)^2;
  #Zad. 9 Oblicz sumę wektorów x i y. Co możesz zaobserwować?
  x+y;
  length(x);
  length(y);
  #Zad. 10 Wydrukuj wartości wektora x, które są większe od 120.
  x[x>120];
  #Zad. 11 Wydrukuj indeksy tych wartości wektora x, które są większe od
  #120.
  which(x>120);
  #Zad. 12 Wprowadź w konsoli następujące wyrażenia:
  x>120 | x<140
  (x>120) || (x<140)
  #Zad. 13 Wydrukuj wektor x bez wyrazów od 10-tego do 20-tego 
  x[-(10:20)]
  #x[-c(10,20)]  pominięcie 10tego i 20 tego
  #Zad. 14 Dla wektora
  
  z<-c(2,4,5,9,7,1,0,3,6,8)  
  sum(z)
  length(z)
  log10(z)
  log(z)
  range(z)
  r<-range(z)
  r[2]-r[1]
  
  #Zad. 15 Oblicz:
    1:10^2
  (1:10)^2
  1:(10^2)
  1:10*2
  1:(10*2)
  1:10/2
  1:10%%3
  1:10%/%3
  1:10+1
  1:(10-1)
  
  #Zaobserwuj wyniki oraz kolejność wykonywania działań.
  #Zad. 16 Wprowadź nową zmienną
  
  l<-"2"
  is.numeric(l)  #sprawdza czy zmienna l jest numeryczna
  as.numeric(l)  #wyświetla zmienną l w postaci numerycznej
  l<-as.numeric(l)  #zmienia typ zmiennej l na numeryczny
  
  #Sprawdź, czy zmienna ma typ liczba. Jeśli nie, zmień jej typ na liczbę.
  
  #Zad. 17 Dla wektora liczb −10, −9.9, . . . , −5 znajdź ich:
    #a) wartość bezwzględną,
#b) exponentę,
#c) minimum i maximum,
#d) podłogę i sufit,
#e) zaokrąglenie.
  v<-seq(-10,10,by=0.1)
  abs(v)
  exp(v)
  min(v)
  max(v)
  floor(v)
  ceiling(v)
  round(v)
