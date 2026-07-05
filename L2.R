#Zad 1

install.packages("UsingR") #tylko przy pierwszym uzyciu pakietu na danym urzadzeniu
library(UsingR) #po uruchomieniu nower sesji w R, bez cudzysłowia

#cfb[,"INCOME"] nie stosujemy

cfb [cfb$INCOME >0 & cfb$NETWORTH <0 , ]

ramka <- cfb [cfb $ INCOME >0 & cfb $ NETWORTH <0 , ]
nrow(ramka)


#Zad 2

istall.packages("MASS")
library(MASS)

#zbiór danych od nazwy zmiennej oddzielamy $

table(Cars93$Origin,Cars93$Type)
lapply(Cars93, is.factor)


#Zad 3

mrcars6 <- mtcars[mtcars$cyl==6,]

#Zad 4
samochody <- Cars93[Cars93$Type=="Small" | Cars93$Type=="Sporty",]


#Zad 5

sort(islands,decreasing = TRUE)[1:8]

#Zad 6
#Wczytując zamieniamy z \ na /

wages <- read.table('C:/Users/Kamil/OneDrive/Pulpit/Studia SEMESTR 5/Statystyka/Statystyka (L)/Listy/wages.txt',header=TRUE)
is.data.frame(wages)

#Zad 7

wages[c(FALSE,TRUE),]
wages[seq(2,nrow(wages),by=2),]

#Zad 8

names(wages)
nchar(names(wages))

#Zad 9

daneBT <- read.csv2("http://www.biecek.pl/R/dane/daneBioTech.csv",fileEncoding = "windows-1252")

#Zad 10

write.table(daneBT[1:10,1:4],"maleDane.txt", sep = "\t", dec = ".")
getwd()


#Zad 11


ramka_danych <- data.frame(
  wzrost = c(167,170,180,200,190),
  waga = c(50,60,70,80,90),
  wiek = c(20,30,40,50,60)
)
