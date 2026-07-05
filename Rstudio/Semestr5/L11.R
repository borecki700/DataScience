#zad 2
library(UsingR)
rat
mean(rat)

#H0: u=120  H1: u !=120
t.test(rat,120)

#p.value < alpha  to odrzucamy H0
#p.value >= alpha  przyjmujemy H0
