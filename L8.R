
#zad 1
rzut<-sample(1:6,6000,replace = TRUE)
table(rzut)

for(i in 1:10000)
{
  licznik<-0
  rzut
  if(table(rzut)[1]>1030)
  {
    licznik<-licznik+1
  }
}

#zad 3

x1<-rnorm(100)
x2<-rnorm(100)
par(mfrow=c(2,1))
hist(x1,20)
hist(x2,20)
par(mfrow=c(1,1))

#zad 4
rnorm(5,0:4,1:5) #srednia zmienia się od 0 do 4 a odchylenie standardowe od 1 do 5

#zad 5
rnorm(10)
rnorm(10,180,4)

#zad 6
x<-rnorm(100,100,10)
sum(abs(x-100)<=20)

#zad 7
#P(Z<=z) = F(z) = 0.05  jeśli F^_1 istnieje to F^-1(F(z))=F^-1(0.05) czyli Z=Q(0.05)
qnorm(0.05)

#zad 8 
# P(X>1.5)= 1-P(X<=1.5)=1-F(1.5)
pnorm(1.5,0,2,lower.tail = FALSE)
#domyślnie lower.tail = TRUE to P(X<=t) jeśli lower.tail = FALSE to P(X>t)

#zad 9
#P(-z<=Z<-z)=0.05
#F(z)- F(-z)=0.05
#F(z)-(1-F(z))=0.05
#2F(z)-1=0.05
#F(z)=0.525
#Z = Q(0.525)
qnorm(0.525)

#zad 10
pbinom(5,300,0.012,lower.tail = FALSE)#dlatego że wyznaczamy więcej niż 5 lamp
#lub 1-pbinom(5,300,0.012)
ppois(5,300*0.012,lower.tail = FALSE) # tutaj POISONA
pnorm((5-300*0.012)/sqrt(300*0.012*0.988),lower.tail = FALSE) # tutaj Bernulli

#zad 11
pexp(21,1/20) # bo lambda to 1/srednia

#zad 12
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
#Zad 13 kurczę nie mam

#zad 14
x<-rexp(100,1/10)
hist(x)
mean(x)
median(x)

#zad 15
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

#zad 16
rozjednostajny<-function(){
  x<-runif(1000)
  return(c(mean(x),var(x),sd(x)))
}
rozjednostajny()

#zad 17
rozjedno<-function(){
  x<=runif(100,3.7,5.8)
  return(mad(x))
}
rozjedno1<-function(a,b){
  x<=runif(100,a,b)
  return(mad(x))
}
rozjedno()

#zad 18
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
