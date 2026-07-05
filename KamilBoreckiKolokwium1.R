#Zad 1

x<-seq(-7,13,1)
x[1:3]
x[which(x>3)]
x^3
y<-sum(x)

#Zad 2

a<-seq(1,10,0.03)

#a)
log10(a)
log(a)
#b)
exp(a)
#c)
min(a)
max(a)
#d)
floor(a)
ceiling(a)
#e)
round(a)

#Zad 3

b<-seq(-10,10,1)
#a)
abs(b)
#b)
sum(b)
#c)
length(b)
#d)
r<-range(b)
r[2] - r[1]

#Zad 4
daneBT <- read.csv2("http://www.biecek.pl/R/dane/daneBioTech.csv",fileEncoding = "windows-1252")
ramka <- daneBT [daneBT$Wiek > 50, ]

#Zad 5

c<- ToothGrowth
c1<-c[c$supp=="VC", ]
write.table(c1,"swinki.txt", sep = "\t", dec = ".")
getwd()

#Zad 6
zad6<-function(wek)
{
  if(is.vector(wek)==TRUE & length(wek)<2)
  {
    stop("wektor nie posiada drugiego i przedostatniego wyrazu lub nie jest wektorem")
  }
  else if (is.vector(wek)==TRUE & length(wek)>=2)
  {
    c(wek[2],wek[length(wek)-1])
  }
  
}
zad6(c(1:1))
zad6(c(1:2))
zad6(c(1:4))
zad6(1)

#Zad 7
for(i in 1:10)
{
  if (i%%2==0)
  {
    print(i*i)
  }
}

#Zad 8
png("moj_wykres.png")
t<-seq(0,1440,by=100)
plot(t,cos(t),type="l")
plot(t,sin(t),type="l",col="red",add=TRUE)
dev.off()
getwd()

#Zad 9
plot(mtcars$mpg,mtcars$wt)
