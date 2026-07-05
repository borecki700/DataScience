#Zad 1

#zad1 <- function(a,b)
#{
#  sum(a:b)
#}



zad1 <- function(a,b)
{
  if(
  !is.numeric(a) |
  !is.numeric(b)) stop("a lub b nie jest liczbą")
  
  if( a*b<=0) stop("a lub b jest ujemne")
  if(!a%%1==0 | !b%%1==0) stop("a lub b nie jest naturalną")
  
  if(a>b)
  {
    a<- a+b
    b<- a-b
    a<- a-b
    print("zamieniono argumenty")
  }
  return((a+b)*(b-a+1)*(1/2))
  
}



#Zad 2

zad2<-function(wek,ile=3)
{
  if(length(wek)<ile) stop("za krótki")
  
  #return(c("największe",sort(wek)[1:ile],"najmniejsze",sort(wek,decreasing = TRUE)[1:ile]))
  return(sort(wek)[c(1:ile,(length(wek)-ile+1):length(wek))])
}


#Zad 3

zad3<-function(a,b,c)
{
  
  boki<-sort(c(a,b,c))
  if(boki[3]<boki[1]+boki[2])
    print("trókąt istnieje")
  else
    return("trójkąt nie istnieje")
  
  if(boki[3]^2==boki[1]^2 + boki[2]^2)
    print("trójkąt prostokątny")
  if(boki[3]==boki[2]& boki[2]==boki[1])
    print("trójkąt równoboczny")
  
}

zad3(3,4,5)
zad3(6,6,6)    
zad3(1,1,sqrt(2))  
zad3(2,2,3)  
zad3(1,1,2) 

#Zad 4

srednia <- function(A)
{
  list<-numeric(0)
  for(i in 1:ncol(A))
    list[i]<-mean(A[ ,i])
  return(list)
}
M<-matrix(1:35,nrow=5,ncol=7)
srednia(M)

#Zad 6
ekstremum<-function(a,b,c)
{
  if(a==0)stop("to nie jest funkcja kwadratowa")
  if(a>0)
    print("minimum")
  else if(a<0)
    print("maximum")
  return(c(-b/(2*a),-b^2/(4*a+c)))
}

ekstremum(0,0,0)
ekstremum(1,2,3)
ekstremum(-3,2,1)

#Zad 7
suma<-0
for(i in 1:100)
{
  suma<-suma+i
}
sum(1:100)

product<-1
  for(i in 1:100)
{
    product<-product*i
}
prod(1:100)

#Zad 10

istall.packages("MASS")
library(MASS)

Pima.tr2

braki<-function()
{
  for(i in 1:ncol(Pima.tr2))
  {
    br[i]<-sum(is.na(Pima.tr2[ ,i]))
  return(br)
  }
}
braki()
