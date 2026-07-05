#Zad 1
#r=1+sin(t)
#phi=c*t

t<-seq(0,360,by=0.01)
c<-1
r<-1+sin(t)
phi<-c*t

#x=r*cos(t)
#y=r*sin(t)

x<-r*cos(phi)
y<-r*sin(t)

plot(x,y,type="l")

wykres<- function(c)
{
  t<-seq(0,360,by=0.01)
  r<-1+sin(t)
  phi<-c*t
  x<-r*cos(phi)
  y<-r*sin(t)
  
  plot(x,y,type="l")
}
wykres(1)
wykres(0.1)
wykres(2.2)


#Zad 2

curve(2*x^3+x^2+3,0,1)
p<-c(0.2,0.8)
w<-2*p^3+p^2+3

f<-function(x)
{
  2*x^3+x^2+3
}
f(0.2)
f(0.8)
points(p,w,pch=19,cex=1.5,add=TRUE)
text(p,w+0.3,paste("c",p,",",w))

#Zad 3

getwd()

iris
#Zapisuje nam w bieżącej ścieżce 

plot(iris$Sepal.Width,iris$Sepal.Length)
pdf("nazwa.pdf")
dev.off()

plot(iris$Sepal.Width,iris$Sepal.Length)
png("nazwa2.png")
dev.off()

#Zad 4

LakeHuron

plot(LakeHuron)
identify(LakeHuron,labels = time(LakeHuron))

#Zad 5

x<-seq(-pi,pi,by=0.01)
plot(x,sin(x),type="l",axes=FALSE)
axis(2,pos =0,las =2, tck=0.02)
axis(1,pos= 0, at =seq(-pi,pi,by=pi/4) , tck=0.02)
arrows(pi,0,1.05*pi,0)
arrows(0,1,0,1.05)
