install.packages("plotly")
library(plotly)
library(tidyr)
library(dplyr)
library(ggplot2)



#Zad1
iris

fig<-plot_ly(data = iris,x=~Petal.Length,y=~Sepal.Length)
fig

fig<-plot_ly(data = iris,x=~Petal.Length,y=~Sepal.Length,color = ~Species)
fig


#Zad2


df<-tibble(trace1 = rnorm(100,5,1),
              trace2 =rnorm(100,0,1),
              trace3 =rnorm(100,-5,1),
           x=1:100)

plot_ly(df,x=~x,y=~trace1,type = "scatter",mode='lines')

plot_ly(df,x=~x) %>% 
  add_trace(y=~trace1,type="scatter",mode='lines') %>% 
  add_trace(y=~trace2,type="scatter",mode='lines+markers') %>%
  add_trace(y=~trace2,type="scatter",mode='markers')

#Zad3
dens <- with(diamonds, tapply(price, INDEX = cut, density))
df <- data.frame(
  x = unlist(lapply(dens, "[[", "x")),
  y = unlist(lapply(dens, "[[", "y")),
  cut = rep(names(dens), each = length(dens[[1]]$x))
)

fig <- plot_ly(df, x = ~x, y = ~y, color = ~cut) 
fig <- fig %>% add_lines()

fig

#Zad4
library(PogromcyDanych)
maturaExam

fig<-maturaExam %>% ggplot(aes(punkty))+
  geom_histogram(binwidth=1)+
  facet_wrap(~przedmiot)

ggplotly(fig)

math_mean<-maturaExam %>% 
  filter(przedmiot=="matematyka") %>% 
  group_by(rok) %>% 
  summarise(srednia = mean(punkty))

ggmeanmath<-math_mean %>% 
  ggplot(aes(x=rok,y=srednia))+
  geom_point()

ggplotly(ggmeanmath)

mean_all<-maturaExam %>% 
  group_by(przedmiot,rok) %>% 
  summarise(srednia = mean(punkty))

ggmean_all <- mean_all %>% 
  ggplot(aes(x=rok,y=srednia))+
  geom_point()+
  facet_wrap(~przedmiot)

ggplotly(ggmean_all)  


#Zad5

mila <-1.6093
galon <- 3.7854
mpgprzelicznik <- mila/galon
litrkm<-(1/mpgprzelicznik)*100

View(mpg)
mympg<-mpg %>% 
  mutate(zuzyte_miasto = litrkm/cty,zuzyte_autostrada = litrkm/hwy)


fig<-mympg %>% 
  ggplot(aes(displ, shape=class,text=paste("Marka",manufacturer,"\nModel",model)))+
  geom_point(aes(y=zuzyte_miasto),color='blue')+
  geom_point(aes(y=zuzyte_autostrada),color='red')+
  ylab("Zużycie paliwa")+
  scale_shape_manual(values= c(0:6))

ggplotly(fig)
