install.packages("tidyr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("plotly")
install.packages("ggExtra")
install.packages("titanic")

library(tidyr)
library(ggplot2)
library(dplyr)
library(plotly)
library(ggExtra)
library(maps)
library(titanic)

#Zad1 
mtcars
View(mtcars)

#a
mtcars %>% 
  filter(hp >100 & cyl ==6) %>% 
  select(-c(gear,carb))

#b
mtcars %>% 
  group_by(cyl,gear) %>% 
  summarise(avg=mean(mpg),med=median(mpg))

#c
mtcars %>% 
  mutate(performance = ifelse(hp>150,"high","standard"))


#Zad 2
mpg
View(mpg)

#a
mpg %>% 
  ggplot(aes(x=class,y=hwy,color = manufacturer))+
  geom_boxplot()

#b
mpg %>% 
  ggplot(aes(displ))+
  geom_histogram()+
  facet_wrap(vars(class))+
  geom_freqpoly(color = 'red')
  
#c
mpg %>% 
  ggplot(aes(cty))+
  geom_density()

#Zad 3
diamonds

#a
sizes_data <-diamonds %>% 
  select(x,y,z,color) %>% 
  pivot_longer(cols = 1:3,names_to = "kategoria",values_to = "wymiary")

sizes_data %>% 
  ggplot(aes(wymiary,fill = color))+
  geom_histogram()+
  facet_grid(cols=vars(kategoria))+
  theme_bw()


#a-poprawnie
diamonds %>% 
  ggplot(aes(cut,fill = color))+
  geom_bar()+
  theme_bw()

#b
diamonds %>% 
  ggplot(aes(x=carat,y=price,colour = clarity,alpha = 0.3))+
  geom_point()+
  facet_grid(cols = vars(cut))


#Zad4
View(who)

#a
my_who<-who %>% 
  pivot_longer(cols = 5:60,names_to = "variable", values_to = "cases") %>% 
  drop_na(cases)

#b
my_who %>% 
  separate(variable, sep="_",into = c("diagnosis","method","age group"))
#c
my_who %>% 
  pivot_wider(names_from = variable,values_from = cases)
#d
my_who %>% 
  unite(iso2,year, col = "country_year",sep = "-")

#Zad5
economics
View(economics)

#a
fig_a <-plot_ly(data =economics,x=~date,y=~pop, type = 'scatter',mode = 'lines') %>% 
  add_trace(y=~unemploy, type = 'scatter',mode = 'lines')
fig_a

#b
fig_b <-plot_ly(data = economics,x=~pce,y=~pop, type = 'scatter' , mode = 'markers',
                marker = list(color = ~unemploy),
                text = ~paste("Data", date,"\nPracofobia",unemploy))
fig_b

#c
fig_c <-plot_ly(data = economics,x=~pce , type = "histogram")
fig_c


#Zad6
wdpv <- read.csv("E:/RStudio/WDVP.csv",dec = ",",sep = ";")

wdpv <- wdpv %>% drop_na()

wdpv <- wdpv %>% 
  select(region = indicator, gini = 'GINI.index', countrycode = "ISO.Country.code")
glimpse(wdpv)

world<-map_data("world")
glimpse(world)
setdiff(world$region,wdpv$region)
setdiff(wdpv$region,world$region)

wdpv<- wdpv %>% 
  mutate(region=recode(region,
                       "United States" = "USA",
                       "United Kingdom" = "UK",
                       "Korea (Dem. People\xd5s Rep.)" = "North Korea",
                       "Korea (Rep.)" = "south Korea",
                       "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
                       "Congo (Rep.)" = "Republic of Congo",
                       "Eswatini" = "Swaziland",
                       "Cabo Verde" = "Cape Verde",
                       "Cote d'Ivoire" = "Ivory Coast",
                       "Gambia, The" = "Gambia",
                       "Macedonia" = "North Macedonia",
  ))

wdpv %>% 
  mutate(row_num = row_number()) %>% 
  filter( region == "Trinidad and Tobago")
wdpv<-wdpv %>% 
  slice(rep(1:n(),times = c(rep(1,177),2,rep(1,17))))
wdpv[c(178,179),1] <- c("Trinidad","Tobago")

world_gini <- inner_join(world,wdpv,by = "region")
glimpse(world_gini)

world_gini$gini <- as.numeric(sub(",",".",world_gini$gini,fixed = TRUE)) 

world_gini %>% 
  ggplot(aes(x=long,y=lat,group = group)) +
  geom_polygon(aes(fill = gini))+
  scale_fill_distiller(palette = "Blues",direction = -1)+
  coord_quickmap()+
  ggtitle("World Gini's 2017")+
  theme_void()

#Zad7
titanic_train

my_titanic<-titanic_train %>% 
  ggplot(aes(x=Age,y=Fare))+
  geom_point()

#ggMarginal(my_titanic,type = "density")