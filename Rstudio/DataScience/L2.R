library(dplyr)
library(ggplot2)
install.packages("PogromcyDanych")
library(PogromcyDanych)

View(serialeIMDB)

serialeIMDB %>% 
  distinct(serial)

#Zad1
serialeIMDB %>% 
  filter(serial == "House M.D.") %>% 
  ggplot(aes(sezon,ocena,color= odcinek,size= glosow))+
  geom_point()+
  labs(
    x="Sezon",
    y="Ocena",
    title = "Zależność ocen od sezonu",
    color = 'numer odcinka',
    size = 'liczba głosów'
  )


#Zad2
serialeIMDB %>% 
  filter(serial == "House M.D.") %>% 
  ggplot(aes(as.numeric(as.character(odcinek)),ocena))+
  geom_point()+
  facet_wrap(~odcinek)

#Zad3
serialeIMDB %>% 
  filter(serial == "House M.D." | serial == "Game of Thrones" | serial == "Futurama" ) %>% 
  ggplot(aes(serial,ocena))+
  geom_boxplot()+
  theme_dark()

#Zad4
serialeIMDB %>% 
  filter(serial == "House M.D." | serial == "Game of Thrones" | serial == "Futurama" ) %>% 
  ggplot(aes(ocena,color=serial))+
  geom_freqpoly()

#Zad5
serialeIMDB %>% 
  filter(serial == "House M.D." | serial == "Game of Thrones" | serial == "Futurama" ) %>% 
  ggplot(aes(ocena,color=serial,fill = serial))+
  geom_density(alpha=0.3,size=1)

#Zad6
zad6<-serialeIMDB %>% 
  filter(serial == "House M.D." | serial == "Game of Thrones" | serial == "Futurama" ) %>% 
  group_by(serial,sezon) %>% 
  summarise(ilosc_odcinek = n()) %>% 
  ungroup() %>% 
  ggplot(aes(sezon,ilosc_odcinek,color = serial))+
  geom_point()

zad6+
  labs(
    title = "Liczba odcinków na sezon"
  )


#Zad7  
rys7<-serialeIMDB %>% 
  filter(glosow<=20000) %>% 
  ggplot(aes(glosow,ocena))+
  geom_point()

#install.packages("ggExtra")
#library(ggExtra)
#ggMarginal(rys7,type = "boxplot")

#Zad8

mila <-1.6093
galon <- 3.7854
mpgprzelicznik <- mila/galon
litrkm<-(1/mpgprzelicznik)*100

View(mpg)
mympg<-mpg %>% 
  mutate(zuzyte_miasto = litrkm/cty,zuzyte_autostrada = litrkm/hwy)


mympg %>% 
  ggplot(aes(displ, shape=class))+
  geom_point(aes(y=zuzyte_miasto),color='blue')+
  geom_point(aes(y=zuzyte_autostrada),color='red')+
  ylab("Zużycie paliwa")+
  scale_shape_manual(values= c(0:6))
 

#Zad9

mpg %>% 
  ggplot(aes(displ,cty))+
  geom_point()+
  facet_grid(~cyl)

mpg %>% 
  ggplot(aes(displ,hwy))+
  geom_point()+
  facet_grid(~cyl)

#Zad10  
View(mtcars)
mtcars %>% 
  ggplot(aes(wt,mpg,color=cyl,size=cyl))+
  geom_point()+
  theme_minimal()+
  labs(
    title="",x="",y=""
  )
  
#ggMarginal(zad10,type = "boxplot")
#ggMarginal(zad10,type = "histogram")
#ggMarginal(zad10,type = "density")

#Zad 11

install.packages("SmarterPoland")
library(SmarterPoland)

library(tidyr)
library(scales)

View(countries)

countries %>% 
  ggplot(aes(birth.rate,death.rate,color=continent,size=population))+
  geom_point(alpha=0.6)

#Zad12
countries %>% 
  ggplot(aes(x=continent,fill=continent))+
  geom_bar()+
  scale_fill_brewer(palette = "Accent")

#Zad13
na.omit(countries) %>% 
  ggplot(aes(birth.rate,death.rate,color=continent))+
  geom_point(alpha=0.6)+
  facet_wrap(~ continent)

#Zad14
na.omit(countries) %>% 
  ggplot(aes(birth.rate,death.rate,color=continent))+
  geom_point(data = countries[,-5],color="grey")+
  geom_point(color ="red")+
  facet_wrap(~continent)

#Zad15
install.packages("babynames")
library(babynames)

View(babynames)
my_babies<-babynames %>% 
  filter(name %in% c("Ashley", 'Amanda', 'Mary', 'Deborah', 'Dorothy', 'Betty', 'Helen',
                     'Jennifer', 'Shirley')) %>% 
  filter(sex == 'F')

my_babies %>% 
  ggplot(aes(year,n,fill=name))+
  geom_area()+
  facet_wrap(~name,scale="free")+
  scale_fill_brewer(palette = "Set3")+
  theme(legend.position = "none")+
  labs(title = "Popularity American names")

#Zad16
value <- c(rnorm(500,10,5),rnorm(500,13,1),rnorm(500,18,1),rnorm(20,25,4),rnorm(100,12,1))
name <- c(rep("A",500),rep("B",1000),rep("C",20),rep("D",100))

df<-data.frame(name,value)

df %>% 
  ggplot(aes(name,value,fill=name))+
  geom_boxplot()+
  geom_jitter(color="black",size=0.4,alpha=0.5)+
  theme(legend.position = "none")+
  labs(title="wykresy pudełkowe",x="")

#Zad17
install.packages("maps")
library(maps)
help(package='maps')

world <- map_data("world")


world %>% 
  ggplot(aes(long, lat, group=group,fill=region))+
  geom_polygon()+
  guides(fill=FALSE)+
  coord_map(projection = "ortho",orientation = c(5,170,0))

#Zad 18

View(map_data("county"))
View(maps::us.cities)

maps::us.cities %>% 
  distinct(country.etc)

ohio_map <-map_data("county") %>% 
  filter(region == "ohio") %>% 
  select(-region)

oh <- maps::us.cities %>% 
  filter(country.etc == "OH") %>% 
  select(-country.etc) %>% 
  arrange(desc(pop))


options(scipen = 999)

ohio_map %>% 
  ggplot(aes(long,lat))+
  geom_polygon(aes(group=group),fill=NA,color='grey')+
  geom_point(aes(size = pop),data=oh,color="red")+
  coord_quickmap()
  

# Zad 19
library(dplyr)
library(tidyverse)  
install.packages("tidyverse")
library(ggplot2)

hap_data <- read.csv("//dc/stud_homes/137425/Desktop/Borecki RStudio/WDVP.csv",sep = ";",dec = ",",header = TRUE)

hap_data <- hap_data %>% drop_na()

hap_data <- hap_data %>% 
  select(region = indicator, hapscore = 'world.happiness.report.score', countrycode = "ISO.Country.code")
glimpse(hap_data)

world<-map_data("world")
glimpse(world)
setdiff(world$region,hap_data$region)
setdiff(hap_data$region,world$region)

hap_data<- hap_data %>% 
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
hap_data %>% 
  mutate(row_num = row_number()) %>% 
  filter( region == "Trinidad and Tobago")
hap_data<-hap_data %>% 
  slice(rep(1:n(),times = c(rep(1,177),2,rep(1,17))))
hap_data[c(178,179),1] <- c("Trinidad","Tobago")

world_hap <- inner_join(world,hap_data,by = "region")
glimpse(world_hap)

world_hap$hapscore <- as.numeric(sub(",",".",world_hap$hapscore,fixed = TRUE)) 

world_hap %>% 
  ggplot(aes(x=long,y=lat,group = group)) +
  geom_polygon(aes(fill = hapscore))+
  scale_fill_distiller(palette = "RdBu",direction = -1)+
  coord_quickmap()+
  ggtitle("World happiness 2017")+
  theme_void()
  
