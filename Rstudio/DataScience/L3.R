install.packages("tidyr")
library(tidyr)
library(ggplot2)
library(dplyr)
#Zad1

df<-as_tibble(iris)
View(iris)

df %>% 
  pivot_longer(cols =-Species,names_to = "Measures",values_to = "Values") %>% 
  separate(col="Measures",into = c("Part","Measure"))


#Zad2

relig_income

relig_income %>% as_tibble() %>% 
  pivot_longer(cols = -religion,names_to = "income",values_to = "values")

#Zad3

stocks <- tibble(
  time = as.Date("2009-01-01") + 0:9,
  Stock_X = rnorm(10,0,1),
  Stock_Y = rnorm(10,0,2),
  Stock_Z = rnorm(10,0,4))

stocks<-stocks %>% 
  pivot_longer(cols=-time,names_to = "stock",values_to = "price") %>% 
  separate(col = "stock",c("delete","stock"),sep = "_") %>% 
  select(-delete) %>%
  separate(col = "time",c("rok","miesiac",'dzien'),sep = "-") %>% 
  select(-c(rok,miesiac))
  
  
  

#Zad4
tb <- read.csv("//dc/stud_homes/137425/Desktop/Borecki RStudio/tb.csv",sep = ",",stringsAsFactors = FALSE)
tb<-tb %>% 
  select(-X) %>% 
  pivot_longer(cols = -c(iso2,year), names_to = "plec_wiek",values_to = "liczba",values_drop_na = TRUE) %>% 
  separate(plec_wiek,c("plec","wiek"),sep=1)

View(tb)
glimpse(tb)

#Zad 5
weather <- read.csv("//dc/stud_homes/137425/Desktop/Borecki RStudio/weather.csv",sep = ",",dec = ".")
weather<-weather %>% 
  select(-X) %>% 
  pivot_longer(cols = d1:d31,names_to = "dzien",values_to = "temp",values_drop_na = TRUE) %>% 
  separate(dzien,c('d','dzien'),sep=1) %>% 
  select(-d) %>% 
  mutate(dzien=as.numeric(dzien)) %>% 
  pivot_wider(names_from = "element",values_from = "temp")
glimpse(weather)

weather %>% 
  ggplot(aes(x=dzien))+
  geom_point(aes(y=tmin),color = 'blue')+
  geom_point(aes(y=tmax),color = 'red')

#Zad 6
billboard <- read.csv("//dc/stud_homes/137425/Desktop/Borecki RStudio/billboard.csv",sep = ",")

billboard <- billboard %>% 
  select(-X) %>% 
  pivot_longer(cols=wk01:wk76,names_to = "week",values_to = "rank",values_drop_na = TRUE) %>% 
  separate(date.entered,c('year','month','day'),sep = "-")

glimpse(billboard)

billboard %>% 
  filter(artist.inverted=="Dr. Dre") %>% 
  filter(rank == min(rank)|rank == max(rank)) %>% 
  select(rank,track,date.entered) %>% 
  
billboard %>% 
  filter(month == "02") %>% 
  filter(year == min(rank))
  
billboard %>%
  filter(artist.inverted =="Santana") %>% 
  distinct(track)

#Zad7
View(starwars)
starwars_plus<-starwars %>% 
  replace_na(list(hair_color="unknown")) %>% 
  replace_na(list(homeworld="missing"))
glimpse(starwars)  

starwars_plus %>% 
  filter(if_any(everything(),is.na))

starwars_plus %>% 
  drop_na()
