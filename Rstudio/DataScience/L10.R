install.packages("bdl")
library(bdl)
library(dplyr)
library(tidyr)
library(ggplot2)

options(bdl.api_private_key = "53b87015-217b-41a9-1703-08dec0941bf2")

#Zad1
search_subjects("małżeństwa")
get_variables("P1872")
get_data_by_variable("58")


search_subjects("rozwody")
get_variables("P1971")
get_data_by_variable("1616553")

search_units(name="Opole")
#1 031613261000 Powiat m. Opole                  031613200000     5 2     BIERZEMY GDZIE POZIOM 5       
#2 031613261011 Opole                            031613261000     6 1              

malz_rozw_opo<-get_data_by_unit(varId =c("58","1616553"), unitId = "031613261000")
#malz_rozw_opo<-get_data_by_unit(varId ="1616553",unitId = "031613261011")


malz_rozw_opo<-malz_rozw_opo %>% 
  dplyr::select(id,year,val) %>% 
  tidyr::pivot_wider(names_from = id,values_from = val) %>% 
  rename(malz='58', rozw='1616553')

malz_rozw_opo %>% 
  ggplot(aes(x=year))+
  geom_line(aes(y=malz,group=1,col='malz'))+
  geom_point(aes(y=malz))+
  geom_line(aes(y=rozw,group=1,col='rozw'))+
  geom_point(aes(y=rozw),col=2)+
  scale_color_manual(name = "zmienna", values = c("malz"='black',"rozw"='red'))+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))

#Zad2
search_subjects("biblioteki")
get_variables("P1688")
#get_data_by_variable("1230")

search_units('Warszawa')
#id = 071412865000
search_units('Wrocław')
#id = 030210564011
search_units(name="Opole")
#id = 031613261000

bibl<-get_data_by_unit(varId = '1230',unitId = c("071412865000","030210564011","031613261000"),year = 2020:2024) %>% 
  dplyr::select(year,val_071412865000,val_030210564011,val_031613261000) %>% 
  rename(Warszawa='val_071412865000',Wroclaw='val_030210564011',Opole='val_031613261000') %>% 
  pivot_longer(cols = 2:4,names_to = 'Miasto',values_to = 'L_Biblio')

bibl %>% 
  ggplot(aes(year,L_Biblio,fill = Miasto))+
  geom_bar(stat = 'identity',position = 'dodge',width = 0.5)
  

#Zad3
search_subjects("wynagrodzenie")
#id = P4609
variables <-get_variables("P4609") %>% 
  dplyr::filter(n3 =='ogółem' & n2 == "wg miejsca zamieszkania") %>% 
  dplyr::select(id)
variables <- variables$id

search_units(name="Opolskie")
#id = 031600000000

tabelka <- get_units(parentId = "031600000000",level = '5')
units<-get_units(parentId = "031600000000",level = '5')$id
nazwy<-get_units(parentId = "031600000000",level = '5')$name

zarobki <- get_data_by_unit(unitId = units,varId = variables,year = 2024)
kolumny <- as.character(units)
kolumny <- paste0('val_', units)
kolumny

zarobki <-zarobki %>% 
  select(id,year,val_031613101000,val_031613102000,val_031613106000,val_031613107000,val_031613110000,val_031613209000,val_031613211000,val_031613261000) %>% 
  rename("Powiat brzeski"="val_031613101000","Powiat głubczycki"="val_031613102000","Powiat namysłowski"="val_031613106000","Powiat nyski"="val_031613107000",
         "Powiat prudnicki"="val_031613110000","Powiat opolski"="val_031613209000","Powiat strzelecki"="val_031613211000","Powiat m. Opole"="val_031613261000")
zarobki <-zarobki %>% 
  pivot_longer(cols = 3:10,names_to = "Powiat",values_to = "Zarobki")

zarobki<-zarobki %>% 
  mutate(Powiat=gsub("Powiat","",Powiat))

zarobki %>% 
  ggplot(aes(x=Powiat,y=Zarobki))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))

#Zad5
search_subjects("bezrobocia")
#P2392
search_subjects("małżeństwa")
get_variables("P2392")
#60270
get_data_by_variable("60270")

unitIDS<-search_units(level = "2",name = "")$id
nazwy <-search_units(level = "2",name = "")$name
bezrobocie <- get_data_by_unit(varId = "60270", unitId = unitIDS)

bezrobocie <- bezrobocie %>% 
  select(year, val_011200000000:val_071400000000)

names(bezrobocie)[-1] <- nazwy

bezrobocie <- bezrobocie %>% 
  pivot_longer(
    -year,
    names_to = "wojewodztwo",
    values_to = "stopa"
  )


factor(bezrobocie$woj)

bezrobocie %>% 
  ggplot(aes(stopa))+
  geom_histogram()+
  facet_wrap(~wojewodztwo)

#Zad6
View(search_subjects("dochody"))
#P1566
get_variables("P1566")
#6455 
View(search_subjects("wydatki"))
#P1528
get_variables("P1528")
#3550 

unitIDS2024<-search_units(level = "2",name = "",year = 2024)$id
nazwy2024 <-search_units(level = "2",name = "",year = 2024)$name

dochody_wydatki <- get_data_by_unit(varId  = c("6455","3550"),unitId = unitIDS2024,year = 2024)

dochody_wydatki <- dochody_wydatki %>% 
  select(id,val_011200000000:val_071400000000)

names(dochody_wydatki)[-1] <- nazwy2024

wynik<-dochody_wydatki %>% 
  pivot_longer(-id,names_to = "woj",
               values_to = "val") %>% 
  mutate(wiersz=row_number(),.by=id) %>% 
  pivot_wider(names_from = "id",
              values_from = "val") %>% 
  dplyr::select(-wiersz) %>% 
  rename("dochody"="6455","wydatki"="3550")

wynik %>% 
  ggplot(aes(dochody/10^9,wydatki/10^9))+
  geom_point()+
  geom_smooth(method = "lm",se=FALSE)+
  geom_text(aes(label = tolower(woj)),
            hjust=0,vjust=0)+
  xlab("Dochody w mld")+
  ylab("Wydatki w mld")+
  ggtitle("Zależność wydatków od dochodu")

#Zad7
install.packages("sf")
library(sf)
install.packages("classInt")
library(classInt)
install.packages("RCOlorBrewer")
library(RColorBrewer)
options(OutDec=",")
install.packages("stringr")
library(stringr)

woj<-read_sf("//dc/stud_homes/137425/Desktop/Borecki RStudio//A01_Granice_wojewodztw.shp")
plot(woj$geometry)


unitIDS<-search_units(level = "2",name = "",year = "2023")$id
nazwy <-search_units(level = "2",name = "",year = "2023")$name
bezrobocie2023 <- get_data_by_unit(varId = "60270", unitId = unitIDS,year = "2023")

bezrobocie2023<-bezrobocie2023 %>% 
  dplyr::select(id,year,val_011200000000:val_071400000000)

x<-as_data_frame(bezrobocie2023)
x<-x %>% 
  pivot_longer(-c(id,year),names_to = "woj",values_to = "Bezrobocie_2023")
x<-x %>% 
  select(-year)

dane <- x %>% 
  mutate(woj = str_sub(woj,7,8))

mapa<-left_join(woj,dane,by=c("JPT_KOD_JE","woj"))

ggplot(mapa)