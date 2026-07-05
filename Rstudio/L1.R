install.packages("dplyr")
library(dplyr)

dane <- read.csv("//dc/stud_homes/137425/Desktop/Borecki RStudio/titanic.csv",sep =",",dec = ".")
dane <- as_tibble()
# Zad. 1 Wybierz kolumny: ’Name’ oraz ’Fare’. Posortuj tabele malejąco po
#kolumnie ’Fare’.
dane  %>% 
  select(Name,Fare)  %>% 
  arrange(desc(Fare))
  
#Zad. 2 Znajdź osoby, które za bilet zapłaciły po między 100 a 150 funtów
dane  %>% 
  filter(between(Fare,100,150)) %>% 
  select(Fare,Name)

#Zad. 3 Ile było na statku kobiet o imieniu ’Helen’?

dane %>% 
  filter(Sex == "female", grepl("Helen",Name)) %>% 
  summarise(ile=n())

#Zad. 4 Policz, ile osób było w każdej z klas.
dane %>% 
  group_by(Pclass) %>% 
  summarise(ile=n()) %>% 
  ungroup()

#Zad. 5 Policz średni wiek osób, które przeżyły katastrofę.
dane %>%
  filter(Survived == 1) %>% 
  summarise(mean_age = mean(Age))
dane %>% 
  summarise(mean_age = mean(Age))

#Zad. 6 Znajdź najmłodszego i najstarszego pasażera.
dane %>% 
  filter(Age == min(Age) | Age == max(Age)) %>% 
  select(Age,Name)

#Zad. 7 Policz średni wiek osób, które nie przeżyły katastrofy i płynęły statkiem za darmo.
dane %>% 
  filter(Survived == 0 & Fare ==0) %>% 
  summarise(mean_age = mean(Age),ile = n())

#Zad. 8 Jaka była średnia wieku niezamężnych kobiet na statku?
dane %>% 
  filter(Sex == "female", grepl("Miss",Name)) %>% 
  summarise(mean_age = mean(Age),ile = n())
  
#Zad. 9 Policz, ile osób (w podziale na płeć) było w każdej z klas.
dane %>% 
  group_by(Pclass,Sex) %>% 
  summarise(n())
#Zad. 10 Ile było osób, które miały 18 lat i przeżyły. Jaki to był procent
#wszystkich osiemnastolatków?  
dane %>% 
  filter(Age==18) %>% 
  group_by(Survived) %>% 
  summarise(ile =n()) %>% 
  ungroup() %>% 
  mutate(proc = ile * 100 / sum(ile)) %>% 
  ungroup()

#Zad. 11 Ilu zginęło mężczyzn a ile kobiet? Jaki to był procent mężczyzn,
#a jaki kobiet?

dane %>% 
  group_by(Sex,Survived) %>% 
  summarise(ile = n()) %>% 
  ungroup() %>% 
  group_by(Sex) %>% 
  mutate(proc = ile * 100 / sum(ile)) %>% 
  
#Zad. 12 Przelicz płatność za bilet na złotówki w nowej zmiennej i ustaw
 # tę zmienną jako pierwszą.
dane %>% 
  mutate(pln = Fare *4) %>% 
  select(Fare,pln,everything())

#Zad. 13 Dla zbioru danych starwars z pakietu dplyr znajdź BMI postaci. Narysuj histogram BMI wszystkich postaci. Znajdź obserwacje odstające
#(pod względem BMI). Następnie sporządź histogram bez obserwacji odstających.

wojny <- starwars
wojny1 <- starwars %>% 
  mutate(BMI = mass/(height/100)^2)
hist(wojny1$BMI)
wojny2 <- wojny1 %>% 
  filter(BMI <=100)
hist(wojny2$BMI)

#Zad. 14 Ze zbioru danych msleep z pakietu ggplot2 wybierz podzbiór
#obejmujący tylko roślinożerców, którzy są aktywni przez co najmniej 12 godzin dziennie.

install.packages("ggplot2")
library(ggplot2)

data_frame <- msleep  

data_frame %>% 
  filter(vore == "herbi", awake >= 12)

#Zad. 15 Ze zbioru danych msleep wybierz podzbiór obejmujący roślinożerców i owadożerców.
data_frame %>% 
  filter(vore %in% c("herbi","carni")) %>% 
  summarise(ile = n())

#Zad. 16 Ze zbioru danych msleep wybierz mięsożerców ważących ponad
#50kg. Pomiń dla tego podzbioru zmienną vore posortuj obserwacje wg wagi
#wyświetl ją jako pierwszą zmienną.

data_frame %>% 
  filter(vore == "carni",bodywt >50) %>% 
  arrange(desc(bodywt)) %>% 
  select(-vore,bodywt,everything())

#Zad. 17 Na podstawie zbioru danych msleep określ ile ssaków waży ponad
#2000 kg?

data_frame %>% 
  filter(bodywt >2000) %>% 
  summarise(ile = n())
#Zad 17 + jaki procent wagi stanowi waga mózgu 
data_frame %>% 
  mutate(proc = brainwt/bodywt * 100 ) %>% 
  select(proc,name) %>% 
  arrange(proc) %>% 
  print(n=83)

#Zad. 18 W zbiorze danych msleep wyznacz wagę ssaków w gramach oraz
#w funtach w nowych kolumnach

data_frame %>% 
  mutate(gramy = bodywt * 1000, funty = bodywt * 2.2 ) %>% 
  View()

#Zad. 19 W zbiorze danych msleep utwórz nowe kolumny: procent dnia, w
#którym dany gatunek jest czuwający oraz stosunek całkowitego czasu snu do
#całkowitego czasu czuwania. Wyznacz średnie z tych zmiennych w podgrupach wg typu odżywiania, wyświetlający wyniki rosnąco względem średniego
#procentu czuwania.

data_frame %>% 
  mutate(pr_dnia = awake*100/24,stos = sleep_total/awake) %>% 
  group_by(vore) %>% 
  summarise(srednia_proc = mean(pr_dnia),srednia_stos = mean(stos)) %>% 
  arrange(srednia_proc) %>% 
  ungroup()
  

#Zad. 20 Dla zbioru danych titanic utwórz nową zmienną, która będzie
#zawierać informację, czy pasażer był starszy czy młodszy niż średnia wieku
#osób, które przeżyły lub nie przeżyły (adekwatnie do statusu tego pasażera)

install.packages("tidyverse")
library(tidyverse)

dane %>% 
  group_by(Survived) %>% 
  summarise(sr = mean(Age)) %>% 
  pull(sr) -> srednie

dane %>% 
  mutate(wiek_kat = ifelse(Survived ==0, ifelse(Age > srednie[1],"starszy","młodszy"),ifelse(Age>srednie[2],"starszy","młodszy"))) %>% 
  select(Survived,Age,wiek_kat) %>%
  filter(Age %in% c(29,30))

#Zad. 21 W zbiorze danych msleep utwórz nową kolumnę, która będzie zawierać dzielić osobniki na te,które są lżejsze niż 1 kg, ważą między 1 a 100
#kg, powyżej 100 kg, ale mniej niż tonę i powyżej tony.

data_frame %>% 
  mutate(rodzaj_wagi = ifelse(bodywt<1,"<1",ifelse(bodywt<=100,"1-100",ifelse(bodywt<=1000,"100-1000",">1000")))) %>% 
  select(name,bodywt,rodzaj_wagi) %>% 
  print(n=1000)

#Zad. 22 Usuń wiersze, w których występują braki danych dla zmiennych
#brainwt lub sleep cycle ze zbioru msleep.

install.packages("tidyverse")
library(tidyverse)

data_frame %>% 
  drop_na(brainwt,sleep_cycle) %>% 
  print(n=100)
           
#Zad. 23 Na podstawie zbioru danych msleep określ: 
#Jak czas snu poszczególnych ssaków różni się do przeciętnego czasu snu ssaków?

data_frame %>% 
  mutate(roznica = sleep_total - mean(sleep_total)) %>% 
  select(name,sleep_total,roznica) %>% 
  arrange(desc(roznica))

#Zad. 24 Znajdź gatunek o największej wadze mózgu w grupach roślinożerców, mięsożerców
#i wszystkożerców. Posortuj wyniki rosnąco. (zbiór danych msleep)

data_frame %>% 
  drop_na(brainwt) %>% 
  group_by(vore) %>% 
  summarise(max_brain = max(brainwt)) %>% 
  drop_na() %>%
  arrange(max_brain) %>% 
  pull(max_brain) -> max_brain

data_frame %>% 
  filter(vore == "herbi" & brainwt == max_brain[4] | vore == "omni"& brainwt == max_brain[3] |
        vore == "carni" & brainwt == max_brain[2] | vore == "insecti" & brainwt == max_brain[1])

#Zad. 25 Która grupa ma najwyższy średni stosunek masy ciała do masy
#mózgu: ssaki udomowione czy nieudomowione?(zbiór danych msleep)

data_frame %>%
  drop_na(conservation,brainwt,bodywt) %>%
  mutate(conser_type = ifelse(conservation== "domesticated","domesticated","nondomestiated")) %>% 
  group_by(conser_type) %>% 
  mutate(stos = bodywt/brainwt) %>%
  summarise(mean_stos= mean(stos)) %>%
  select(conser_type,mean_stos) %>% 
  print(n=100)
  
#Zad. 26 * Nadaj osobnikom rangę na podstawie wagi mózgu, najniższą
#rangę przydzielając osobnikom o najniższej wadze. Ssaki o identycznej wadze
#mózgu powinny otrzymać takie same rangi - najniższą spośród nich. Ranga
#ssaka, który wystąpił po powtórzonej wadze powinna być równa liczbie ssaków, od których ma większy mózg powiększonej o 1. 
#Ile jest niepowtórzonych
#wag mózgu? (zbiór danych msleep)
  
moje_dane <- msleep %>% 
  arrange(brainwt) %>% 
  mutate(
    flag = duplicated(brainwt), 
    rank = ifelse(!flag, row_number(), NA)
  ) %>% 
  select(name, brainwt, rank, flag)

msleep %>% 
  arrange(brainwt) %>%
  mutate(rang = min_rank(brainwt)) %>% 
  select(name, brainwt, rang) %>% 
  summarise(liczba_unikalnych =length(unique(rang)))

  
#Zad. 27 Zaimportuj do R zbiory danych uber-raw-data-... i połącz je w
#jedną ramkę danych oraz przekształć do typu tibble.

april <- read.csv("//dc/stud_homes/137425/Desktop/Borecki RStudio/uber-raw-data-apr14.csv",sep = ",")
aug <- read.csv("//dc/stud_homes/137425/Desktop/Borecki RStudio/uber-raw-data-aug14.csv",sep = ",")
jul <- read.csv("//dc/stud_homes/137425/Desktop/Borecki RStudio/uber-raw-data-jul14.csv",sep = ",")
jun <- read.csv("//dc/stud_homes/137425/Desktop/Borecki RStudio/uber-raw-data-jun14.csv",sep = ",")
may <- read.csv("//dc/stud_homes/137425/Desktop/Borecki RStudio/uber-raw-data-may14.csv",sep = ",")
sep <- read.csv("//dc/stud_homes/137425/Desktop/Borecki RStudio/uber-raw-data-sep14.csv",sep = ",")

uber <- bind_rows(april,aug,jul,jun,may,sep)
