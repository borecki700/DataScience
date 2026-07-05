library(tidyr)
library(ggplot2)
library(dplyr)

#biblioteki do testu Tukey (HSD)
install.packages("DescTools")
library(DescTools)
install.packages("rstatix")
library(rstatix)

#biblioteki do wykrsu
install.packages("emmeans")
library(emmeans)

#Zad4

med_zar <- c(74,63,55,67.4,58,53,71.4,63,60,60,55,57,72.5,68,65,56,57,56.9,55,60,49,55,60,54,48.5,55,50,56.4,68,56
)

region <- c(rep("śr",15),rep("pwś",15))
zawod <- c(rep(c("mat-inf", "biol", "fiz", "soc", "inż"),each =3))

df<-data.frame(med_zar,region,zawod)
p<-2
r<-5
l<-3

#x^{-}_{ij}
df %>% 
  group_by(zawod,region) %>% 
  summarise(mean(med_zar))

#x^{-}_{.j}
df %>% 
  group_by(region) %>% 
  summarise(mean(med_zar))

#x^{-}_{i.}
df %>% 
  group_by(zawod) %>% 
  summarise(mean(med_zar))

#x^{-}
df %>% 
  summarise(sr=mean(med_zar)) %>% 
  pull(sr) -> sr

#srednie kwadratowe odchyleń 

#q_A
df %>% 
  group_by(zawod) %>% 
  summarise(sr_i=mean(med_zar)) %>% 
  mutate(roz_A=(sr_i - sr)^2) %>% 
  summarise(q_A = sum(roz_A*l*p)) %>% 
  pull(q_A) ->q_A
#q_A/(r-1)
QA <-q_A/(r-1)

#q_B
df %>% 
  group_by(region) %>% 
  summarise(sr_j=mean(med_zar)) %>% 
  mutate(roz_B=(sr_j - sr)^2) %>% 
  summarise(q_B = sum(roz_B*l*r)) %>% 
  pull(q_B) ->q_B
#q_B/(r-1)
QB <-q_B/(p-1)

#q_AB
df %>% 
  group_by(zawod,region) %>% 
  mutate(mean_AB=mean(med_zar)) %>% 
  ungroup() %>% 
  group_by(zawod) %>% 
  mutate(mean_A=mean(med_zar)) %>% 
  ungroup() %>% 
  group_by(region) %>% 
  mutate(mean_B=mean(med_zar)) %>% 
  print(n=30) %>% 
  ungroup() %>% 
  mutate(q_AB=(mean_AB-mean_A-mean_B+sr)^2) %>% 
  summarise(q_AB=sum(q_AB)) %>% 
  pull(q_AB) -> q_AB

#q_AB/(r-1)(l-1)
QAB <-q_AB/((r-1)*(p-1))

#q_r
df %>% 
  group_by(zawod,region) %>%
  mutate(roz_R = (med_zar - mean(med_zar))^2) %>% 
  ungroup() %>% 
  summarise(qR = sum(roz_R)) %>% 
  pull(qR) -> qR
#QR
QR<-qR/(r*p*(l-1))
#q
q<-sum((med_zar-sr)^2)
  
#F
F_A <- QA/QR
F_B<- QB/QR  
F_AB<-QAB/QR

model <- aov(med_zar~zawod*region,data=df)
summary(model)

df %>% 
  ggplot(aes(zawod,med_zar,color = region)) +
  geom_boxplot()
  
TukeyHSD(model)
#fisher_test(model)
#test
#Zad 5

dane <- data.frame(wielk_zb = c(64.5,66.3,64.8,66.5,69.3,70.3,69.0,71.5,
                                69.3,67.0,66.8,67.3,70.0,69.0,71.3,72.0,
                                74.0,75.8,77.3,71.5,76.3,72.0,77.0,74.5,
                                72.0,72.5,74.0,74.5,72.5,76.8,79.0,79.8),
                   dawka = rep(rep(1:4,each = 2),times =4), #generuje to 1 razy następnie 2 itd aż gdy skoczy się jeden wiersz powtarzamy sekwencję
                   nawadnianie = rep(1:2,each=16)) #generuje to ciąg 16 razy 1 a następnie 16 razy 2

p<-2
r<-4
l<-4

#x^{-}_ij
dane %>% 
  group_by(dawka,nawadnianie) %>% 
  summarise(mean(wielk_zb))
#x^{-}_.j
dane %>% 
  group_by(nawadnianie) %>% 
  summarise(mean(wielk_zb))
#x^{-}_i.
dane %>% 
  group_by(dawka) %>% 
  summarise(mean(wielk_zb))

#x^{-}
dane %>% 
  summarise(sr=mean(wielk_zb)) %>% 
  pull(sr) -> sr

#srednie kwadratowe odchyleń 

#q_A
dane %>% 
  group_by(dawka) %>% 
  summarise(sr_i=mean(wielk_zb)) %>% 
  mutate(roz_A=(sr_i - sr)^2) %>% 
  summarise(q_A = sum(roz_A*l*p)) %>% 
  pull(q_A) ->q_A
#q_A/(r-1)
QA <-q_A/(r-1)

#q_B
dane %>% 
  group_by(nawadnianie) %>% 
  summarise(sr_j=mean(wielk_zb)) %>% 
  mutate(roz_B=(sr_j - sr)^2) %>% 
  summarise(q_B = sum(roz_B*l*r)) %>% 
  pull(q_B) ->q_B
#q_B/(r-1)
QB <-q_B/(p-1)

#q_AB
dane %>% 
  group_by(dawka,nawadnianie) %>% 
  mutate(mean_AB=mean(wielk_zb)) %>% 
  ungroup() %>% 
  group_by(dawka) %>% 
  mutate(mean_A=mean(wielk_zb)) %>% 
  ungroup() %>% 
  group_by(nawadnianie) %>% 
  mutate(mean_B=mean(wielk_zb)) %>% 
  print(n=50) %>% 
  ungroup() %>% 
  mutate(q_AB=(mean_AB-mean_A-mean_B+sr)^2) %>% 
  summarise(q_AB=sum(q_AB)) %>% 
  pull(q_AB) -> q_AB

#q_AB/(r-1)(l-1)
QAB <-q_AB/((r-1)*(p-1))

#q_r
dane %>% 
  group_by(dawka,nawadnianie) %>%
  mutate(roz_R = (wielk_zb - mean(wielk_zb))^2) %>% 
  ungroup() %>% 
  summarise(qR = sum(roz_R)) %>% 
  pull(qR) -> qR
#QR
QR<-qR/(r*p*(l-1))
#q
dane %>% 
  summarise(q=sum((wielk_zb-sr)^2)) %>% 
  pull(q) -> q

#F
F_A <- QA/QR
F_B<- QB/QR  
F_AB<-QAB/QR

#zbiory krytyczne w domu


#POPRAWIĆ WYKRES W DOMU
model <- aov(wielk_zb~dawka*nawadnianie,data=dane)
summary(model)

dane %>% 
  ggplot(aes(group=dawka,wielk_zb,color = nawadnianie)) +
  geom_boxplot()


#Zad6

dane<-data.frame(egzamin=c(46,43,50,47.5,34,36.5,36,37.5,29,27.5,25.5,30,
                          44.5,47,45.5,43.5,32,38,35,36.5,27,26.5,23,28.5,
                          45,45.5,45,47.5,37,34.5,35.5,35,27.5,27,26.5,26.5,
                          44,46.5,43,44.5,39.5,37,37.5,37,32.5,31,33,29,
                          42.5,45.5,45,43.5,37.5,40.5,38.5,37.5,31,28.5,31.5,34.5,
                          44.5,47,45.5,44,33,34,37.5,41.5,34,33,33,30),
                 grupa = rep(rep(c("B","P","S"),each=4),times = 6),
                 kurs = rep(c("T","N"),each=12*3))

p<-2
r<-3
l<-12

#x^{-}_ij
dane %>% 
  group_by(grupa,kurs) %>% 
  summarise(mean(egzamin))
#x^{-}_.j
dane %>% 
  group_by(kurs) %>% 
  summarise(mean(egzamin))
#x^{-}_i.
dane %>% 
  group_by(grupa) %>% 
  summarise(mean(egzamin))

#x^{-}
dane %>% 
  summarise(sr=mean(egzamin)) %>% 
  pull(sr) -> sr

#srednie kwadratowe odchyleń 

#q_A
dane %>% 
  group_by(grupa) %>% 
  summarise(sr_i=mean(egzamin)) %>% 
  mutate(roz_A=(sr_i - sr)^2) %>% 
  summarise(q_A = sum(roz_A*l*p)) %>% 
  pull(q_A) ->q_A
#q_A/(r-1)
QA <-q_A/(r-1)

#q_B
dane %>% 
  group_by(kurs) %>% 
  summarise(sr_j=mean(egzamin)) %>% 
  mutate(roz_B=(sr_j - sr)^2) %>% 
  summarise(q_B = sum(roz_B*l*r)) %>% 
  pull(q_B) ->q_B
#q_B/(r-1)
QB <-q_B/(p-1)

#q_AB
dane %>% 
  group_by(grupa,kurs) %>% 
  mutate(mean_AB=mean(egzamin)) %>% 
  ungroup() %>% 
  group_by(grupa) %>% 
  mutate(mean_A=mean(egzamin)) %>% 
  ungroup() %>% 
  group_by(kurs) %>% 
  mutate(mean_B=mean(egzamin)) %>% 
  print(n=72) %>% 
  ungroup() %>% 
  mutate(q_AB=(mean_AB-mean_A-mean_B+sr)^2) %>% 
  summarise(q_AB=sum(q_AB)) %>% 
  pull(q_AB) -> q_AB

#q_AB/(r-1)(l-1)
QAB <-q_AB/((r-1)*(p-1))

#q_r
dane %>% 
  group_by(grupa,kurs) %>%
  mutate(roz_R = (egzamin - mean(egzamin))^2) %>% 
  ungroup() %>% 
  summarise(qR = sum(roz_R)) %>% 
  pull(qR) -> qR
#QR
QR<-qR/(r*p*(l-1))
#q
dane %>% 
  summarise(q=sum((egzamin-sr)^2)) %>% 
  pull(q) -> q

#F
F_A <- QA/QR
F_B<- QB/QR  
F_AB<-QAB/QR

#FA NALEŻY
#FB NALEŻY
#FAB NIE NALEŻY

dane %>%
  tukey_hsd(egzamin~kurs*grupa)
dane %>% 
  

#Wykresy
model <- aes(egzamin ~ grupa*kurs, data= dane)

#Zad7

dane<-data.frame(rytm =c(162.7,154.1,137.4,159.4,188.8,148.2,206,135.4,238.3,
                        128.7,183.3,128.7,153.7,185.3,145.4,191.4,162.7,179.8,
                        114.6,109.7,126.7,158.1,193.3,136,165.3,136,200.1,129.4,
                        171.4,NA,124.4,161,NA,193.3,163.7,NA,192.7,126.4,188.2,183.4,180.6,
                        164.7,203,181.8,180.2,168.1,132.4,182.3,226.6,208.7,144.9,
                        142.6,140.9,194.5,151.3,171.7,159.6,200,156.2,130.4,176.2,
                        147.6,157.6,179.2,158.2,NA,193.6,186.5,NA,202.8,232.3,NA),
                 wiek = rep(rep(c("11-12","13-14","15-16"),each=3),times = 8),
                 plec = rep(c("DZ","CH"),each = 9*4))

dane <- dane %>% 
  drop_na()
model <- aov(rytm~wiek*plec,data = dane)
summary(model)

r<-3
p<-2
l<-11

dane %>% 
  ggplot(aes(x=wiek,y=rytm,group=plec,color=plec))+
  stat_summary(fun = "mean",
               geom = "line")+
  stat_summary(fun = "mean",
               geom = "point",size =3)+
  stat_summary(fun.data = mean_se,fun = "mean_se",
               geom = "errorbar",width =0.2)+
  labs(y="sredni rytm serca")
  

dane %>% 
  ggplot(aes(x=plec,y=rytm,group=wiek,color=wiek))+
  stat_summary(fun = "mean",
               geom = "line")+
  stat_summary(fun = "mean",
               geom = "point",size =3)+
  stat_summary(fun.data = mean_se,fun = "mean_se",
               geom = "errorbar",width =0.2)+
  labs(y="sredni rytm serca")

dane %>% 
  ggplot(aes(x=wiek,y=rytm,color=plec))+
  geom_boxplot()

dane %>% 
  group_by(plec,wiek) %>% 
  shapiro_test(rytm)
#Nie ma podstaw do odrzucenie H0 (p>0.05)

#Zad1
install.packages("datarium")
library(datarium)

View(jobsatisfaction)

jobs<-jobsatisfaction
jobs %>% 
  ggplot(aes(x=gender,y=score,color=education_level))+
  geom_boxplot()

jobs %>% 
  ggplot(aes(score))+
  geom_histogram(bins =10)+
  facet_grid(gender~education_level)

#SPRAWDZANIE WART ODSTAJĄCYCH
jobs %>% 
  group_by(gender,education_level) %>% 
  identify_outliers(score)

jobs %>% 
  levene_test(score~gender*education_level)
#Nie podstaw do odrzucenia H0 o jednorodności wariancji

jobs %>% 
  group_by(gender, education_level) %>%
  shapiro_test(score)
#Nie ma podstaw do odrzucenia H0 (p>0.05)


model <- aov(score~gender*education_level,data = jobs)
summary(model)
#Dla plci nie ma istotnej roznicy a dla pozostałych mamy 

jobs %>% 
  group_by(gender) %>% 
  tukey_hsd(score~education_level)
  
getwd()

#Zad 2
dane <- headache
dane %>% 
  ggplot(aes(x=treatment,pain_score))+
  geom_boxplot()
dane %>% 
  ggplot(aes(x=risk,pain_score))+
  geom_boxplot()
dane %>% 
  ggplot(aes(x=gender,pain_score))+
  geom_boxplot()

dane %>% 
  ggplot(aes(x=treatment,pain_score,color = risk))+
  facet_wrap(~gender)+
  geom_boxplot()

dane %>% 
  group_by(gender,risk,treatment) %>% 
  identify_outliers(pain_score)

dane<-dane %>% 
  filter(id!=57,id!=62,id!=67,id!=71)

dane %>% 
  levene_test(pain_score~risk*gender*treatment)
#p>0.05 nie ma podstaw do odrzucenia H0 czyli o jednorodnej wariancji
#H0 simga1^2 = sigma2^2 = sigma3^
#H1 simga1^2 != sigma2^2 != sigma3^2
#gdy p>alpha brak podtaw do odrzucenia H0


dane %>% 
  group_by(gender,risk,treatment) %>%
  shapiro_test(pain_score)
#p>0.05 nie ma odstaw do drzucenia H0 czyli czy pochodzą z rozkładu normalnego
#X~pain_score
#H0 X~N (gender =l,risk = k,treatment = m)
#H1 X!~N (w danej grupie podpopulacji)
#1p-value , 2p-value ,......,np-value > alpha Nie ma podstaw do odrzucenia H0
#Możemy sądzić że rozkład zmiennej siła bólu głowy w podgrypach jest normalny

model <- aov(pain_score~risk*gender*treatment,data = dane)
summary(model)
#dla risk:gender,risk:treatment,gender:treatment są istotne różnice dla pozostałch nie ma

TukeyHSD(model)

model <- aov(pain_score~risk*gender*treatment-risk:treatment,data = dane)
summary(model)

TukeyHSD(model)

#Zad3

warpbreaks
View(warpbreaks)

warpbreaks %>% 
  ggplot(aes(x=tension,breaks,color = wool))+
  geom_boxplot()+
  geom_hline(yintercept = mean(warpbreaks$breaks))

warpbreaks %>% 
  ggplot(aes(x=tension,breaks))+
  geom_boxplot()

warpbreaks %>% 
  ggplot(aes(x=wool,breaks))+
  geom_boxplot()

warpbreaks %>% 
  group_by(tension,wool) %>% 
  identify_outliers(breaks)

warpbreaks %>% 
  levene_test(breaks~wool*tension)
#p>0.01 nie ma   podstaw do odrzucenia H0 czyli o jednorodnej wariancji
#H0 simga1^2 = sigma2^2 = sigma3^
#H1 simga1^2 != sigma2^2 != sigma3^2

warpbreaks %>% 
  group_by(wool,tension) %>%
  shapiro_test(breaks)
#p>0.01 nie ma odstaw do drzucenia H0 czyli czy pochodzą z rozkładu normalnego
#X~pain_score
#H0 X~N (gender =l,risk = k,treatment = m)
#H1 X!~N (w danej grupie podpopulacji)
#1p-value , 2p-value ,......,np-value > alpha Nie ma podstaw do odrzucenia H0
#Możemy sądzić że rozkład zmiennej siła bólu głowy w podgrypach jest normalny

warp_aov <- aov(breaks~wool*tension,data = warpbreaks)
summary(warp_aov)
#dla alpha = 0.01 istotna przy tension


TukeyHSD(warp_aov)
#istotna różnica pomiędzy low a high