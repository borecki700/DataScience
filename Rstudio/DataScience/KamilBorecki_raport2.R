# ============================================================================
# ZADANIE 1 – Wieloczynnikowa ANOVA z transformacją 1/x
# ============================================================================

# 1) Tworzenie ramki danych z poprawną kolejnością poziomów czynników
install.packages("rlang")
library(dplyr)
library(tidyr)

raw_data <- data.frame(
  czas_przez = c(
    0.31, 0.45, 0.82, 1.1,  0.43, 0.45, 0.45, 0.71,
    0.46, 0.43, 0.88, 0.72, 0.63, 0.76, 0.66, 0.62,
    0.36, 0.29, 0.92, 0.61, 0.44, 0.35, 0.56, 1.02,
    0.4,  0.23, 0.49, 1.24, 0.31, 0.4,  0.71, 0.38,
    0.22, 0.21, 0.3,  0.37, 0.23, 0.25, 0.3,  0.36,
    0.18, 0.23, 0.38, 0.29, 0.24, 0.22, 0.31, 0.33
  ),
  dose = c(
    rep("dawka m", 16),
    rep("dawka ś", 16),
    rep("dawka d", 16)
  ),
  therapy  = rep(c("terapia 1", "terapia 2", "terapia 3", "terapia 4"),
                each = 2, times = 6)
)

#jawna kolejność poziomów czynników
dane <- raw_data %>%
  mutate(
    dose  = factor(dose,  levels = c("dawka m", "dawka ś", "dawka d")),
    therapy = factor(therapy, levels = c("terapia 1", "terapia 2",
                                         "terapia 3", "terapia 4"))
  )

# Transformacja zgodnie z zaleceniem Boxa-Coxa
dane <- dane %>%
  mutate(czas_t = 1 / czas_przez)

# ----------------------------------------------------------------------------
# Weryfikacja założeń
# ----------------------------------------------------------------------------
library(rstatix)

# Normalność rozkładu reszt (test Shapiro-Wilka w każdej komórce)
dane %>%
  group_by(dose, therapy) %>%
  shapiro_test(czas_t)

# Jednorodność wariancji (test Bartletta)
bartlett.test(czas_t ~ interaction(dose, therapy), data = dane)
# p-value = 0.5394 -> nie odrzucamy H0, wariancje równe

# ----------------------------------------------------------------------------
# Ręczne obliczenia ANOVA
# ----------------------------------------------------------------------------
# Średnie komórkowe
sr_komorki <- dane %>%
  group_by(therapy, dose) %>%
  summarise(sr_ij = mean(czas_t), .groups = "drop")

# rozróżnienie nazw ramek i kolumn
sr_terapii <- dane %>%
  group_by(therapy) %>%
  summarise(sr_terapia = mean(czas_t), .groups = "drop")

sr_dawki <- dane %>%
  group_by(dose) %>%
  summarise(sr_dawka = mean(czas_t), .groups = "drop")

sr_o <- dane %>%
  summarise(sr_o = mean(czas_t)) %>%
  pull(sr_o)

# Parametry
l <- 4   # liczba powtórzeń (zwierząt w komórce)
p <- 3   # liczba poziomów dawki
r <- 4   # liczba poziomów terapii

# Sumy kwadratów
# Czynnik A (terapia)
q_A <- sr_terapii %>%
  mutate(roz_A = (sr_terapia - sr_o)^2) %>%
  summarise(q_A = sum(roz_A * l * p)) %>%
  pull(q_A)

# Czynnik B (dawka)
q_B <- sr_dawki %>%
  mutate(roz_B = (sr_dawka - sr_o)^2) %>%
  summarise(q_B = sum(roz_B * l * r)) %>%
  pull(q_B)

# Interakcja A×B
q_AB <- dane %>%
  group_by(therapy, dose) %>%
  mutate(mean_AB = mean(czas_t)) %>%
  ungroup() %>%
  group_by(therapy) %>%
  mutate(mean_A = mean(czas_t)) %>%
  ungroup() %>%
  group_by(dose) %>%
  mutate(mean_B = mean(czas_t)) %>%
  ungroup() %>%
  mutate(roz_AB = (mean_AB - mean_A - mean_B + sr_o)^2) %>%
  summarise(q_AB = sum(roz_AB)) %>%
  pull(q_AB)

# Resztowa (wewnątrz grup)
q_R <- dane %>%
  group_by(therapy, dose) %>%
  mutate(mean_AB = mean(czas_t)) %>%
  ungroup() %>%
  mutate(roz_R = (czas_t - mean_AB)^2) %>%
  summarise(qR = sum(roz_R)) %>%
  pull(qR)

# Całkowita
q_total <- dane %>%
  mutate(roz_q = (czas_t - sr_o)^2) %>%
  summarise(q = sum(roz_q)) %>%
  pull(q)

# Stopnie swobody
df_A  <- r - 1
df_B  <- p - 1
df_AB <- (r - 1) * (p - 1)
df_R  <- r * p * (l - 1)
df_total <- r * p * l - 1

# Średnie kwadraty
MS_A  <- q_A / df_A
MS_B  <- q_B / df_B
MS_AB <- q_AB / df_AB
MS_R  <- q_R / df_R

# Statystyki F
F_A  <- MS_A / MS_R
F_B  <- MS_B / MS_R
F_AB <- MS_AB / MS_R

# Wartości krytyczne (α = 0.01)
alpha <- 0.01
Fkr_A  <- qf(1 - alpha, df_A,  df_R)
Fkr_B  <- qf(1 - alpha, df_B,  df_R)
Fkr_AB <- qf(1 - alpha, df_AB, df_R)

# Porównanie z wartościami krytycznymi
# F_A > Fkr_A -> odrzucamy H0 dla terapii
# F_B > Fkr_B -> odrzucamy H0 dla dawki
# F_AB < Fkr_AB -> nie odrzucamy H0 dla interakcji

print(list(SKA = q_A, SKB = q_B, SKAB = q_AB, SKR = q_R, SKcał = q_total,
           dfA = df_A, dfB = df_B, dfAB = df_AB, dfR = df_R,
           MSA = MS_A, MSB = MS_B, MSAB = MS_AB, MSR = MS_R,
           FA = F_A, FB = F_B, FAB = F_AB,
           FkrA = Fkr_A, FkrB = Fkr_B, FkrAB = Fkr_AB))

# ----------------------------------------------------------------------------
# Gotowa funkcja aov() – potwierdzenie wyników
# ----------------------------------------------------------------------------
model <- aov(czas_t ~ therapy * dose, data = dane)
summary(model)

# ----------------------------------------------------------------------------
# Wykresy pudełkowe
# ----------------------------------------------------------------------------
library(ggplot2)

# Wpływ jednoczesny dwóch czynników
dane %>%
  ggplot(aes(dose, czas_t, color = therapy)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3,
               position = position_dodge(width = 0.75), color = "black") +
  labs(title = "Wpływ dawki i terapii na czas_t",
       x = "Dawka", y = "czas_t")

# Wpływ dawki (osobno)
dane %>%
  ggplot(aes(dose, czas_t, color = dose)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  labs(title = "Wpływ dawki na czas_t", x = "Dawka", y = "czas_t")

# Wpływ terapii (osobno)
dane %>%
  ggplot(aes(therapy, czas_t, color = therapy)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  labs(title = "Wpływ terapii na czas_t", x = "Terapia", y = "czas_t")

# ----------------------------------------------------------------------------
# Porównania wielokrotne Tukeya (α = 0.01)
# ----------------------------------------------------------------------------
TukeyHSD(model, which = "dose",   conf.level = 0.99)
TukeyHSD(model, which = "therapy", conf.level = 0.99)

# Wnioski:
# - Dawka i terapia mają istotny wpływ na czas_t.
# - Istotne różnice między dawkami: d vs m oraz d vs ś.
# - Istotne różnice między terapiami: 1–2, 1–4, 2–3, 3–4.
# - Interakcja nieistotna.


# ============================================================================
# ZADANIE 2 – Wizualizacja danych z BDL
# ============================================================================
install.packages("bdl")
library(bdl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(sf)

options(bdl.api_private_key = "53b87015-217b-41a9-1703-08dec0941bf2")
options(OutDec = ",")   # opcjonalnie, może wpływać na zapis liczb

# ----------------------------------------------------------------------------
# 2.1 Liczba mieszkań oddanych do użytkowania (2020–2024)
# ----------------------------------------------------------------------------

opole   <- search_units(name = "Opole",   level = "6")
krakow  <- search_units(name = "Kraków",  level = "6")
wroclaw <- search_units(name = "Wrocław", level = "6")

miasta <- bind_rows(
  opole   %>% filter(name == "Opole")   %>% slice(1),
  krakow  %>% filter(name == "Kraków")  %>% slice(1),
  wroclaw %>% filter(name == "Wrocław") %>% slice(1)
)

# Pobranie danych (zmienna: P3824 – mieszkania oddane)
# ID zmiennej dla ogółu: 748601
l_mieszkan <- get_data_by_unit(
  unitId = miasta$id,
  varId = "748601",
  year = 2020:2024
)

dane_l_mieszkan <- l_mieszkan %>%
  select(year, starts_with("val_")) %>%
  pivot_longer(
    cols = -year,
    names_to = "kod_miasta",
    values_to = "liczba_mieszkan"
  ) %>%
  mutate(id = str_remove(kod_miasta, "val_")) %>%
  left_join(miasta %>% select(id, miasto = name), by = "id")

ggplot(dane_l_mieszkan, aes(x = factor(year), y = liczba_mieszkan, fill = miasto)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Liczba mieszkań oddanych do użytkowania ogółem w latach 2020-2024",
    x = "Rok", y = "Liczba mieszkań", fill = "Miasto"
  ) +
  theme_minimal()

# ----------------------------------------------------------------------------
# 2.2 Urodzenia i zgony na 1000 ludności (2020–2024)
# ----------------------------------------------------------------------------
# Zmienna: P3428 – Urodzenia (ID 450540) i Zgony (ID 450541)
dane_urodzenia <- get_data_by_unit(
  unitId = miasta$id,
  varId = 450540,
  year = 2020:2024
)

dane_zgony <- get_data_by_unit(
  unitId = miasta$id,
  varId = 450541,
  year = 2020:2024
)

# Przekształcenie do wspólnej struktury
dane_urodzenia <- dane_urodzenia %>%
  select(year, starts_with("val_")) %>%
  pivot_longer(cols = -year, names_to = "kod_miasta", values_to = "wartosc") %>%
  mutate(
    id = str_remove(kod_miasta, "val_"),
    wskaznik = "Urodzenia żywe na 1000 ludności"
  ) %>%
  left_join(miasta %>% select(id, miasto = name), by = "id")

dane_zgony <- dane_zgony %>%
  select(year, starts_with("val_")) %>%
  pivot_longer(cols = -year, names_to = "kod_miasta", values_to = "wartosc") %>%
  mutate(
    id = str_remove(kod_miasta, "val_"),
    wskaznik = "Zgony na 1000 ludności"
  ) %>%
  left_join(miasta %>% select(id, miasto = name), by = "id")

dane_u_zg <- bind_rows(dane_urodzenia, dane_zgony)

ggplot(dane_u_zg, aes(
  x = year,
  y = wartosc,
  color = miasto,
  linetype = wskaznik,
  group = interaction(miasto, wskaznik)
)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Urodzenia żywe i zgony na 1000 ludności w latach 2020-2024",
    x = "Rok", y = "Wartość na 1000 ludności",
    color = "Miasto", linetype = "Wskaźnik"
  ) +
  theme_minimal()

# ----------------------------------------------------------------------------
# 2.3 Mapa przeciętnego wynagrodzenia brutto w powiatach (2024)
# ----------------------------------------------------------------------------
# Wczytanie mapy powiatów (plik .shp)
pow <- read_sf("C:/Users/Kamil/OneDrive/Pulpit/DataScience/RStudio/A02_Granice_powiatow.shp")

pow <- pow %>%
  mutate(kod_pow = str_pad(as.character(JPT_KOD_JE), 4, pad = "0"))

# Usunięcie duplikatów geometrii
pow_bez_duplikatow <- pow %>%
  group_by(kod_pow) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# Pobranie danych o wynagrodzeniu (P4609)
# ID dla każdego miesiąca (lista 12 zmiennych)
zmienne_miesieczne <- c(
  1749928, 1749934, 1749940, 1749946, 1749952, 1749958,
  1749964, 1749970, 1749976, 1749982, 1749988, 1749994
)

dane_wynag <- get_data_by_variable(
  varId = zmienne_miesieczne,
  unitLevel = "5",   # powiaty
  year = 2024
)

# Funkcja czyszcząca nazwy powiatów
czysc_nazwe <- function(x) {
  x %>%
    tolower() %>%
    str_replace_all("_", " ") %>%
    str_replace_all("powiat m\\.st\\. ", "") %>%
    str_replace_all("powiat m\\. ", "") %>%
    str_replace_all("powiat ", "") %>%
    str_replace_all("m\\.st\\. ", "") %>%
    str_replace_all("m\\. ", "") %>%
    str_replace_all("st\\. warszawa", "warszawa") %>%
    str_replace_all("wałbrzych od 2013", "wałbrzych") %>%
    str_squish()
}

dane_wynag <- dane_wynag %>%
  select(id, name, year, starts_with("val_")) %>%
  pivot_longer(
    cols = starts_with("val_"),
    names_to = "miesiac",
    values_to = "wynagrodzenie"
  )

wynag_srednie <- dane_wynag %>%
  group_by(id, name) %>%
  summarise(
    wynagrodzenie_2024 = mean(wynagrodzenie, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(nazwa_pow = czysc_nazwe(name))

# Przygotowanie mapy z danymi
pow_bez_duplikatow <- pow %>%
  mutate(nazwa_pow = czysc_nazwe(JPT_NAZWA_)) %>%
  group_by(nazwa_pow) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

mapa_wynag <- pow_bez_duplikatow %>%
  left_join(wynag_srednie, by = "nazwa_pow")

ggplot(mapa_wynag) +
  geom_sf(aes(fill = wynagrodzenie_2024)) +
  labs(
    title = "Przeciętne wynagrodzenie brutto w powiatach Polski w 2024 roku",
    subtitle = "Średnia ze wszystkich miesięcy 2024 roku",
    fill = "Wynagrodzenie brutto"
  ) +
  theme_minimal()


# ============================================================================
# ZADANIE 3 – Analiza czynnikowa dla mtcars
# ============================================================================
install.packages("psych")   # jeśli nie są jeszcze zainstalowane
install.packages("dplyr")

library(psych)
library(dplyr)

data(mtcars)

#wybór TYLKO zmiennych ilościowych (ciągłych)
# Wykluczamy: cyl (dyskretna), gear (dyskretna), carb (dyskretna),
#             vs i am (binarne) – zgodnie z zaleceniem pozostawiamy ciągłe
mtcars_num <- mtcars %>%
  select(mpg, disp, hp, drat, wt, qsec)

# Sprawdzenie macierzy korelacji i wartości własnych
cor(mtcars_num)
eigen(cor(mtcars_num))

#  fm = "pa" (principal axis factoring)
fa_my <- fa(
  mtcars_num,
  nfactors = 2,
  rotate = "varimax",
  fm = "pa"          # metoda osi głównych
)

summary(fa_my)
print(fa_my)
