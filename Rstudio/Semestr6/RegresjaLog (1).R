# Przykładowe dane
data <- 
  
  # Przykład zastosowania regresji logistycznej
  model <- glm(y ~ x1 + x2, data = data, family = binomial)

# Dopasowanie modelu
fit <- glm(y ~ x1 + x2, data = data, family = binomial)

# Wyświetlenie podsumowania modelu
summary(fit)

# Przykładowe predykcje
predictions <- predict(fit, newdata = new_data, type = "response")

# Przykład oceny predykcji
confusion_matrix <- table(data$y, predictions > 0.5)
print(confusion_matrix)
# Przykład wyświetlenia krzywej ROC
library(pROC)
roc_curve <- roc(data$y, predictions)
plot(roc_curve)








# Zad. 1 Zbiór danych SAheart (South African Heart Disease) z pakietu
# bestglm zawiera dane dotyczące zapadalności na zawał serca wśród mężczyzn
# pomiędzy 15 a 64 rokiem życia. Zmienna chd oznacza, że wystąpił
# (wartość 1) lub nie wystąpił (wartość 0) zawał serca. Dokładny opis danych
# znajduje się w dokumentacji R.

# a) Dopasuj model regresji logistycznej.
# b) Które zmienne są istotne statystycznie w modelu pełnym?
# c) Oblicz iloraz szans (ang. odds ratio) w modelu logistycznym w przypadku,
# kiedy wartości wszystkich zmiennych są ustalone, natomiast zwiększamy wiek pacjenta o jeden rok.
# d) Używając metody eliminacji wstecznej z kryterium AIC oraz BIC
# dokonać selekcji zmiennych (funkcja step). Utworzyć model z zastosowaniem
# wybranych tą metodą zmiennych. Zapisz równanie uzyskanego modelu.
# e) Ocenić prawdopodobieństwo zawału serca u 46-letniego mężczyzny z
# poziomem ldl równym 6, który wypalił 10 kg tytoniu, miał w wywiadzie
# rodzinnym choroby serca i ocenę wzorca zachowań typu A równą 50.



#ZADANIE 1
# Załadowanie pakietów i danych
install.packages("bestglm")
install.packages("MASS")
library(bestglm)
library(MASS)
data(SAheart)

#a) Dopasuj model regresji logistycznej.
# Pełny model regresji logistycznej
full_model <- glm(chd ~ ., data = SAheart, family = binomial)
summary(full_model) #glm(...) – to funkcja dopasowująca 


#b) Które zmienne są istotne statystycznie w modelu pełnym?:
#istotne te które P(z)<alpha
#Zmienne istotne statystycznie (p < 0.05):
#czyli tobacco,ldl,famhist,typea,age


#c) Oblicz iloraz szans (odds ratio) dla wieku.
# Obliczenie OR dla wieku (zwiększenie o 1 rok)
exp(coef(full_model)["age"])
#Jeśli OR = 1.05, oznacza to, że każde dodatkowe 5 lat życia zwiększa szansę na zawał o 5% (przy założeniu stałych pozostałych zmiennych)



#d) Selekcja zmiennych metodą AIC i BIC + zapis równania modelu
# Eliminacja wsteczna z AIC
step_aic <- step(full_model, direction = "backward", trace = 0)
summary(step_aic)

# Eliminacja wsteczna z BIC
step_bic <- step(full_model, direction = "backward", k = log(nrow(SAheart)), trace = 0)
summary(step_bic)

# Równanie modelu AIC :
# chd ~ sbp + tobacco + ldl + famhist + typea + age
# Równanie modelu BIC 
# chd ~tobacco +ldl + famhist +typea + age


#e) Oszacuj prawdopodobieństwo zawału u konkretnego pacjenta.
# Nowa obserwacja
new_patient <- data.frame(
  sbp = mean(SAheart$sbp),  # Uśredniamy brakujące zmienne
  tobacco = 10,
  ldl = 6,
  famhist = "Present",
  typea = 50,
  age = 46
)

# Predykcja prawdopodobieństwa
prob <- predict(step_aic, newdata = new_patient, type = "response")
prob
#Prawdopodobieństwo zawału wynosi np. 0.65 (65%) dla tego pacjenta.


#ZADANIE 2 nie działa

# Instalacja i ładowanie pakietów
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("caret")) install.packages("caret")

library(ggplot2)
library(caret)

# -------------------------------------------
# Pobranie i wczytanie danych Framingham z internetu
# -------------------------------------------
url <- "https://raw.githubusercontent.com/jbrownlee/Datasets/master/framingham.csv"
framingham <- read.csv(url)

# Sprawdzenie pierwszych wierszy
head(framingham)

# -------------------------------------------
# Sprawdzenie struktury danych i nazwy zmiennych
# -------------------------------------------
str(framingham)

# -------------------------------------------
# Usuwamy wiersze z brakującymi danymi
framingham <- na.omit(framingham)

# -------------------------------------------
# Zamiana nazwy zmiennej celu na MI_FCHD
# W tym zbiorze zmienna celu to 'TenYearCHD' (1 = choroba w ciągu 10 lat)
# -------------------------------------------
framingham$MI_FCHD <- framingham$TenYearCHD

# -------------------------------------------
# a) Wykres rozrzutu zmiennych TOTCHOL i SYSBP
# -------------------------------------------
ggplot(framingham, aes(x = totChol, y = sysBP)) +
  geom_point(alpha = 0.6) +
  labs(title = "a) Wykres rozrzutu: TOTCHOL vs SYSBP",
       x = "Całkowity cholesterol (totChol)",
       y = "Ciśnienie skurczowe (sysBP)") +
  theme_minimal()

# -------------------------------------------
# b) Wykres pudełkowy DIABP w podziale względem MI_FCHD
# W zbiorze mamy zmienną diaBP
# -------------------------------------------
ggplot(framingham, aes(x = factor(MI_FCHD), y = diaBP)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "b) Wykres pudełkowy diaBP wg MI_FCHD",
       x = "Wystąpienie choroby niedokrwiennej serca (MI_FCHD)",
       y = "Ciśnienie rozkurczowe (diaBP)") +
  theme_minimal()

# -------------------------------------------
# c) Podział na zbiór treningowy (80%) i testowy (20%)
# -------------------------------------------
set.seed(123)
train_index <- createDataPartition(framingham$MI_FCHD, p = 0.8, list = FALSE)
train_data <- framingham[train_index, ]
test_data <- framingham[-train_index, ]

# -------------------------------------------
# d) Budowa modelu regresji logistycznej
# zmienne objaśniające: age, sex, cigsPerDay, totChol, sysBP, diaBP, glucose
# zmienna objaśniana: MI_FCHD
# -------------------------------------------
model <- glm(MI_FCHD ~ age + sex + cigsPerDay + totChol + sysBP + diaBP + glucose,
             data = train_data,
             family = binomial)

summary(model)

# -------------------------------------------
# e) Współczynniki modelu (równanie regresji logistycznej)
# -------------------------------------------
cat("e) Równanie modelu (logit(p)):\n")
print(coef(model))
cat("\nInterpretacja:\nlog(p/(1-p)) = b0 + b1*age + b2*sex + b3*cigsPerDay + b4*totChol + b5*sysBP + b6*diaBP + b7*glucose\n")

# -------------------------------------------
# f) Predykcja i tabela klasyfikacji na zbiorze treningowym
# -------------------------------------------
train_pred_prob <- predict(model, newdata = train_data, type = "response")
train_pred_class <- ifelse(train_pred_prob > 0.5, 1, 0)

train_table <- table(Predicted = train_pred_class, Actual = train_data$MI_FCHD)
cat("f) Tabela klasyfikacji na zbiorze treningowym:\n")
print(train_table)

train_accuracy <- sum(diag(train_table)) / sum(train_table) * 100
cat(sprintf("Dokładność klasyfikacji na zbiorze treningowym: %.2f%%\n\n", train_accuracy))

# -------------------------------------------
# g) Predykcja i tabela klasyfikacji na zbiorze testowym
# -------------------------------------------
test_pred_prob <- predict(model, newdata = test_data, type = "response")
test_pred_class <- ifelse(test_pred_prob > 0.5, 1, 0)

test_table <- table(Predicted = test_pred_class, Actual = test_data$MI_FCHD)
cat("g) Tabela klasyfikacji na zbiorze testowym:\n")
print(test_table)

test_accuracy <- sum(diag(test_table)) / sum(test_table) * 100
cat(sprintf("Dokładność klasyfikacji na zbiorze testowym: %.2f%%\n", test_accuracy))



# Zad. 3
# Na podstawie zbioru danych BreastCancer z pakietu mlbench określ,
# czy dana próbka jest łagodna czy złośliwa (Class), w oparciu o 9 innych cech komórki.
# a) Zachowaj tylko kompletne przypadki, usuń kolumnę z numerem ID.
# b) Przygotuj model zależności zmiennej Class od zmiennej Cell.shape,
#    czy w tym modelu uwzględniona jest tylko jedna zmienna objaśniająca?
# c) Przekształć kolumny od 1 do 9 do typu numerycznego.
# d) Przekształć wartości zmiennej Class z na 1 lub 0.
# e) Podziel zbiór danych na treningowy (70%) i testowy.
# f) Opracuj model logistyczny zależności zmiennej Class od grubości, rozmiaru i kształtu komórki.
# g) Przeprowadź predykcję dla danych testowych i na jej podstawie przygotuj tabelę porównawczą wartości
#    zmiennej Class oraz jej predykcji na podstawie modelu. Oblicz jaki procent danych został poprawnie zaklasyfikowany.






#ZADANIE 3

# Załadowanie pakietów i danych
install.packages(" mlbench")

library( mlbench)

data(BreastCancer)



# -------------------------------------------
# a) Zachowaj tylko kompletne przypadki, usuń kolumnę ID
# -------------------------------------------
bc <- BreastCancer
bc <- bc[complete.cases(bc), ]   # usunięcie wierszy z NA
bc$Id <- NULL                    # usunięcie kolumny Id

# Sprawdzenie danych po oczyszczeniu
str(bc)

# -------------------------------------------
# b) Model zależności Class od Cell.shape
#    Czy model ma tylko jedną zmienną objaśniającą?
# -------------------------------------------
model_b <- glm(Class ~ Cell.shape, data = bc, family = binomial)
summary(model_b)
# Tak, model ma tylko jedną zmienną objaśniającą: Cell.shape

# -------------------------------------------
# c) Przekształcenie kolumn 1 do 9 do typu numerycznego
# -------------------------------------------
for (i in 1:9) {
  bc[, i] <- as.numeric(as.character(bc[, i]))
}
str(bc)

# -------------------------------------------
# d) Przekształcenie wartości zmiennej Class na 0/1
# -------------------------------------------
# Class ma wartości "benign" i "malignant"
# zamienimy "benign" na 0, "malignant" na 1
bc$Class <- ifelse(bc$Class == "malignant", 1, 0)

table(bc$Class)  # sprawdzenie

# -------------------------------------------
# e) Podział na zbiór treningowy (70%) i testowy (30%)
# -------------------------------------------
install.packages("caret")
install.packages("stringi")
library(caret)
set.seed(123)
train_index <- createDataPartition(bc$Class, p = 0.7, list = FALSE)
train_data <- bc[train_index, ]
test_data <- bc[-train_index, ]

# -------------------------------------------
# f) Model logistyczny Class ~ Thickness + Size + Shape (kolumny 1,2,3)
#    W BreastCancer:
#    1 - Cl.thickness, 2 - Cell.size, 3 - Cell.shape
# -------------------------------------------
model_f <- glm(Class ~ Cl.thickness + Cell.size + Cell.shape,
               data = train_data,
               family = binomial)
summary(model_f)

# -------------------------------------------
# g) Predykcja dla zbioru testowego i tabela porównawcza
# -------------------------------------------
test_pred_prob <- predict(model_f, newdata = test_data, type = "response")
test_pred_class <- ifelse(test_pred_prob > 0.5, 1, 0)

test_table <- table(Predicted = test_pred_class, Actual = test_data$Class)
print(test_table)

# Obliczenie procentu poprawnej klasyfikacji
accuracy <- sum(diag(test_table)) / sum(test_table) * 100
cat(sprintf("Dokładność klasyfikacji na zbiorze testowym: %.2f%%\n", accuracy))
