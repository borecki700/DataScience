##Zad 1

# Załadowanie pakietów i danych
install.packages("bestglm")
install.packages("MASS")
library(bestglm)
library(MASS)
data(SAheart)

# Pełny model regresji logistycznej
full_model <- glm(chd ~ ., data = SAheart, family = binomial)
summary(full_model)
#istotne te które P(z)<alpha
#czyli sbp,tobacco,ldl,famhist,typea,age

# Obliczenie OR dla wieku (zwiększenie o 1 rok)
exp(coef(full_model)["age"])
#Jeśli OR = 1.05, oznacza to, że każde dodatkowe 5 lat życia zwiększa szansę na zawał o 5% (przy założeniu stałych pozostałych zmiennych)

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
