#Zad. 1 Lekarz ocenia poziom metabolizmu u 40-letniego mężczyzny. Wiadomo, że 
#średnio wynosi on 70mg (na 100 mm), z odchyleniem standardowym wynoszącym 2. 
#Pomiar wykazał 80 mg. Czy na poziomie istotności 0.05 można uznać go za 
#podwyższony? Zastosuj test z.

# Parametry
x_bar <- 80  # pomiar
mu_0 <- 70   # hipoteza zerowa
sigma <- 2   # odchylenie standardowe
n <- 1       # liczba prób

# Obliczanie statystyki z
z_stat <- (x_bar - mu_0) / (sigma / sqrt(n))
z_stat

# Wartość krytyczna dla poziomu istotności 0.05
z_critical <- qnorm(0.95)

# Sprawdzamy, czy wartość z jest większa niż wartość krytyczna
if (z_stat > z_critical) {
  result <- "Odrzucamy hipotezę zerową"
} else {
  result <- "Nie odrzucamy hipotezy zerowej"
}

result


#Zad.2 Dla zbioru danych rat z pakietu UsingR, zawierającego czasy przeżycia 
#szczurów poddanych promieniowaniu, wykonaj test t (µ = 120). Dobierz 
#odpowiednią alternatywę.

install.packages("UsingR")
library(UsingR)
data(rat)
t.test(rat, mu = 120, alternative="greater")


#Zad. 3 Lekarz specjalizujący się w chorobach genetycznych uważa, że na
#pewną chorobę genetyczną zapada 2/3 mężczyzn i 1/3 kobiet. Wybrał on
#300 osób chorych i zaobserwował, że 140 z nich to kobiety. Czy na poziomie
#istotności 0.01, wyniki te potwierdzają jego przypuszczenia? Zastosuj test χ^2


#Obliczenia:
#Oczekiwane liczby mężczyzn i kobiet:
#Oczekiwana liczba mężczyzn: 300 × 2/3= 200
#Oczekiwana liczba kobiet: 300 × 1/3= 100

# Liczba osób
observed <- c(140, 160)  # Obserwowane liczby kobiet i mężczyzn
expected <- c(100, 200)  # Oczekiwane liczby kobiet i mężczyzn

# Test χ^2
chisq_test <- chisq.test(observed, p = expected / sum(expected), rescale.p = TRUE)
chisq_test


#Zad. 4 Wykonano badania dotyczące proporcji ryb występujących w pewnej okolicy.
#Odnotowano liczbę papugoryb - 53, graników - 22 i pokolców - 49. 
#Wcześniejsze badania wykazały, że populacje te występowały w stosunku
#5:3:4. Czy obecny stan różni się od zaobserwowanego? Zastosuj test χ^2

# Liczba ryb
observed <- c(53, 22, 49)  # Obserwowane liczby (papugoryby, graniki, pokolce)
expected <- c(124 * 5/12, 124 * 3/12, 124 * 4/12)  # Oczekiwane liczby

# Test χ^2
chisq_test <- chisq.test(observed, p = expected / sum(expected), rescale.p = TRUE)
chisq_test


#Zad. 5 W doświadczeniu polegającym na krzyżowaniu dwóch gatunków ryb, 
#z których jeden charakteryzuje się dobrym smakiem, ale niedużą wagą, 
#natomiast drugi dużą wagą, ale nieco gorszym smakiem uzyskano wyniki:
     #Smaczne Niesmaczne
#Duże 315       101   
#Małe 108       32
#Według teoretycznych rozważań genetyków prawdopodobieństwo wystąpienia 
#wymienionych rodzajów powinno być 9:3:3:1. Na poziomie istotności 5%
#zweryfikować hipotezę, że eksperyment potwierdził rozważania teoretyczne.
#Zastosuj test χ^2


# Dane obserwowane
observed <- c(315, 101, 108, 32)

# Oczekiwane liczby na podstawie proporcji teoretycznych
expected <- c((9/16) * 556, (3/16) * 556, (3/16) * 556, (1/16) * 556)

# Test chi-kwadrat
chisq.test(observed, p = expected / sum(expected), rescale.p = TRUE)


#Wynik testu:
#Jeśli wartość p z testu chi-kwadrat jest mniejsza niż 0.05 (poziom istotności 5%), to odrzucamy hipotezę zerową i stwierdzamy, że wyniki eksperymentu nie pasują do teoretycznych rozważań.
#Jeśli wartość p jest większa niż 0.05, to nie odrzucamy hipotezy zerowej i wyniki eksperymentu mogą być zgodne z teoretycznymi założeniami.
