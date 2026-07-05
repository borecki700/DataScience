# -----------------------------------------------------------------------------
# ROZBUDOWANY PROGRAM DO SYMULACJI MECZÓW PIŁKARSKICH
# -----------------------------------------------------------------------------

# ----------------------
# 1. KONFIGURACJA DANYCH
# ----------------------

team_stats <- list(
  "AT" = list(
    home_avg = 0.75, # Średnia liczba goli strzelonych u siebie
    away_avg = 0.55, # Średnia liczba goli strzelonych na wyjeździe
    attack = 0.8, # Siła ataku (1.0 = średnia ligowa, 1.2 = silniejszy atak)
    defense = 0.8, # Siła obrony (0.8 = słabsza obrona niż średnia)
    last_5_home = c(2,0,1,1,0), # Liczba goli w ostatnich 5 meczach u siebie
    key_players = c(TRUE, TRUE, TRUE)  # Czy grają kluczowi zawodnicy
  ),
  "Kerry" = list(
    home_avg = 0.88, 
    away_avg = 1.12,
    attack = 1.0,
    defense = 1.0,
    last_5_away = c(0,1,2,2,0),
    key_players = c(TRUE, TRUE, TRUE)
  )
)

# Czynniki zewnętrzne
match_conditions <- list(
  importance = 1.1,  # Czy mecz jest ważny? (1.0 = normalny, 1.5 = bardzo ważny)
  weather = "good",   # "good" (dobra), "rain" (deszcz), "snow" (śnieg)
  referee = "strict"  # "strict" (ostry), "normal", "lenient" (pobłażliwy)
)

# ----------------------
# 2. FUNKCJE POMOCNICZE
# ----------------------

# Oblicza średnią ważoną formę
calculate_form <- function(goals) {
  weights <- c(1.5, 1.5, 1.2, 1.1, 1) # Wagi dla ostatnich 5 meczów
  sum(goals * weights) / sum(weights) # Średnia ważona
}

# Oblicza współczynnik warunków pogodowych
get_weather_factor <- function(weather) {
  switch(weather,
         "good" = 1.0, # Brak wpływu (Domyślna wartość)
         "rain" = 0.9, # -10% goli
         "snow" = 0.8, # -20% goli
         1.0)
}

# Oblicza współczynnik sędziego
get_referee_factor <- function(referee, home_advantage = TRUE) {
  factor <- switch(referee,
                   "strict" = 0.9, #Surowy -10% goli
                   "normal" = 1.0, #neutralny (domyślny)
                   "lenient" = 1.1, #łagodny +10%goli
                   1.0)
  if (home_advantage) factor * 1.05 else factor # Gospodarz ma +5% bonus
}

# Oblicza wpływ kluczowych zawodników
get_key_players_factor <- function(key_players) {
  playing <- sum(key_players) # Ile kluczowych gra
  total <- length(key_players) # Ile jest wszystkich
  1.0 - (0.1 * (total - playing))  # -10% za każdego brakującego
}

# ----------------------
# 3. ROZBUDOWANA SYMULACJA MECZU
# ----------------------

simulate_match <- function(home_team, away_team, n_sim = 10000, conditions = match_conditions) {
  # Pobierz statystyki drużyn
  home <- team_stats[[home_team]] #Gospodarz
  away <- team_stats[[away_team]] #Gość
  
  # Oblicz aktualną formę
  home_current <- calculate_form(home$last_5_home) #Forma gospodarza
  away_current <- calculate_form(away$last_5_away) #Forma gościa
  
  # Współczynniki dodatkowe
  weather_fact <- get_weather_factor(conditions$weather)
  referee_fact_home <- get_referee_factor(conditions$referee, TRUE)
  referee_fact_away <- get_referee_factor(conditions$referee, FALSE)
  key_players_fact_home <- get_key_players_factor(home$key_players)
  key_players_fact_away <- get_key_players_factor(away$key_players)
  
  # Skorygowane średnie (uwzględniające więcej czynników)
  home_adj <- (0.6 * home$home_avg + 0.3 * home_current + 0.1 * home$attack) * 
    conditions$importance * weather_fact * referee_fact_home * key_players_fact_home
  
  away_adj <- (0.6 * away$away_avg + 0.3 * away_current + 0.1 * away$attack) * 
    conditions$importance * weather_fact * referee_fact_away * key_players_fact_away
  
  # Uwzględnij siłę obrony przeciwnika
  home_adj <- home_adj * (1.1 - away$defense/10)
  away_adj <- away_adj * (1.1 - home$defense/10)
  
  # Symulacja z rozkładem Poissona
  home_goals <- rpois(n_sim, home_adj)
  away_goals <- rpois(n_sim, away_adj)
  
  # Oblicz prawdopodobieństwa
  results <- data.frame(home = home_goals, away = away_goals)
  
  home_win <- mean(results$home > results$away) * 100
  draw <- mean(results$home == results$away) * 100
  away_win <- mean(results$home < results$away) * 100
  
  # Najczęstsze wyniki
  common_results <- sort(table(paste(results$home, "-", results$away)), decreasing = TRUE)
  top_3_results <- head(common_results, 3)
  
  # Wizualizacja
  par(mfrow = c(1, 2))
  
  # Histogram różnicy goli
  hist(results$home - results$away,
       main = paste("Różnica goli:", home_team, "vs", away_team),
       xlab = "Gole gospodarzy - gole gości",
       ylab = "Częstość",
       col = "lightblue")
  
  # Wykres prawdopodobieństw
  prob_df <- data.frame(
    outcome = factor(c("Gospodarze", "Remis", "Goście"), 
                     levels = c("Goście", "Remis", "Gospodarze")),
    probability = c(home_win, draw, away_win)
  )
  
  barplot(prob_df$probability, 
          names.arg = prob_df$outcome,
          horiz = TRUE,
          las = 1,
          main = "Prawdopodobieństwa wyniku",
          xlab = "Procent",
          col = c("red", "gray", "green"))
  
  # Zwróć szczegółowe wyniki
  list(
    home_team = home_team,
    away_team = away_team,
    adjusted_avg = c(
      home = round(home_adj, 2),
      away = round(away_adj, 2)
    ),
    probabilities = c(
      home_win = round(home_win, 1),
      draw = round(draw, 1),
      away_win = round(away_win, 1)
    ),
    top_results = top_3_results,
    simulation_count = n_sim,
    match_conditions = conditions
  )
}

# ----------------------
# 4. PRZYKŁADOWE UŻYCIE
# ----------------------

# Ustaw ziarno dla powtarzalności
set.seed(123)

# Symuluj mecz z uwzględnieniem warunków
match_result <- simulate_match(
  "AT", 
  "Kerry",
  conditions = list(
    importance = 1.0,
    weather = "good",
    referee = "normal"
  )
)

# Wyświetl szczegółowe wyniki
cat("\nSYMULACJA MECZU:", match_result$home_team, "vs", match_result$away_team, "\n")
cat("----------------------------------\n")
cat("Warunki meczu:\n")
cat("  Ważność:", match_result$match_conditions$importance, "\n")
cat("  Pogoda:", match_result$match_conditions$weather, "\n")
cat("  Sędzia:", match_result$match_conditions$referee, "\n\n")

cat("Skorygowane średnie:\n")
cat(match_result$home_team, ":", match_result$adjusted_avg["home"], "goli\n")
cat(match_result$away_team, ":", match_result$adjusted_avg["away"], "goli\n\n")

cat("Prawdopodobieństwa:\n")
cat("Wygrana gospodarzy:", match_result$probabilities["home_win"], "%\n")
cat("Remis:", match_result$probabilities["draw"], "%\n")
cat("Wygrana gości:", match_result$probabilities["away_win"], "%\n\n")

cat("Najbardziej prawdopodobne wyniki:\n")
print(match_result$top_results)

