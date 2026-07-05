# -----------------------------------------------------------------------------
# DZIAŁAJĄCY PROGRAM DO SYMULACJI MECZÓW (PODSTAWOWY R - BRAK ZEWNĘTRZNYCH PAKIETÓW)
# -----------------------------------------------------------------------------

# ----------------------
# 1. KONFIGURACJA DANYCH (TU WPISZ SWOJE WARTOŚCI)
# ----------------------

# Średnia liczba goli drużyn (dom/wyjazd)
team_stats <- list(
  "Bayern" = list(home_avg = 2.5, away_avg = 2.0, last_5_home = c(3, 2, 4, 1, 2)),
  "Dortmund" = list(home_avg = 1.8, away_avg = 1.5, last_5_away = c(1, 1, 0, 2, 1))
)

# ----------------------
# 2. FUNKCJE POMOCNICZE
# ----------------------

# Oblicza średnią ważoną formę (ostatnie mecze mają większą wagę)
calculate_form <- function(goals) {
  weights <- c(1.5, 1.5, 1.2, 1.1, 1)  # Wagi dla ostatnich 5 meczów
  sum(goals * weights) / sum(weights)
}

# ----------------------
# 3. SYMULACJA MECZU
# ----------------------

simulate_match <- function(home_team, away_team, n_sim = 10000) {
  # Pobierz statystyki drużyn
  home <- team_stats[[home_team]]
  away <- team_stats[[away_team]]
  
  # Oblicz aktualną formę
  home_current <- calculate_form(home$last_5_home)
  away_current <- calculate_form(away$last_5_away)
  
  # Skorygowane średnie (forma + 70% siły bazowej)
  home_adj <- 0.7 * home$home_avg + 0.3 * home_current
  away_adj <- 0.7 * away$away_avg + 0.3 * away_current
  
  # Symulacja
  home_goals <- rpois(n_sim, home_adj)
  away_goals <- rpois(n_sim, away_adj)
  
  # Oblicz prawdopodobieństwa
  results <- data.frame(
    home = home_goals,
    away = away_goals
  )
  
  home_win <- mean(results$home > results$away) * 100
  draw <- mean(results$home == results$away) * 100
  away_win <- mean(results$home < results$away) * 100
  
  # Wizualizacja
  hist(
    results$home - results$away,
    main = paste(home_team, "vs", away_team),
    xlab = "Różnica goli",
    ylab = "Częstość",
    col = "lightblue"
  )
  
  # Zwróć wyniki
  list(
    home_team = home_team,
    away_team = away_team,
    home_avg = round(home_adj, 2),
    away_avg = round(away_adj, 2),
    probabilities = c(
      home_win = round(home_win, 1),
      draw = round(draw, 1),
      away_win = round(away_win, 1)
    )
  )
}

# ----------------------
# 4. PRZYKŁADOWE UŻYCIE
# ----------------------

# Ustaw ziarno dla powtarzalności
set.seed(123)

# Symuluj mecz
match_result <- simulate_match("Bayern", "Dortmund")

# Wyświetl wyniki
cat("SYMULACJA MECZU:", match_result$home_team, "vs", match_result$away_team, "\n")
cat("----------------------------------\n")
cat("Średnia przewidywanych goli:\n")
cat(match_result$home_team, ":", match_result$home_avg, "\n")
cat(match_result$away_team, ":", match_result$away_avg, "\n\n")

cat("Prawdopodobieństwa:\n")
cat("Wygrana gospodarzy:", match_result$probabilities["home_win"], "%\n")
cat("Remis:", match_result$probabilities["draw"], "%\n")
cat("Wygrana gości:", match_result$probabilities["away_win"], "%\n")

