

# Funkcja generate_additional_data przyjmuje:
# - data: ramkę danych zawierającą przynajmniej kolumny: weight, height, age
# - indicators: listę współczynników dla poszczególnych przedziałów wiekowych edukacji oraz współczynnika obywatelstwa
generate_additional_data <- function(data, indicators) {
  
  # Oblicz liczbę wierszy (potrzebne do próbkowania)
  n <- nrow(data)
  
  # Losowanie wartości year z przedziału 1925–2025
  data$year <- sample(1925:2025, n, replace = TRUE)
  # Oblicz kolumnę BMI
  data <- data %>% 
    mutate(BMI = round(weight / ((height / 100) ^ 2), 2))
  
  # Oblicz poziom edukacji według ISCED wykorzystując rowwise do indywidualnego losowania dla każdego wiersza
  data <- data %>%
    rowwise() %>%
    mutate(
      education = {
        ind <- indicators[[country]]  # pobieramy zestaw współczynników dla kraju
        
        if (age < 3) {
          "0"  # edukacja przedszkolna – dzieci przed systemem formalnym
        } else if (age >= 3 & age <= 5) {
          sample(c("0", "None"), 1, prob = c(ind$age_indicator_kindergarten, ind$age_indicator_nokindergarten))
        } else if (age >= 6 & age <= 11) {
          sample(c("1", "None"), 1, prob = c(ind$age_indicator_primary, ind$age_indicator_noprimary))
        } else if (age >= 12 & age <= 14) {
          sample(c("2", "None"), 1, prob = c(ind$age_indicator_lowersecondary, ind$age_indicator_nolowersecondary))
        } else if (age >= 15 & age <= 17) {
          sample(c("3", "4", "None"), 1, prob = c(ind$age_indicator_uppersecondary, ind$age_indicator_postsecondary, ind$age_indicator_nouppersecondary))
        } else if (age >= 18 & age <= 20) {
          sample(c("4", "5", "6", "None"), 1, prob = c(ind$age_indicator_postsecondary, ind$age_indicator_postsecondary, ind$age_indicator_bachelor, ind$age_indicator_nohigher))
        } else if (age >= 21 & age <= 24) {
          sample(c("6", "7", "None"), 1, prob = c(ind$age_indicator_bachelor, ind$age_indicator_master, ind$age_indicator_nohigher))
        } else if (age >= 25) {
          sample(c("6", "7", "8", "None"), 1, prob = c(ind$age_indicator_bachelor, ind$age_indicator_master, ind$age_indicator_doctorate, ind$age_indicator_nohigher))
        } else {
          NA_character_
        }
      }
    ) %>%
    ungroup()
  
  
  # Dodaj kolumnę citizenship – losuje dla każdego wiersza
  n <- nrow(data)
  data <- data %>% 
    mutate(citizenship = sample(c("1", "0"), n, replace = TRUE, prob = indicators$citizenship_prob))
  
  return(data)
}