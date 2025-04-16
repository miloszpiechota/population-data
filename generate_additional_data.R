

# Funkcja generate_additional_data przyjmuje:
# - data: ramkę danych zawierającą przynajmniej kolumny: weight, height, age
# - indicators: listę współczynników dla poszczególnych przedziałów wiekowych edukacji oraz współczynnika obywatelstwa
generate_additional_data <- function(data, indicators) {
  
  # Oblicz kolumnę BMI
  data <- data %>% 
    mutate(BMI = round(weight / ((height / 100) ^ 2), 2))
  
  # Oblicz poziom edukacji według ISCED wykorzystując rowwise do indywidualnego losowania dla każdego wiersza
  data <- data %>% 
    rowwise() %>% 
    mutate(education = case_when(
      age < 3 ~ "0",   # przedział: <3 lata
      age >= 3 & age <= 5 ~ 
        sample(c("0", "None"), 1, 
               prob = c(indicators$age_indicator_kindergarten, indicators$age_indicator_nokindergarten)),
      age >= 6 & age <= 11 ~ 
        sample(c("1", "None"), 1, 
               prob = c(indicators$age_indicator_primary, indicators$age_indicator_noprimary)),
      age >= 12 & age <= 15 ~ 
        sample(c("2", "None"), 1, 
               prob = c(indicators$age_indicator_lowersecondary, indicators$age_indicator_nolowersecondary)),
      age >= 16 & age <= 18 ~ 
        sample(c("3", "4", "None"), 1, 
               prob = c(indicators$age_indicator_uppersecondary, indicators$age_indicator_postsecondary,
                        indicators$age_indicator_nouppersecondary)),
      age >= 19 & age <= 20 ~ 
        sample(c("4", "6", "None"), 1, 
               prob = c(indicators$age_indicator_postsecondary, indicators$age_indicator_bachelor,
                        indicators$age_indicator_nohigher)),
      age >= 21 ~ 
        sample(c("6", "7", "8", "None"), 1, 
               prob = c(indicators$age_indicator_bachelor, indicators$age_indicator_master, 
                        indicators$age_indicator_doctorate, indicators$age_indicator_nohigher)),
      TRUE ~ NA_character_
    )) %>% 
    ungroup()
  
  # Dodaj kolumnę citizenship – losuje dla każdego wiersza
  n <- nrow(data)
  data <- data %>% 
    mutate(citizenship = sample(c("1", "0"), n, replace = TRUE, prob = indicators$citizenship_prob))
  
  return(data)
}