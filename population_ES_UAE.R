# Wczytanie wymaganych bibliotek

library(googlesheets4)
library(dplyr)
library(ggplot2)
library(ggridges)
library(tidyverse)

# Generowanie danych: 1000 obserwacji
n <- 1000

# Autoryzacja
#gs4_auth()

# Funkcja generująca dane Hiszpanii
es_database <- function(n, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  # Generacja roku pomiaru z przedziału 1925-2025
  year <- sample(1925:2025, n, replace = TRUE)
  
  # Generacja płci - przyjmujemy prawdopodobieństwo: mężczyźni 0.49, kobiety 0.51
  sex <- sample(c(1, 0), n, replace = TRUE, prob = c(0.49, 0.51))
  
  # Generacja wzrostu:
  height <- ifelse(sex == 1,
                   rnorm(n, mean = 173, sd = 7),
                   rnorm(n, mean = 161, sd = 7))
  
  # Generacja wagi:
  weight <- ifelse(sex == 0,
                   rnorm(n, mean = 77.5, sd = 10),
                   rnorm(n, mean = 67.5, sd = 10))
  
  # Zaokrąglenie wzrostu i wagi do dwóch miejsc po przecinku
  height <- round(height, 2)
  weight <- round(weight, 2)
  
  # Obliczenie BMI
  bmi <- round(weight / (height / 100)^2, 2)
  
  # Generacja wieku (przyjmujemy przedział 1-100 lat)
  age <- sample(1:100, n, replace = TRUE)
  
  # Generacja wykształcenia w zależności od wieku
  education <- ifelse(age < 3, "Brak",
                      ifelse(age >= 3 & age <= 5, 
                             sample(c("Przedszkole", "Brak"), n, replace = TRUE, 
                                    prob = c(0.9, 0.1)),
                             ifelse(age >= 6 & age <= 11, 
                                    sample(c("Podstawowe", "Brak"), n, replace = TRUE,
                                           prob = c(0.9, 0.1)),
                                    ifelse(age >= 12 & age <= 18, 
                                           sample(c("Średnie", "Brak"), n, replace = TRUE,
                                                  prob = c(0.8, 0.2)),
                                           ifelse(age > 18, 
                                                  sample(c("Tylko przedszkole", "Tylko podstawowe", "Tylko średnie", "Wyższe"), n, replace = TRUE,
                                                         prob = c(0.1, 0.2, 0.3, 0.4)), NA)))))
  
  # Generacja obywatelstwa: 87.3% obywateli, 12.7% nie-obywateli
  citizenship <- sample(c("obywatel ES", "nie-obywatel ES"), 
                        n, replace = TRUE, prob = c(87.3, 12.7))
  
  # Tworzymy ramkę danych
  data <- data.frame(year = year,
                     sex = sex,
                     age = age,
                     height = height,
                     weight = weight,
                     bmi = bmi,
                     education = education,
                     citizenship = citizenship,
                     country = "ES")
  
  return(data)
}
# Funkcja generująca dane Zjednoczonych Emiratów Arabskich
uae_database <- function(n, seed = NULL){
  Indicator_Male <- 0.64
  Indicator_Female <- 1-Indicator_Male
  
  Boy_newborn_height <-90
  Boy_kindergarten_height <- 105
  Boy_primary_height <- 130
  Boy_secondary_height <- 160
  Adult_male_height <- 172
  
  Girl_newborn_height <-95
  Girl_kindergarten_height <- 108
  Girl_primary_height <- 120
  Girl_secondary_height <- 150
  Adult_female_height <- 160
  
  Boy_newborn_weight <-12
  Boy_kindergarten_weight <- 19    
  Boy_primary_weight <- 32        
  Boy_secondary_weight <- 55      
  Adult_male_weight <- 75   
  
  Girl_newborn_height <-13
  Girl_kindergarten_weight <- 18   
  Girl_primary_weight <- 30       
  Girl_secondary_weight <- 50     
  Adult_female_weight <- 65
  
  age_indicator_kindergarten <- 0.85
  age_indicator_nokindergarten <- 1-age_indicator_kindergarten
  age_indicator_primary <- 0.95
  age_indicator_noprimary <- 1-age_indicator_primary
  age_indicator_secondary <- 0.90
  age_indicator_nosecondary <- 1-age_indicator_secondary
  age_indicator_only_kindergarten <- 0.15
  age_indicator_only_primary <- 0.15
  age_indicator_only_secondary <- 0.30
  age_indicator_only_higher <- 1-age_indicator_only_kindergarten-age_indicator_only_primary-age_indicator_only_secondary      
  
  Religion_Islam <- 0.745
  Religion_Christian <- 0.129
  Religion_Hindu <- 0.062
  Religion_Buddhist <- 0.032
  Religion_Agnosticism <- 0.013
  Religion_Others <- 1-Religion_Islam-Religion_Christian-Religion_Hindu-Religion_Buddhist-Religion_Agnosticism
  
  ZEA_citizen <- 0.19
  ZEA_noncitizen <- 1-ZEA_citizen
  
  
  if(!is.null(seed)) set.seed(seed)
  
  eps <- rnorm(n, mean = 0, sd = 1)
  
  year <- sample(1925:2025, n, replace = TRUE)
  
  age <- sample(1:100, n, replace = TRUE)

  
  sex <- sample(c(0,1), n,replace = TRUE, prob = c(Indicator_Female, Indicator_Male))
  
  # Wzrost w zależności od wieku i płci
  height <- ifelse(sex == 1,
                   ifelse(age < 1, round(rnorm(n, mean = Boy_newborn_height, sd = 5) + eps, 2),
                   ifelse(age >= 1 & age < 6, round(rnorm(n, mean = Boy_kindergarten_height, sd = 3) + eps, 2),
                   ifelse(age >= 6 & age < 12, round(rnorm(n, mean = Boy_primary_height, sd = 3) + eps, 2),
                   ifelse(age >= 12 & age < 18, round(rnorm(n, mean = Boy_secondary_height, sd = 10) + eps, 2),
                        round(rnorm(n, mean = Adult_male_height, sd = 15) + 2*eps, 2))))),
                   ifelse(age < 1, round(rnorm(n, mean = Girl_newborn_height, sd = 5) + eps, 2),
                   ifelse(age >= 1 & age < 6, round(rnorm(n, mean = Girl_kindergarten_height, sd = 4) + eps, 2),
                   ifelse(age >= 6 & age < 12, round(rnorm(n, mean = Girl_primary_height, sd = 10) + eps, 2),
                   ifelse(age >= 12 & age < 18, round(rnorm(n, mean = Girl_secondary_height, sd = 10) + eps, 2),
                        round(rnorm(n, mean = Adult_female_height, sd = 15) + eps, 2))))))
  
  # Waga w zależności od wieku i płci
  weight <- ifelse(sex == 1,
            ifelse(age < 1, round(rnorm(n, mean = Boy_newborn_weight, sd = 2) + 3*eps, 2),
            ifelse(age >= 1 & age < 6, round(rnorm(n, mean = Boy_kindergarten_weight, sd = 3) + 3*eps, 2),
            ifelse(age >= 6 & age < 12, round(rnorm(n, mean = Boy_primary_weight, sd = 5) + 3*eps, 2),
            ifelse(age >= 12 & age < 18, round(rnorm(n, mean = Boy_secondary_weight, sd = 7) + 3*eps, 2),
                    round(rnorm(n, mean = Adult_male_weight, sd = 10) + 3*eps, 2))))),
            ifelse(age < 1, round(rnorm(n, mean = Girl_newborn_weight, sd = 2) + 3*eps, 2),
            ifelse(age >= 1 & age < 6, round(rnorm(n, mean = Girl_kindergarten_weight, sd = 3) + 3*eps, 2),
            ifelse(age >= 6 & age < 12, round(rnorm(n, mean = Girl_primary_weight, sd = 5) + 3*eps, 2),
            ifelse(age >= 12 & age < 18, round(rnorm(n, mean = Girl_secondary_weight, sd = 7) + 3*eps, 2),
                    round(rnorm(n, mean = Adult_female_weight, sd = 10) + 3*eps, 2))))))
  
  education <- ifelse(age < 3, "Brak",
               ifelse(age >= 3 & age <= 5, 
                      sample(c("Przedszkole", "Brak"), n, replace = TRUE, 
                      prob = c(age_indicator_kindergarten, age_indicator_nokindergarten)),
               ifelse(age >= 6 & age <= 11, 
                      sample(c("Podstawowe", "Brak"), n, replace = TRUE,
                      prob = c(age_indicator_primary, age_indicator_noprimary)),
               ifelse(age >= 12 & age <= 18, 
                      sample(c("Średnie", "Brak"), n, replace = TRUE,
                      prob = c(age_indicator_secondary, age_indicator_nosecondary)),
              ifelse(age > 18, 
                      sample(c("Tylko przedszkole", "Tylko podstawowe", "Tylko średnie", "Wyższe"), n, replace = TRUE,
                      prob = c(age_indicator_only_kindergarten, age_indicator_only_primary, age_indicator_only_secondary, age_indicator_only_higher)),NA)))))
  
  
  bmi <- round(weight/((height/100)^2),2)
  #religion <- sample(c("Islam", "Chrześcijaństwo", "Hinduizm", "Buddyzm", "Agnostycyzm", "Inne"), n, replace = TRUE, prob = c(Religion_Islam, Religion_Christian, Religion_Hindu, Religion_Buddhist, Religion_Agnosticism, Religion_Others))
  citizenship <- sample(c("obywatel ZEA", "nie-obywatel ZEA"), n, replace = TRUE, prob = c(ZEA_citizen, ZEA_noncitizen))
  data.frame(height = height, weight = weight, sex = sex, year = year, age = age, education = education, bmi=bmi, citizenship=citizenship, country = "ZEA")
}


# --------- GENEROWANIE DANYCH DLA HISZPANII ---------

#ES <- es_database(n, seed = 314592)

# Podgląd pierwszych kilku wierszy danych dla Hiszpanii
#head(ES)

# Zaokrąglamy wartości do 2 miejsc po przecinku
#ES$height <- round(ES$height, 2)
#ES$weight <- round(ES$weight, 2)

# Zapisanie danych do pliku CSV
#write.csv(ES, file = "ES_database.csv", row.names = FALSE)

# ID arkusza Hiszpanii
#ES_sheet_id <- "1Ri4C2_FD_mjz7fqYfR9zS5eLlD-fRD1NZQAaMohF0lk"

# Zapisanie danych do arkusza "Dane" - Do arkusza z danymi Hiszpanii
#sheet_write(ES, ss = ES_sheet_id, sheet = "Dane")
#range_write(ES_sheet_id, data = ES, sheet = "Arkusz1", col_names = TRUE)



# --------- GENEROWANIE DANYCH DLA ZEA ---------

# UAE <- uae_database(n, seed = 314592)

# Podgląd pierwszych kilku wierszy danych dla Zjednoczonych Emiratów Arabskich
# head(UAE)

# Zapisanie danych do pliku CSV
#write.csv(UAE, file = "UAE_database.csv", row.names = FALSE)

# UAE_sheet_id <- "1DC8ckcPiqi9MgmansTnKDc6VyiawesZbhgNJN-MldP0"
# sheet_write(UAE ,UAE_sheet_id, sheet = "source")

# ------------------------------------------------

ES_UAE_sheet_id<- "1MPRhaBzMiVvIzX3NQJhgDvrtRi5mVChwExsJNsNU8gI"
ES_UAE_sheet_name <- "Data_ES_ZEA"

# ----- Dodawanie utworzonych danych do wspólnego arkusza ----------

# Łączenie nowych danych 
#merged_data <- bind_rows(ES, UAE)

# Zapisanie połączonych danych do wspólnego
#range_write(ES_UAE_sheet_id, data = merged_data, sheet = ES_UAE_sheet_name, col_names = TRUE)


# ----- Czytanie danych z arkusza ----------

ES_UAE_data <- read_sheet(ES_UAE_sheet_id, sheet = ES_UAE_sheet_name)

# -----------------------------------------------------------------

# --- WYKRESY WSPÓLNE ---

# 1. Wykres zależności między wzrostem a wagą
plot(ES_UAE_data$height, ES_UAE_data$weight,
     main = "Wzrost vs Waga",
     xlab = "Wzrost (cm)",
     ylab = "Waga (kg)",
     col = ifelse(ES_UAE_data$sex == "1", "blue", "red"),
     pch = 16)
legend("topleft", legend = c("Male", "Female"), col = c("blue", "red"), pch = 16)

# 2. Macierz wykresów (pairs) dla zmiennych numerycznych: wzrost, waga, wiek i rok
pairs(ES_UAE_data[, c("height", "weight", "age", "year")],
      main = "Pary wykresów zmiennych numerycznych")

# 4. Boxplot przedstawiający rozkład wzrostu w zależności od poziomu wykształcenia
boxplot(height ~ education, data = ES_UAE_data,
        main = "Rozkład wzrostu wg wykształcenia",
        xlab = "Wykształcenie",
        ylab = "Wzrost (cm)",
        col = c("orange", "lightblue", "lightgreen", "pink"))

# Obliczanie średniej wagi dla każdej płci
avg_weight <- tapply(ES_UAE_data$weight, ES_UAE_data$sex, mean)

# Tworzenie wykresu słupkowego
barplot(avg_weight,
        main = "Średnia waga w zależności od płci",
        xlab = "Płeć",
        ylab = "Średnia waga (kg)",
        col = c("red", "blue"),
        ylim = c(0, max(avg_weight) * 1.1))
# Przekształcenie zmiennej 'sex' na format tekstowy dla lepszej czytelności
#ES_UAE_data$sex <- factor(ES_UAE_data$sex, labels = c("Kobieta", "Mężczyzna"))

# Tworzenie wykresu punktowego
plot(ES_UAE_data$sex, ES_UAE_data$weight,
     main = "Waga w zależności od płci",
     xlab = "Płeć",
     ylab = "Waga (kg)",
     col = c("red", "blue"),
     pch = 16)  # 'pch' określa symbol punktu; 16 to pełne kółko


# 5. Histogram wieku
hist(ES_UAE_data$age, breaks = 20,
     main = "Rozkład wieku",
     xlab = "Wiek",
     col = "lightgreen")

# 6. Wykres słupkowy - liczebność poszczególnych kategorii wykształcenia
education_counts <- table(ES_UAE_data$education)
barplot(education_counts,
        main = "Liczebność kategorii wykształcenia",
        xlab = "Wykształcenie",
        ylab = "Liczba obserwacji",
        col = "orange")

# **1. Histogram wieku**
ggplot(ES_UAE_data, aes(x = age, fill = country)) +
  geom_histogram(binwidth = 5, alpha = 0.7, position = "identity") +
  labs(title = "Rozkład wieku w Hiszpanii i ZEA", x = "Wiek", y = "Liczba osób") +
  theme_minimal()

# **2. Scatter plot BMI vs wiek**

ggplot(ES_UAE_data, aes(x = age, y = bmi, color = country)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Zależność BMI od wieku", x = "Wiek", y = "BMI") +
  theme_minimal()

# **3. Boxplot wzrostu według płci**
ggplot(ES_UAE_data, aes(x = as.factor(sex), y = height, fill = country)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("Kobiety", "Mężczyźni")) +
  labs(title = "Wzrost według płci i kraju", x = "Płeć", y = "Wzrost (cm)") +
  theme_minimal()

# --- WYKRESY DLA HISZPANII ---


# **4. Wykres słupkowy obywatelstwa w Hiszpanii**
ggplot(filter(ES_UAE_data, country == "ES"), aes(x = citizenship, fill = citizenship)) +
  geom_bar() +
  labs(title = "Podział obywateli i nie-obywateli w Hiszpanii", x = "Obywatelstwo", y = "Liczba osób") +
  theme_minimal()

# --- WYKRESY DLA ZEA ---

#Wykres zależności wzrostu i wagi 
ggplot(filter(ES_UAE_data, country == "ZEA"),   aes(x = height, y = weight, color = ifelse(sex == 1, "Mężczyzna", "Kobieta"))) + 
  geom_point(alpha = 0.6) +
  ggtitle("Zależność między wzrostem a wagą") + 
  labs(x = "Wzrost (cm)", y = "Waga (kg)",color = "Płeć") +
  scale_color_manual(values = c("Kobieta" = "pink", "Mężczyzna" = "blue")) +
  theme(plot.title = element_text(hjust = 0.5)) 

# Histogram rozkładu wzrostu i wagi 
ggplot(filter(ES_UAE_data, country == "ZEA"),   aes(x = height, fill = ifelse(sex == 1, "Mężczyzna", "Kobieta"))) +
  geom_histogram(position = "dodge", binwidth = 5, color = "black", alpha = 0.7) +
  ggtitle("Rozkład wzrostu w zależności od płci") +
  labs(x = "Wzrost (cm)", y = "Liczba osób", fill = "Płeć") +
  scale_fill_manual(values = c("Kobieta" = "pink", "Mężczyzna" = "blue")) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(filter(ES_UAE_data, country == "ZEA"),   aes(x = weight, fill = ifelse(sex == 1, "Mężczyzna", "Kobieta"))) +
  geom_histogram(position = "dodge", binwidth = 5, color = "black", alpha = 0.7) +
  ggtitle("Rozkład wagi w zależności od płci") +
  labs(x = "Waga (kg)", y = "Liczba osób", fill = "Płeć") +  
  scale_fill_manual(values = c("Kobieta" = "pink", "Mężczyzna" = "blue")) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Wykres poziomu edukacji w zależności od płci
ggplot(filter(ES_UAE_data, country == "ZEA"),  aes(x = education, fill = ifelse(sex == 1, "Mężczyzna", "Kobieta"))) +
  geom_bar(position = "dodge", width = 1, color = "black", alpha = 0.7) +
  ggtitle("Poziom edukacji w zależności od płci") +
  labs(x = "Poziom edukacji", y = "Liczba osób", fill = "Płeć") +
  scale_fill_manual(values = c("Kobieta" = "pink", "Mężczyzna" = "blue")) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Wykres pie chart dla obywatelstwa
ggplot(ES_UAE_data %>% filter(country == "ZEA"),  aes(x = factor(1), fill = citizenship)) +
  geom_bar(width = 1, color = "white") +
  coord_polar(theta = "y") +
  ggtitle("Rozkład obywatelstwa wsród mieszkańców ZEA") +
  labs(fill = "Obywatelstwo") +
  scale_fill_brewer(palette = "Set2") +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  )

# Wykres z mapą ciepła wagi względem wieku i płci
ggplot(filter(ES_UAE_data, country == "ZEA"),   
       aes(x = age, y = weight, color = ifelse(sex == 1, "Mężczyzna", "Kobieta"))) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # czarne linie
  labs(title = "Rozkład wagi w zależności od wieku i płci",
       x = "Wiek",
       y = "Waga (kg)",
       color = "Płeć") +
  scale_color_manual(values = c("Mężczyzna" = "blue", "Kobieta" = "pink")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



# Rozkład BMI wg. płci
# filtrowanie i etykietowanie
es_zea_zea <- ES_UAE_data %>%
  filter(country == "ZEA") %>%
  mutate(sex = factor(sex, levels = c(0, 1), labels = c("Kobieta", "Mężczyzna")))

# wykres
ggplot(es_zea_zea, aes(x = bmi, y = sex, fill = sex)) +
  geom_density_ridges(alpha = 0.6) +
  scale_fill_manual(values = c("Kobieta" = "red", "Mężczyzna" = "blue")) +
  labs(title = "Rozkład BMI wg płci", x = "BMI", y = "Płeć") +
  theme_minimal()


