
# Funkcja generująca dane
dgp <- function(n, seed = NULL) {
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

# Generowanie danych: 1000 obserwacji
n <- 1000
dta <- dgp(n, seed = 314592)

# Podgląd pierwszych kilku wierszy danych
head(dta)


# Generowanie danych: 1000 obserwacji
n <- 1000
dta <- dgp(n, seed = 314592)

# Podgląd pierwszych kilku wierszy danych
head(dta)


# 1. Wykres zależności między wzrostem a wagą
plot(dta$height, dta$weight,
     main = "Wzrost vs Waga",
     xlab = "Wzrost (cm)",
     ylab = "Waga (kg)",
     col = ifelse(dta$sex == "1", "blue", "red"),
     pch = 16)
legend("topleft", legend = c("Male", "Female"), col = c("blue", "red"), pch = 16)

# 2. Macierz wykresów (pairs) dla zmiennych numerycznych: wzrost, waga, wiek i rok
pairs(dta[, c("height", "weight", "age", "year")],
      main = "Pary wykresów zmiennych numerycznych")



# 4. Boxplot przedstawiający rozkład wzrostu w zależności od poziomu wykształcenia
boxplot(height ~ education, data = dta,
        main = "Rozkład wzrostu wg wykształcenia",
        xlab = "Wykształcenie",
        ylab = "Wzrost (cm)",
        col = c("orange", "lightblue", "lightgreen", "pink"))


# Obliczanie średniej wagi dla każdej płci
avg_weight <- tapply(dta$weight, dta$sex, mean)

# Tworzenie wykresu słupkowego
barplot(avg_weight,
        main = "Średnia waga w zależności od płci",
        xlab = "Płeć",
        ylab = "Średnia waga (kg)",
        col = c("red", "blue"),
        ylim = c(0, max(avg_weight) * 1.1))
# Przekształcenie zmiennej 'sex' na format tekstowy dla lepszej czytelności
#dta$sex <- factor(dta$sex, labels = c("Kobieta", "Mężczyzna"))

# Tworzenie wykresu punktowego
plot(dta$sex, dta$weight,
     main = "Waga w zależności od płci",
     xlab = "Płeć",
     ylab = "Waga (kg)",
     col = c("red", "blue"),
     pch = 16)  # 'pch' określa symbol punktu; 16 to pełne kółko


# 5. Histogram wieku
hist(dta$age, breaks = 20,
     main = "Rozkład wieku",
     xlab = "Wiek",
     col = "lightgreen")

# 6. Wykres słupkowy - liczebność poszczególnych kategorii wykształcenia
education_counts <- table(dta$education)
barplot(education_counts,
        main = "Liczebność kategorii wykształcenia",
        xlab = "Wykształcenie",
        ylab = "Liczba obserwacji",
        col = "orange")

# Zapisanie danych do pliku CSV
write.csv(dta, file = "dane_ESS.csv", row.names = FALSE)

library(googlesheets4)

#gs4_auth()

# Podaj ID swojego arkusza Google Sheets (sprawdź w URL arkusza)
sheet_id <- "1Ri4C2_FD_mjz7fqYfR9zS5eLlD-fRD1NZQAaMohF0lk"
# Zaokrąglamy wartości do 2 miejsc po przecinku
dta$height <- round(dta$height, 2)
dta$weight <- round(dta$weight, 2)

# Zapisanie danych do pliku CSV
write.csv(dta, file = "dane_ESS.csv", row.names = FALSE)

# Zapisanie danych do arkusza "Dane"
#sheet_write(dta, ss =sheet_id, sheet = "Dane")
#range_write(sheet_id, data = dta, sheet = "Arkusz1", col_names = TRUE)

#------------------------

#sheet_id_mateusz<- "1DC8ckcPiqi9MgmansTnKDc6VyiawesZbhgNJN-MldP0"
#dta_kolegi <- read_sheet(sheet_id_mateusz, sheet = "source")

#zea_key_mateusz <- "1DC8ckcPiqi9MgmansTnKDc6VyiawesZbhgNJN-MldP0"

#zea_data <- read_sheet(zea_key_mateusz, sheet = "source")

#head(zea_data)
#head(dta)


# Wczytanie wymaganych bibliotek
library(googlesheets4)
library(dplyr)
library(ggplot2)

# Wczytanie danych kolegi z Google Sheets
zea_key_mateusz <- "1DC8ckcPiqi9MgmansTnKDc6VyiawesZbhgNJN-MldP0"
zea_data <- read_sheet(zea_key_mateusz, sheet = "source")

# Konwersja płci na numeryczną
zea_data$sex <- as.numeric(zea_data$sex)  

# Dodanie kraju "ZEA"
zea_data$country <- "ZEA"

# Łączenie danych
dane_ES_ZEA <- bind_rows(dta, zea_data)

# **1. Histogram wieku**
ggplot(dane_ES_ZEA, aes(x = age, fill = country)) +
  geom_histogram(binwidth = 5, alpha = 0.7, position = "identity") +
  labs(title = "Rozkład wieku w Hiszpanii i ZEA", x = "Wiek", y = "Liczba osób") +
  theme_minimal()

# **2. Scatter plot BMI vs wiek**

ggplot(dane_ES_ZEA, aes(x = age, y = bmi, color = country)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Zależność BMI od wieku", x = "Wiek", y = "BMI") +
  theme_minimal()

# **3. Boxplot wzrostu według płci**
ggplot(dane_ES_ZEA, aes(x = as.factor(sex), y = height, fill = country)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("Kobiety", "Mężczyźni")) +
  labs(title = "Wzrost według płci i kraju", x = "Płeć", y = "Wzrost (cm)") +
  theme_minimal()

# **4. Wykres słupkowy obywatelstwa w Hiszpanii**
ggplot(filter(dane_ES_ZEA, country == "ES"), aes(x = citizenship, fill = citizenship)) +
  geom_bar() +
  labs(title = "Podział obywateli i nie-obywateli w Hiszpanii", x = "Obywatelstwo", y = "Liczba osób") +
  theme_minimal()

