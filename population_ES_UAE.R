# Loading required libraries

library(googlesheets4)
library(dplyr)
library(ggplot2)
library(ggridges)
library(tidyverse)


# Generating data: 1000 observations
n <- 1000

# Authorization
#gs4_auth()

# Function to generate data for Spain with age-gender logic
es_database <- function(n, seed = NULL) {
  Indicator_Male <- 0.49
  Indicator_Female <- 1 - Indicator_Male
  
  Boy_newborn_height <- 50
  Boy_kindergarten_height <- 100
  Boy_primary_height <- 130
  Boy_secondary_height <- 165
  Adult_male_height <- 173
  
  Girl_newborn_height <- 49
  Girl_kindergarten_height <- 98
  Girl_primary_height <- 125
  Girl_secondary_height <- 160
  Adult_female_height <- 161
  
  Boy_newborn_weight <- 3.5
  Boy_kindergarten_weight <- 18    
  Boy_primary_weight <- 30        
  Boy_secondary_weight <- 55      
  Adult_male_weight <- 77.5
  
  Girl_newborn_weight <- 3.3
  Girl_kindergarten_weight <- 17   
  Girl_primary_weight <- 28       
  Girl_secondary_weight <- 52     
  Adult_female_weight <- 67.5
  
  age_indicator_kindergarten <- 0.9
  age_indicator_nokindergarten <- 1 - age_indicator_kindergarten
  age_indicator_primary <- 0.9
  age_indicator_noprimary <- 1 - age_indicator_primary
  age_indicator_secondary <- 0.8
  age_indicator_nosecondary <- 1 - age_indicator_secondary
  age_indicator_only_kindergarten <- 0.1
  age_indicator_only_primary <- 0.2
  age_indicator_only_secondary <- 0.3
  age_indicator_only_higher <- 0.4
  
  ES_citizen <- 0.873
  ES_noncitizen <- 1 - ES_citizen
  
  if (!is.null(seed)) set.seed(seed)
  
  eps <- rnorm(n, mean = 0, sd = 1)
  
  year <- sample(1925:2025, n, replace = TRUE)
  age <- sample(1:100, n, replace = TRUE)
  sex <- sample(c(0, 1), n, replace = TRUE, prob = c(Indicator_Female, Indicator_Male))
  
  # Height based on age and gender
  height <- ifelse(sex == 1,
                   ifelse(age < 1, round(rnorm(n, mean = Boy_newborn_height, sd = 5) + eps, 2),
                          ifelse(age >= 1 & age < 6, round(rnorm(n, mean = Boy_kindergarten_height, sd = 4) + eps, 2),
                                 ifelse(age >= 6 & age < 12, round(rnorm(n, mean = Boy_primary_height, sd = 4) + eps, 2),
                                        ifelse(age >= 12 & age < 18, round(rnorm(n, mean = Boy_secondary_height, sd = 7) + eps, 2),
                                               round(rnorm(n, mean = Adult_male_height, sd = 10) + 2*eps, 2))))),
                   ifelse(age < 1, round(rnorm(n, mean = Girl_newborn_height, sd = 5) + eps, 2),
                          ifelse(age >= 1 & age < 6, round(rnorm(n, mean = Girl_kindergarten_height, sd = 4) + eps, 2),
                                 ifelse(age >= 6 & age < 12, round(rnorm(n, mean = Girl_primary_height, sd = 4) + eps, 2),
                                        ifelse(age >= 12 & age < 18, round(rnorm(n, mean = Girl_secondary_height, sd = 7) + eps, 2),
                                               round(rnorm(n, mean = Adult_female_height, sd = 10) + eps, 2))))))
  
  # Weight based on age and gender
  weight <- ifelse(sex == 1,
                   ifelse(age < 1, round(rnorm(n, mean = Boy_newborn_weight, sd = 0.5) + 3*eps, 2),
                          ifelse(age >= 1 & age < 6, round(rnorm(n, mean = Boy_kindergarten_weight, sd = 3) + 3*eps, 2),
                                 ifelse(age >= 6 & age < 12, round(rnorm(n, mean = Boy_primary_weight, sd = 4) + 3*eps, 2),
                                        ifelse(age >= 12 & age < 18, round(rnorm(n, mean = Boy_secondary_weight, sd = 6) + 3*eps, 2),
                                               round(rnorm(n, mean = Adult_male_weight, sd = 8) + 3*eps, 2))))),
                   ifelse(age < 1, round(rnorm(n, mean = Girl_newborn_weight, sd = 0.5) + 3*eps, 2),
                          ifelse(age >= 1 & age < 6, round(rnorm(n, mean = Girl_kindergarten_weight, sd = 3) + 3*eps, 2),
                                 ifelse(age >= 6 & age < 12, round(rnorm(n, mean = Girl_primary_weight, sd = 4) + 3*eps, 2),
                                        ifelse(age >= 12 & age < 18, round(rnorm(n, mean = Girl_secondary_weight, sd = 6) + 3*eps, 2),
                                               round(rnorm(n, mean = Adult_female_weight, sd = 8) + 3*eps, 2))))))
  
  education <- ifelse(age < 3, "None",
                      ifelse(age >= 3 & age <= 5, 
                             sample(c("Kindergarten", "None"), n, replace = TRUE, 
                                    prob = c(age_indicator_kindergarten, age_indicator_nokindergarten)),
                             ifelse(age >= 6 & age <= 11, 
                                    sample(c("Primary", "None"), n, replace = TRUE,
                                           prob = c(age_indicator_primary, age_indicator_noprimary)),
                                    ifelse(age >= 12 & age <= 18, 
                                           sample(c("Secondary", "None"), n, replace = TRUE,
                                                  prob = c(age_indicator_secondary, age_indicator_nosecondary)),
                                           ifelse(age > 18, 
                                                  sample(c("Only kindergarten", "Only primary", "Only secondary", "Higher"), n, replace = TRUE,
                                                         prob = c(age_indicator_only_kindergarten, age_indicator_only_primary, age_indicator_only_secondary, age_indicator_only_higher)), NA)))))
  
  bmi <- round(weight / ((height / 100)^2), 2)
  
  # Citizenship
  citizenship <- sample(c("ES citizen", "non-ES citizen"), n, replace = TRUE, prob = c(ES_citizen, ES_noncitizen))
  
  data.frame(height = height, weight = weight, sex = sex, year = year, age = age, education = education, bmi = bmi, citizenship = citizenship, country = "ES")
}



# Function to generate data for the United Arab Emirates
uae_database <- function(n, seed = NULL){
  Indicator_Male <- 0.64
  Indicator_Female <- 1 - Indicator_Male
  
  Boy_newborn_height <- 90
  Boy_kindergarten_height <- 105
  Boy_primary_height <- 130
  Boy_secondary_height <- 160
  Adult_male_height <- 172
  
  Girl_newborn_height <- 95
  Girl_kindergarten_height <- 108
  Girl_primary_height <- 120
  Girl_secondary_height <- 150
  Adult_female_height <- 160
  
  Boy_newborn_weight <- 12
  Boy_kindergarten_weight <- 19    
  Boy_primary_weight <- 32        
  Boy_secondary_weight <- 55      
  Adult_male_weight <- 75   
  
  Girl_newborn_weight <- 13
  Girl_kindergarten_weight <- 18   
  Girl_primary_weight <- 30       
  Girl_secondary_weight <- 50     
  Adult_female_weight <- 65
  
  age_indicator_kindergarten <- 0.85
  age_indicator_nokindergarten <- 1 - age_indicator_kindergarten
  age_indicator_primary <- 0.95
  age_indicator_noprimary <- 1 - age_indicator_primary
  age_indicator_secondary <- 0.90
  age_indicator_nosecondary <- 1 - age_indicator_secondary
  age_indicator_only_kindergarten <- 0.15
  age_indicator_only_primary <- 0.15
  age_indicator_only_secondary <- 0.30
  age_indicator_only_higher <- 1 - age_indicator_only_kindergarten - age_indicator_only_primary - age_indicator_only_secondary      
  
  Religion_Islam <- 0.745
  Religion_Christian <- 0.129
  Religion_Hindu <- 0.062
  Religion_Buddhist <- 0.032
  Religion_Agnosticism <- 0.013
  Religion_Others <- 1 - Religion_Islam - Religion_Christian - Religion_Hindu - Religion_Buddhist - Religion_Agnosticism
  
  UAE_citizen <- 0.19
  UAE_noncitizen <- 1 - UAE_citizen
  
  if (!is.null(seed)) set.seed(seed)
  
  eps <- rnorm(n, mean = 0, sd = 1)
  
  year <- sample(1925:2025, n, replace = TRUE)
  age <- sample(1:100, n, replace = TRUE)
  sex <- sample(c(0,1), n,replace = TRUE, prob = c(Indicator_Female, Indicator_Male))
  
  # Height based on age and gender
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
  
  # Weight based on age and gender
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
  
  education <- ifelse(age < 3, "None",
                      ifelse(age >= 3 & age <= 5, 
                             sample(c("Kindergarten", "None"), n, replace = TRUE, 
                                    prob = c(age_indicator_kindergarten, age_indicator_nokindergarten)),
                             ifelse(age >= 6 & age <= 11, 
                                    sample(c("Primary", "None"), n, replace = TRUE,
                                           prob = c(age_indicator_primary, age_indicator_noprimary)),
                                    ifelse(age >= 12 & age <= 18, 
                                           sample(c("Secondary", "None"), n, replace = TRUE,
                                                  prob = c(age_indicator_secondary, age_indicator_nosecondary)),
                                           ifelse(age > 18, 
                                                  sample(c("Only kindergarten", "Only primary", "Only secondary", "Higher"), n, replace = TRUE,
                                                         prob = c(age_indicator_only_kindergarten, age_indicator_only_primary, age_indicator_only_secondary, age_indicator_only_higher)),NA)))))
  
  bmi <- round(weight/((height/100)^2),2)
  
  #citizenship
  citizenship <- sample(c("UAE citizen", "non-UAE citizen"), n, replace = TRUE, prob = c(UAE_citizen, UAE_noncitizen))
  
  data.frame(height = height, weight = weight, sex = sex, year = year, age = age, education = education, bmi = bmi, citizenship = citizenship, country = "UAE")
}




# --------- GENEROWANIE DANYCH DLA HISZPANII ---------

#ES <- es_database(n, seed = 314592)

# Podgląd pierwszych kilku wierszy danych dla Hiszpanii
#head(ES)



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
#head(UAE)

# Zapisanie danych do pliku CSV
#write.csv(UAE, file = "UAE_database.csv", row.names = FALSE)

#UAE_sheet_id <- "1DC8ckcPiqi9MgmansTnKDc6VyiawesZbhgNJN-MldP0"
#sheet_write(UAE ,UAE_sheet_id, sheet = "source")

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

# --- PLOTS FOR ES & UAE


# **1. Age Histogram**

ggplot(ES_UAE_data, aes(x = age, fill = country)) +
  geom_histogram(binwidth = 5, alpha = 0.7, position = "identity") +
  labs(title = "Age Distribution in Spain and UAE", x = "Age", y = "Number of People") +
  theme_minimal()

# **2. Scatter plot BMI vs Age**

ggplot(ES_UAE_data, aes(x = age, y = bmi, color = country)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "BMI vs Age", x = "Age", y = "BMI") +
  theme_minimal()

# **3. Boxplot of Height by Gender**

ggplot(ES_UAE_data, aes(x = as.factor(sex), y = height, fill = country)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("Female", "Male")) +
  labs(title = "Height by Gender and Country", x = "Gender", y = "Height (cm)") +
  theme_minimal()


# **4. Scatter Plot of Weight by Gender**
ggplot(ES_UAE_data, aes(x = weight, fill = factor(sex))) +
  geom_histogram(position = "identity", alpha = 0.6, binwidth = 2, color = "gray40") +
  scale_fill_manual(values = c("red", "blue"), labels = c("Kobiety (0)", "Mężczyźni (1)")) +
  labs(title = "Rozkład wagi według płci",
       subtitle = "Z dokładniejszą skalą liczby osób",
       x = "Waga (kg)",
       y = "Liczba osób",
       fill = "Płeć") +
  theme_minimal() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +  # Kontrola podziałek na osi Y
  coord_cartesian(ylim = c(0, max(table(cut(ES_UAE_data$weight, breaks = 30))) * 0.6)) +  # Ograniczenie skali Y
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +  # Kontrola podziałek na osi X
  coord_cartesian(xlim = c(0, max(table(cut(ES_UAE_data$height, breaks = 30))) * 0.6))  # Ograniczenie skali X



# --- PLOTS FOR ES ---
# **1. Scatter Plot of Height vs Weight**

plot(filter(ES_UAE_data, country == "ES")$height, filter(ES_UAE_data, country == "ES")$weight,
     main = "Height vs Weight",
     xlab = "Height (cm)",
     ylab = "Weight (kg)",
     col = ifelse(filter(ES_UAE_data, country == "ES")$sex == "1", "blue", "red"),
     pch = 16)
legend("topleft", legend = c("Male", "Female"), col = c("blue", "red"), pch = 16)

# **2. Pair Plot for Numerical Variables: Height, Weight, Age, and Year**

pairs(filter(ES_UAE_data, country == "ES")[, c("height", "weight", "age", "year")],
      main = "Pair Plot for Numerical Variables")

# **3. Boxplot of Height by Education Level**

boxplot(height ~ education, data = filter(ES_UAE_data, country == "ES"),
        main = "Height Distribution by Education Level",
        xlab = "Education",
        ylab = "Height (cm)",
        col = c("orange", "lightblue", "lightgreen", "pink"))

# **4. Bar Plot of Average Weight by Gender**

avg_weight <- tapply(filter(ES_UAE_data, country == "ES")$weight, filter(ES_UAE_data, country == "ES")$sex, mean)
barplot(avg_weight,
        main = "Average Weight by Gender",
        xlab = "Gender",
        ylab = "Average Weight (kg)",
        col = c("red", "blue"),
        ylim = c(0, max(avg_weight) * 1.1))

# **5. Scatter Plot of Weight by Gender**

plot(filter(ES_UAE_data, country == "ES")$sex, filter(ES_UAE_data, country == "ES")$weight,
     main = "Weight by Gender",
     xlab = "Gender",
     ylab = "Weight (kg)",
     col = c("red", "blue"),
     pch = 16)

# **6. Age Histogram**

hist(filter(ES_UAE_data, country == "ES")$age, breaks = 20,
     main = "Age Distribution",
     xlab = "Age",
     col = "lightgreen")

# **7. Bar Plot of Education Category Counts**

education_counts <- table(filter(ES_UAE_data, country == "ES")$education)
barplot(education_counts,
        main = "Counts by Education Category",
        xlab = "Education",
        ylab = "Number of Observations",
        col = "orange")

# --- PLOTS FOR UAE ---

source("Plots/plots_UAE.R")
