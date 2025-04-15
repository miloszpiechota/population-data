
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
  
  # Przykładowe prawdopodobieństwa (dostosuj według rzeczywistego rozkładu)
  age_indicator_kindergarten <- 0.9
  age_indicator_nokindergarten <- 0.1
  
  age_indicator_primary <- 0.95
  age_indicator_noprimary <- 0.05
  
  age_indicator_lowersecondary <- 0.9
  age_indicator_nolowersecondary <- 0.1
  
  age_indicator_uppersecondary <- 0.75
  age_indicator_postsecondary <- 0.15
  age_indicator_nouppersecondary <- 0.1
  
  age_indicator_bachelor <- 0.5
  age_indicator_nohigher <- 0.4
  age_indicator_master <- 0.08
  age_indicator_doctorate <- 0.02
  
  
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
  
  education <- ifelse(age < 3, "0",  # brak obowiązku edukacji, ale można uznać za edukację przedszkolną
                      ifelse(age >= 3 & age <= 5, 
                             sample(c("0", "None"), n, replace = TRUE, 
                                    prob = c(age_indicator_kindergarten, age_indicator_nokindergarten)),
                             ifelse(age >= 6 & age <= 11, 
                                    sample(c("1", "None"), n, replace = TRUE,
                                           prob = c(age_indicator_primary, age_indicator_noprimary)),
                                    ifelse(age >= 12 & age <= 15,
                                           sample(c("2", "None"), n, replace = TRUE,
                                                  prob = c(age_indicator_lowersecondary, age_indicator_nolowersecondary)),
                                           ifelse(age >= 16 & age <= 18,
                                                  sample(c("3", "4", "None"), n, replace = TRUE,
                                                         prob = c(age_indicator_uppersecondary, age_indicator_postsecondary, age_indicator_nouppersecondary)),
                                                  ifelse(age >= 19 & age <= 20,
                                                         sample(c("4", "6", "None"), n, replace = TRUE,
                                                                prob = c(age_indicator_postsecondary, age_indicator_bachelor, age_indicator_nohigher)),
                                                         ifelse(age >= 21,
                                                                sample(c("6", "7", "8", "None"), n, replace = TRUE,
                                                                       prob = c(age_indicator_bachelor, age_indicator_master, age_indicator_doctorate, age_indicator_nohigher)), NA)))))))
  
  
  bmi <- round(weight / ((height / 100)^2), 2)
  
  # Citizenship
  citizenship <- sample(c("ES citizen", "non-ES citizen"), n, replace = TRUE, prob = c(ES_citizen, ES_noncitizen))
  
  data.frame(height = height, weight = weight, sex = sex, year = year, age = age, education = education, bmi = bmi, citizenship = citizenship, country = "ES")
}

ES <- es_database(n, seed = 314592)
# ID arkusza Hiszpanii
ES_sheet_id <- "1Ri4C2_FD_mjz7fqYfR9zS5eLlD-fRD1NZQAaMohF0lk"
# Zapisanie danych do arkusza "Dane" - Do arkusza z danymi Hiszpanii
sheet_write(ES, ss = ES_sheet_id, sheet = "Dane")
ES_data <- read_sheet(ES_sheet_id, sheet = "Dane")
head(ES_data)