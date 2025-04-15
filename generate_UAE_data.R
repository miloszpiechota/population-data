

library(googlesheets4)
library(dplyr)
library(ggplot2)
library(ggridges)
library(tidyverse)


# Generating data: 1000 observations
n <- 1000

# Authorization
#gs4_auth()
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


UAE <- uae_database(n, seed = 314592)
# ID arkusza Hiszpanii
UAE_sheet_id <- "1DC8ckcPiqi9MgmansTnKDc6VyiawesZbhgNJN-MldP0"

sheet_write(UAE, ss = UAE_sheet_id, sheet = "source")
UAE_data <- read_sheet(ES_sheet_id, sheet = "source")
head(UAE_data)