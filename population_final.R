
library(googlesheets4)
library(dplyr)

library(ggridges)
library(tidyverse)
#gs4_auth()

GB_sheet_id <- "17lKNBZYUmPJJoNtaMX_tj17JhzdLlZvSmdch3wNwW3c"
US_sheet_id <- "1_yeiClAqUlG_t5PgEPM0QBK99_AXi3qe_HAKrIQ_8Ic"
TJK_sheet_id <- "1O_GnUPEdCELxoiyczK_iYu5xxq0DZAZCQ4R4ZiLPYcs"
ES_sheet_id <- "1Ri4C2_FD_mjz7fqYfR9zS5eLlD-fRD1NZQAaMohF0lk"
UAE_sheet_id <- "1DC8ckcPiqi9MgmansTnKDc6VyiawesZbhgNJN-MldP0"



# 1. Wczytaj dane z arkuszy
GB_data <- read_sheet(GB_sheet_id, sheet = "source") %>% mutate(country = "GB")
US_data <- read_sheet(US_sheet_id, sheet = "source") %>% mutate(country = "US")
TJK_data <- read_sheet(TJK_sheet_id, sheet = "source") %>% mutate(country = "TJK")
ES_data <- read_sheet(ES_sheet_id, sheet = "Dane") %>% mutate(country = "ES")
UAE_data <- read_sheet(UAE_sheet_id, sheet = "source") %>% mutate(country = "UAE")

GB_data <- GB_data %>% mutate(education = as.character(education), country = "GB")
US_data <- US_data %>% mutate(education = as.character(education), country = "US")
TJK_data <- TJK_data %>% mutate(education = as.character(edu), country = "TJK")
ES_data <- ES_data %>% mutate(education = as.character(education), country = "ES")
UAE_data <- UAE_data %>% mutate(education = as.character(education), country = "UAE")


# 2. Połącz dane i wybierz tylko istotne kolumny
population_final <- bind_rows(GB_data, US_data, TJK_data, ES_data, UAE_data) %>%
  select(height, weight, sex, year, age, country)%>%
  mutate(
    height = round(as.numeric(height), 2),
    weight = round(as.numeric(weight), 2)
  )

# 3. Zapisz jako CSV lokalnie
write_csv(population_final, "population_final.csv")

# 4. Załaduj dane z powrotem do arkusza Google (Hiszpania)
sheet_write(population_final, ss = ES_sheet_id, sheet = "final_population")


indicators <- list(
  GB = list(
    age_indicator_kindergarten = 0.95,       # Wysoki wskaźnik uczęszczania do przedszkola
    age_indicator_nokindergarten = 0.05,     # Niski wskaźnik nieuczestnictwa w przedszkolu
    age_indicator_primary = 0.98,            # Wysoki wskaźnik uczęszczania do szkoły podstawowej
    age_indicator_noprimary = 0.02,          # Niski wskaźnik nieuczestnictwa w szkole podstawowej
    age_indicator_lowersecondary = 0.95,     # Wysoki wskaźnik ukończenia niższego wykształcenia średniego
    age_indicator_nolowersecondary = 0.05,   # Niski wskaźnik nieukończenia niższego wykształcenia średniego
    age_indicator_uppersecondary = 0.85,     # Wysoki wskaźnik ukończenia wyższego wykształcenia średniego
    age_indicator_postsecondary = 0.10,      # Umiarkowany wskaźnik ukończenia wykształcenia policealnego
    age_indicator_nouppersecondary = 0.05,   # Niski wskaźnik nieukończenia wyższego wykształcenia średniego
    age_indicator_bachelor = 0.50,           # Wysoki wskaźnik posiadania tytułu licencjata
    age_indicator_nohigher = 0.35,           # Umiarkowany wskaźnik braku wyższego wykształcenia
    age_indicator_master = 0.10,             # Niski wskaźnik posiadania tytułu magistra
    age_indicator_doctorate = 0.05,          # Niski wskaźnik posiadania tytułu doktora
    citizenship_prob = c(0.85, 0.15)         # 85% obywateli, 15% cudzoziemców
  ),
  
  ES = list(
    age_indicator_kindergarten = 0.92,       # Wysoki wskaźnik uczęszczania do przedszkola
    age_indicator_nokindergarten = 0.08,     # Niski wskaźnik nieuczestnictwa w przedszkolu
    age_indicator_primary = 0.96,            # Wysoki wskaźnik uczęszczania do szkoły podstawowej
    age_indicator_noprimary = 0.04,          # Niski wskaźnik nieuczestnictwa w szkole podstawowej
    age_indicator_lowersecondary = 0.88,     # Wysoki wskaźnik ukończenia niższego wykształcenia średniego
    age_indicator_nolowersecondary = 0.12,   # Niski wskaźnik nieukończenia niższego wykształcenia średniego
    age_indicator_uppersecondary = 0.78,     # Wysoki wskaźnik ukończenia wyższego wykształcenia średniego
    age_indicator_postsecondary = 0.12,      # Umiarkowany wskaźnik ukończenia wykształcenia policealnego
    age_indicator_nouppersecondary = 0.10,   # Niski wskaźnik nieukończenia wyższego wykształcenia średniego
    age_indicator_bachelor = 0.55,           # Wysoki wskaźnik posiadania tytułu licencjata
    age_indicator_nohigher = 0.35,           # Umiarkowany wskaźnik braku wyższego wykształcenia
    age_indicator_master = 0.06,             # Niski wskaźnik posiadania tytułu magistra
    age_indicator_doctorate = 0.04,          # Niski wskaźnik posiadania tytułu doktora
    citizenship_prob = c(0.8, 0.2)           # 80% obywateli, 20% cudzoziemców
  ),
  
  UAE = list(
    age_indicator_kindergarten = 0.80,       # Szacunkowo 80% dzieci uczęszcza do przedszkola
    age_indicator_nokindergarten = 0.20,     # 20% dzieci nie uczęszcza do przedszkola
    age_indicator_primary = 0.90,            # Wysoki wskaźnik uczęszczania do szkoły podstawowej
    age_indicator_noprimary = 0.10,          # 10% dzieci nie uczęszcza do szkoły podstawowej
    age_indicator_lowersecondary = 0.85,     # 85% uczniów kończy niższe wykształcenie średnie
    age_indicator_nolowersecondary = 0.15,   # 15% nie kończy niższego wykształcenia średniego
    age_indicator_uppersecondary = 0.74,     # 74% kończy wyższe wykształcenie średnie :contentReference[oaicite:2]{index=2}
    age_indicator_postsecondary = 0.56,      # 56% kończy wykształcenie policealne :contentReference[oaicite:3]{index=3}
    age_indicator_nouppersecondary = 0.26,   # 26% nie kończy wyższego wykształcenia średniego
    age_indicator_bachelor = 0.45,           # Szacunkowo 45% posiada tytuł licencjata
    age_indicator_nohigher = 0.40,           # 40% nie posiada wyższego wykształcenia
    age_indicator_master = 0.10,             # 10% posiada tytuł magistra
    age_indicator_doctorate = 0.05,          # 5% posiada tytuł doktora
    citizenship_prob = c(0.115, 0.885)       # 11.5% obywateli, 88.5% cudzoziemców :contentReference[oaicite:4]{index=4}
  ),
  
  TJK = list(
    age_indicator_kindergarten = 0.983,       # Bardzo wysoka frekwencja w przedszkolach
    age_indicator_nokindergarten = 0.017,
    age_indicator_primary = 0.9474,           # Wysoki wskaźnik uczęszczania do szkoły podstawowej
    age_indicator_noprimary = 0.0526,
    age_indicator_lowersecondary = 0.936,     # Wysoki wskaźnik kontynuowania nauki na poziomie średnim
    age_indicator_nolowersecondary = 0.064,
    age_indicator_uppersecondary = 0.936,     # Przyjęto taką samą wartość jak dla niższego średniego
    age_indicator_postsecondary = 0.1,        # Szacunkowy udział kształcenia policealnego
    age_indicator_nouppersecondary = 0.1,     # Osoby kończące na poziomie średnim
    age_indicator_bachelor = 0.6706,          # Wysoka liczba zapisanych na studia licencjackie
    age_indicator_nohigher = 0.3294,
    age_indicator_master = 0.1,               # Szacunkowy udział studiów magisterskich
    age_indicator_doctorate = 0.05,           # Szacunkowy udział doktorantów
    citizenship_prob = c(0.95, 0.05)          # Około 95% to obywatele Tadżykistanu
  ),
  US = list(
    age_indicator_kindergarten = 0.84,       # Wskaźnik uczęszczania do przedszkola wśród 5-latków
    age_indicator_nokindergarten = 0.16,     # Wskaźnik nieuczestnictwa w przedszkolu
    age_indicator_primary = 0.99,            # Wysoki wskaźnik uczęszczania do szkoły podstawowej
    age_indicator_noprimary = 0.01,          # Niski wskaźnik nieuczestnictwa w szkole podstawowej
    age_indicator_lowersecondary = 1.03,     # Wskaźnik ukończenia niższego wykształcenia średniego (może przekraczać 100% ze względu na metodologię obliczeń)
    age_indicator_nolowersecondary = 0.0,    # Praktycznie brak nieukończenia niższego wykształcenia średniego
    age_indicator_uppersecondary = 0.92,     # Wysoki wskaźnik ukończenia wyższego wykształcenia średniego
    age_indicator_postsecondary = 0.10,      # Umiarkowany wskaźnik ukończenia wykształcenia policealnego
    age_indicator_nouppersecondary = 0.08,   # Niski wskaźnik nieukończenia wyższego wykształcenia średniego
    age_indicator_bachelor = 0.35,           # Wskaźnik posiadania tytułu licencjata
    age_indicator_nohigher = 0.51,           # Wskaźnik braku wyższego wykształcenia
    age_indicator_master = 0.13,             # Wskaźnik posiadania tytułu magistra
    age_indicator_doctorate = 0.02,          # Wskaźnik posiadania tytułu doktora
    citizenship_prob = c(0.93, 0.07)         # 93% obywateli, 7% cudzoziemców
  )
  

)

source("functions/generate_additional_data.R")
additional_data <- generate_additional_data(population_final, indicators)

# 3. Zapisz jako CSV lokalnie
write_csv(additional_data, "additional_data.csv")

# 4. Załaduj dane z powrotem do arkusza Google (Hiszpania)
sheet_write(additional_data, ss = ES_sheet_id, sheet = "additional_data")


