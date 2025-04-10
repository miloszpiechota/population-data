# height vs weight in UAE
title_create_weight_uae <- "Height vs Weight in UAE"
create_weight_uae <- function(t){
  ggplot(filter(ES_UAE_data, country == "UAE"), aes(x = height, y = weight, color = ifelse(sex == 1, "Male", "Female"))) + 
    geom_point(alpha = 0.6) +
    ggtitle(t) + 
    labs(x = "Height (cm)", y = "Weight (kg)", color = "Sex") +
    scale_color_manual(values = c("Female" = "pink", "Male" = "blue")) +
    theme(plot.title = element_text(hjust = 0.5))
}
print(create_weight_uae(title_create_weight_uae))

# Histogram of height by sex in UAE
title_height_by_sex_in_UAE <- "Height Distribution by Sex"
create_height_by_sex_in_UAE<- function(t){
  ggplot(filter(ES_UAE_data, country == "UAE"), aes(x = height, fill = ifelse(sex == 1, "Male", "Female"))) +
    geom_histogram(position = "dodge", binwidth = 5, color = "black", alpha = 0.7) +
    ggtitle(t) +
    labs(x = "Height (cm)", y = "Number of People", fill = "Sex") +
    scale_fill_manual(values = c("Female" = "pink", "Male" = "blue")) + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}
print(create_height_by_sex_in_UAE(title_height_by_sex_in_UAE))

# Histogram of weight by sex in UAE +
title_weight_distribution_by_sex <- "Weight Distribution by Sex"
create_weight_distribution_by_sex <- function(t){
  ggplot(filter(ES_UAE_data, country == "UAE"), aes(x = weight, fill = ifelse(sex == 1, "Male", "Female"))) +
    geom_histogram(position = "dodge", binwidth = 5, color = "black", alpha = 0.7) +
    ggtitle(t) +
    labs(x = "Weight (kg)", y = "Number of People", fill = "Sex") +  
    scale_fill_manual(values = c("Female" = "pink", "Male" = "blue")) + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}
print(create_weight_distribution_by_sex(title_weight_distribution_by_sex))

# Education level by sex in UAE
title_education_by_sex <- "Weight Distribution by Sex"
create_education_by_sex <- function(t){
  ggplot(filter(ES_UAE_data, country == "UAE"), aes(x = education, fill = ifelse(sex == 1, "Male", "Female"))) +
    geom_bar(position = "dodge", width = 1, color = "black", alpha = 0.7) +
    ggtitle(t) +
    labs(x = "Education Level", y = "Number of People", fill = "Sex") +
    scale_fill_manual(values = c("Female" = "pink", "Male" = "blue")) + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}
print(create_education_by_sex(title_education_by_sex))

# Pie chart of citizenship in UAE
title_citizenship_distribution <- "Citizenship Distribution in UAE"
create_citizenship_distribution <- function(t){
  ggplot(ES_UAE_data %>% filter(country == "UAE"), aes(x = factor(1), fill = citizenship)) +
    geom_bar(width = 1, color = "white") +
    coord_polar(theta = "y") +
    ggtitle(t) +
    labs(fill = "Citizenship") +
    scale_fill_brewer(palette = "Set2") +  
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right"
    )
}
print(create_citizenship_distribution(title_citizenship_distribution))

# Heat map of weight by age and sex
title_weight_by_age_and_sex <- "Weight by Age and Sex"
create_weight_by_age_and_sex <- function(t){
  ggplot(filter(ES_UAE_data, country == "UAE"), aes(x = age, y = weight, color = ifelse(sex == 1, "Male", "Female"))) +
    geom_point(alpha = 0.5) +  
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    labs(title = t,
         x = "Age",
         y = "Weight (kg)",
         color = "Sex") +
    scale_color_manual(values = c("Male" = "blue", "Female" = "pink")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}
print(create_weight_by_age_and_sex(title_weight_by_age_and_sex))

# BMI distribution by sex in UAE
title_BMI_distribution_by_sex <- "BMI Distribution by Sex"
create_BMI_distribution_by_sex  <- function(t){
  es_uae_uae <- ES_UAE_data %>%
    filter(country == "UAE") %>%
    mutate(sex = factor(sex, levels = c(0, 1), labels = c("Female", "Male")))
  
  ggplot(es_uae_uae, aes(x = bmi, y = sex, fill = sex)) +
    geom_density_ridges(alpha = 0.6) +
    scale_fill_manual(values = c("Female" = "red", "Male" = "blue")) +
    labs(title = t, x = "BMI", y = "Sex") +
    theme_minimal()
}
print(create_BMI_distribution_by_sex(title_BMI_distribution_by_sex))

plots_list <- list(
  title_create_weight_uae = function() create_weight_uae(title_create_weight_uae),
  title_height_by_sex_in_UAE = function() create_height_by_sex_in_UAE(title_height_by_sex_in_UAE),
  title_weight_distribution_by_sex = function() create_weight_distribution_by_sex(title_weight_distribution_by_sex),
  title_education_by_sex = function() create_education_by_sex(title_education_by_sex),
  title_citizenship_distribution = function() create_citizenship_distribution(title_citizenship_distribution),
  title_weight_by_age_and_sex = function() create_weight_by_age_and_sex(title_weight_by_age_and_sex),
  title_BMI_distribution_by_sex = function () create_BMI_distribution_by_sex(title_BMI_distribution_by_sex)
)

names(plots_list)[1] <- title_create_weight_uae  
names(plots_list)[2] <- title_height_by_sex_in_UAE
names(plots_list)[3] <- title_weight_distribution_by_sex
names(plots_list)[4] <- title_education_by_sex
names(plots_list)[5] <- title_citizenship_distribution
names(plots_list)[6] <- title_weight_by_age_and_sex
names(plots_list)[7] <- title_BMI_distribution_by_sex