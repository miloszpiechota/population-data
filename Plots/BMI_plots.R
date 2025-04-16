# Załaduj pakiety
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# Wczytaj dane
additional_data <- read_csv("additional_data.csv")
dane <- read_csv("BMI_real.csv")

# Przetwórz dane
obesity_trends <- additional_data %>%
  mutate(is_obese = ifelse(BMI >= 30, 1, 0)) %>%
  group_by(country, year) %>%
  summarise(obesity_rate = mean(is_obese, na.rm = TRUE) * 100, .groups = 'drop')

bmi_trend <- additional_data %>%
  group_by(country, year) %>%
  summarise(mean_BMI = mean(BMI, na.rm = TRUE), .groups = 'drop')

# Definiowanie UI
ui <- fluidPage(
  titlePanel("Dashboard BMI i Otyłości"),
  sidebarLayout(
    sidebarPanel(
      p("Dashboard przedstawia różne analizy dotyczące BMI i otyłości w wybranych krajach.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Trend otyłości", plotOutput("obesityTrend")),
        tabPanel("BMI na przestrzeni lat", plotOutput("bmiOverTime")),
        tabPanel("Trend średniego BMI", plotOutput("meanBmiTrend")),
        tabPanel("Procentowy udział kategorii BMI", plotOutput("bmiCategories"))
      )
    )
  )
)

# Definiowanie serwera
server <- function(input, output) {
  
  # Wykres 1: Trend otyłości
  output$obesityTrend <- renderPlot({
    ggplot(obesity_trends, aes(x = year, y = obesity_rate, color = country)) +
      geom_smooth(method = "lm", se = FALSE, size = 1.3) +  # tylko linie trendu
      labs(
        title = "Trend otyłości (BMI ≥ 30) w czasie",
        subtitle = "Odsetek osób z otyłością w wybranych krajach (1925–2025)",
        x = "Rok",
        y = "Odsetek osób z otyłością [%]",
        color = "Kraj"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # Wykres 2: Wartości BMI na przestrzeni lat
  output$bmiOverTime <- renderPlot({
    ggplot(additional_data, aes(x = year, y = BMI, color = country)) +
      geom_jitter(alpha = 0.2, size = 1) +  # punkty indywidualne
      geom_smooth(se = FALSE, size = 1.2, method = "loess") +  # wygładzone linie dla każdego kraju
      labs(
        title = "Wartości BMI na przestrzeni lat",
        subtitle = "Indywidualne dane z podziałem na kraj",
        x = "Rok",
        y = "BMI",
        color = "Kraj"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # Wykres 3: Trend średniego BMI
  output$meanBmiTrend <- renderPlot({
    ggplot(bmi_trend, aes(x = year, y = mean_BMI, color = country)) +
      geom_smooth(method = "lm", se = FALSE, size = 1.3) +
      labs(
        title = "Trend średniego BMI w czasie",
        subtitle = "Dane dla wybranych krajów (1925–2025)",
        x = "Rok",
        y = "Średni BMI",
        color = "Kraj"
      ) +
      theme_minimal()
  })
  
  # Wykres 4: Procentowy udział kategorii BMI
  output$bmiCategories <- renderPlot({
    bmi_counts <- additional_data %>%
      mutate(BMI_category = case_when(
        BMI < 18.5 ~ "Niedowaga",
        BMI >= 18.5 & BMI < 25 ~ "Prawidłowa",
        BMI >= 25 & BMI < 30 ~ "Nadwaga",
        BMI >= 30 ~ "Otyłość"
      )) %>%
      group_by(country, BMI_category) %>%
      summarise(count = n(), .groups = 'drop')
    
    bmi_percentages <- bmi_counts %>%
      group_by(country) %>%
      mutate(
        percentage = count / sum(count) * 100,
        label = paste0(round(percentage, 1), "%")
      ) %>%
      ungroup()
    
    ggplot(bmi_percentages, aes(x = country, y = percentage, fill = BMI_category)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "black", size = 3) +
      labs(
        title = "Procentowy udział kategorii BMI w populacji każdego kraju",
        x = "Kraj",
        y = "Procent populacji",
        fill = "Kategoria BMI"
      ) +
      theme_minimal()
  })
}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)
