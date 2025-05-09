# 📊 Population Data Dashboard – Spain & United Arab Emirates

## 📚 Table of Contents

* [About the Project](#about-the-project)
* [Team Collaboration](#team-collaboration)
* [Key Features](#key-features)
* [Technologies Used](#technologies-used)
* [Data Sources](#data-sources)
* [Visualizations](#visualizations)
* [How to Run](#how-to-run)

---

## 🧩 About the Project

This interactive dashboard was developed as a group project using **R** and **Shiny** with **flexdashboard** to analyze and visualize demographic and health-related data for Spain and the United Arab Emirates. The focus is on population metrics such as height, weight, and BMI, including real WHO data and simulated datasets. The project presents trends in obesity, average BMI, and educational indicators across countries.

---

## 👥 Team Collaboration

This project was created in a collaborative environment by a team of data science enthusiasts. We divided responsibilities among data preparation, visualization design, and dashboard development. Google Sheets integration and dynamic plot generation enabled smooth contributions across team members.

---

## 🚀 Key Features

* Dynamic dashboard with multiple tabs using **flexdashboard** and **Shiny**
* Real and synthetic BMI data comparison (WHO vs simulation)
* Time-series plots of average BMI and obesity rates (1925–2025)
* Interactive filters for country selection and plot type
* Categorization and percentage breakdown of BMI classes
* Visual difference analysis using ribbon plots
* Educational indicator-based data generation for simulations

---

## 🛠 Technologies Used

* **R**, **Shiny**, **flexdashboard**, **tidyverse**, **ggplot2**
* **DT** for interactive data tables
* **Google Sheets API** (`googlesheets4`) for real-time data input/output
* **ggridges**, **readr**, **dplyr**, **stringr** for data manipulation

---

## 📂 Data Sources

* WHO real BMI dataset (BMI\_real.csv)
* Simulated BMI and demographic data generated from:

  * Google Sheets-based population indicators
  * Country-specific mappings for education, citizenship, etc.

---

## 📈 Visualizations

The dashboard includes:

* **Obesity trends** over time based on simulated BMI ≥ 30
* **Average BMI trends** (simulated and real)
* **BMI category distribution** (underweight, normal, overweight, obese)
* **WHO vs Simulated** BMI comparisons (with difference & ribbon plots)
* **Country-wise filters** for tailored visual exploration

---

## 🧪 How to Run

To run the dashboard locally:

```r
# In RStudio or R console:
rmarkdown::run("population_dashboard.Rmd")
```

Make sure you have all required packages installed:

```r
install.packages(c("tidyverse", "flexdashboard", "shiny", "ggplot2", "DT", "googlesheets4"))
```

---

> 📌 Note: The dashboard was built with flexibility and scalability in mind, allowing future integration with additional demographic sources and extended international comparisons.

![image](https://github.com/user-attachments/assets/06315e27-ee6a-4312-a157-8740e877f46d)

![image](https://github.com/user-attachments/assets/094da586-0c19-47ce-b1d2-8929e8e65aeb)

![image](https://github.com/user-attachments/assets/ef4f85d9-12e5-4949-926c-c48b7b063718)

![image](https://github.com/user-attachments/assets/ad95d3b9-ae71-4651-a6b8-a1fdbbea4dc9)

![image](https://github.com/user-attachments/assets/45003ecf-8a2e-4026-b50a-319e1e349694)




