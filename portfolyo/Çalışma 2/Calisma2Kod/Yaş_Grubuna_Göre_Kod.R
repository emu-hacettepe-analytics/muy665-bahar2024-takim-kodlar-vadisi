
library(readxl)
# Load the data, assuming the file path is correct and the sheet is named appropriately
# Ensure to replace with the actual path and type it directly here
unemployment_data <- read_excel("C:\Users\myalcin\Documents\GitHub\muy665-bahar2024-takim-kodlar-vadisi\portfolyo\??al????ma 2\Calisma2VeriSet\Ya??_Grubu_VS.xlsx", skip = 9)
unemployment_data <- unemployment_data[,-2]  # This drops the second column
# Adjust path and skip rows as necessary
# Set column names, assuming they represent age groups directly
colnames(unemployment_data) <- c("Date", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65+")
library(ggplot2)
library(dplyr)
library(tidyr)

# Calculating the average unemployment rate for each age group
age_group_averages <- unemployment_data %>%
  select(-Date) %>%  # Exclude the Date for calculation
  summarise(across(everything(), mean, na.rm = TRUE)) %>%  # Calculate mean, ignoring NA values
  pivot_longer(
    cols = everything(),
    names_to = "Age_Group",
    values_to = "Average_Unemployment_Rate"
  )

# Plotting the average unemployment rates for each age group
ggplot(age_group_averages, aes(x = Age_Group, y = Average_Unemployment_Rate, fill = Age_Group)) +
  geom_bar(stat = "identity") +
  labs(title = "YaE gruplarD1na gC6re Ortalama D0Esizlik OranD1", x = "YaE Grubu", y = "Ortalama D0Esizlik OranD1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


