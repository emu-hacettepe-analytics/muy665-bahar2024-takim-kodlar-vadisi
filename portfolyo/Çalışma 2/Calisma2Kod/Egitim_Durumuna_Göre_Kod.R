library(readxl)

# Assuming the Excel file is stored locally and columns are as seen in the screenshot
education_data <- read_excel("C:\Users\myalcin\Documents\GitHub\muy665-bahar2024-takim-kodlar-vadisi\portfolyo\??al????ma 2\Calisma2VeriSet\Evlilik_Durumu_VS.xlsx", skip = 9)  # Adjust the path and skip as necessary
education_data <- education_data[,-2]  # This drops the second column
# Set column names based on your screenshot
colnames(education_data) <- c("Date","Okuma-yazma bilmeyen","Okuma yazma bilen fakat bir okul bitirmeyen",	"D0lkokul",	"Ortaokul veya dengi meslek okul",	"Genel lise","Lise dengi meslek okul"	,"YC<ksekokul veya fakC<lte",	"AC'D1k C6Dretim")

library(dplyr)
library(tidyr)
library(ggplot2)

# Extract the year from the Date assuming it's in a format like "2014 Ocak"
education_data$Year <- as.integer(sub(" .*", "", education_data$Date))

# Calculate the yearly averages for each education category
yearly_education_averages <- education_data %>%
  group_by(Year) %>%
  summarise(across(`Okuma-yazma bilmeyen`:`AC'D1k C6Dretim`, mean, na.rm = TRUE))

# Reshape the data for easier plotting
yearly_education_long <- yearly_education_averages %>%
  pivot_longer(cols = `Okuma-yazma bilmeyen`:`AC'D1k C6Dretim`, names_to = "Education_Status", values_to = "Average_Unemployment_Rate")
# Plotting the average unemployment rates by year and education status
ggplot(yearly_education_long, aes(x = Education_Status, y = Average_Unemployment_Rate, fill = Education_Status)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Yearly Average Unemployment Rates by Education Status", x = "Education_Status", y = "Average Unemployment Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
