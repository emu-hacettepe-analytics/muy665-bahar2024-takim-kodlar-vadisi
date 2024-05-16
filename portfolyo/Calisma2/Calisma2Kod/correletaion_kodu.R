library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

install.packages("corrplot")
# Load age group data
age_data <- read_excel("C:\\Users\\sdemirtas\\Desktop\\İş analitiği ödev 15.05.2024\\yas grubu.xlsx", skip = 9)
age_data <- age_data[,-2]
colnames(age_data) <- c("Date", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64","65+")

# Extract the year and prepare yearly averages
age_data$Year <- as.integer(sub(" .*", "", age_data$Date))
yearly_age_averages <- age_data %>%
  group_by(Year) %>%
  summarise(across(`15-19`:`65+`, mean, na.rm = TRUE))

# Clean up workspace
rm(age_data)
# Load education status data
education_data <- read_excel("C:\\Users\\sdemirtas\\Desktop\\İş analitiği ödev 15.05.2024\\eğitim durumu.xlsx", skip = 9)
education_data <- education_data[,-2]
colnames(education_data) <- c("Date","Okuma-yazma bilmeyen","Okuma yazma bilen fakat bir okul bitirmeyen",	"İlkokul",	"Ortaokul veya dengi meslek okul",	"Genel lise","Lise dengi meslek okul"	,"Yüksekokul veya fakülte",	"Açık öğretim")

# Extract the year and prepare yearly averages
education_data$Year <- as.integer(sub(" .*", "", education_data$Date))
yearly_education_averages <- education_data %>%
  group_by(Year) %>%
  summarise(across(`Okuma-yazma bilmeyen`:`Açık öğretim`, mean, na.rm = TRUE))

# Clean up workspace
rm(education_data)
# Load marital status data
marital_data <- read_excel("C:\\Users\\sdemirtas\\Desktop\\İş analitiği ödev 15.05.2024\\evlilik durumuna göre.xlsx", skip = 9)
marital_data <- marital_data[,-2]
colnames(marital_data) <- c("Date", "Hiç Evlenmedi", "Evlendi", "Boşandı", "Eşi Öldü")

# Extract the year and prepare yearly averages
marital_data$Year <- as.integer(sub(" .*", "", marital_data$Date))
yearly_marriage_averages <- marital_data %>%
  group_by(Year) %>%
  summarise(across(`Hiç Evlenmedi`:`Eşi Öldü`, mean, na.rm = TRUE))

# Clean up workspace
rm(marital_data)

# Merge the datasets on 'Year'
combined_data <- full_join(yearly_age_averages, yearly_education_averages, by = "Year") %>%
  full_join(yearly_marriage_averages, by = "Year")

# Select only age and marital status columns for correlation
age_marital_correlation <- cor(combined_data[grepl("15-19|20-24|Evlendi|Boşandı|Eşi Öldü", names(combined_data))], use = "complete.obs")

# Select only age and education status columns for correlation
age_education_correlation <- cor(combined_data[grepl("15-19|20-24|Yüksekokul veya fakülte|Açık öğretim", names(combined_data))], use = "complete.obs")

# Select only marital and education status columns for correlation
marital_education_correlation <- cor(combined_data[grepl("Evlendi|Boşandı|Yüksekokul veya fakülte|Açık öğretim", names(combined_data))], use = "complete.obs")

library(corrplot)

# Visualizing Age and Marital Status Correlation
corrplot(age_marital_correlation, method = "circle")

# Visualizing Age and Education Status Correlation
corrplot(age_education_correlation, method = "circle")

# Visualizing Marital and Education Status Correlation
corrplot(marital_education_correlation, method = "circle")

