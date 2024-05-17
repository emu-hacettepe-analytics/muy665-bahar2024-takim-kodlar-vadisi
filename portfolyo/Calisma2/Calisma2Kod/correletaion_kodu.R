library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
# Load age group data
age_data <- read_excel("C:\\Users\\parad\\Documents\\GitHub\\muy665-bahar2024-takim-kodlar-vadisi\\portfolyo\\Calisma2\\Calisma2VeriSet\\Yas_Grubu_VS.xlsx", skip = 9)
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
education_data <- read_excel("C:\\Users\\parad\\Documents\\GitHub\\muy665-bahar2024-takim-kodlar-vadisi\\portfolyo\\Calisma2\\Calisma2VeriSet\\Egitim_Durum_VS.xlsx", skip = 9)
education_data <- education_data[,-2]
colnames(education_data) <- c("Date","Okuma-yazma bilmeyen","Okuma yazma bilen fakat bir okul bitirmeyen",	"Ilkokul",	"Ortaokul veya dengi meslek okul",	"Genel lise","Lise dengi meslek okul"	,"Yuksekokul veya fakulte",	"Acik Ogretim")

# Extract the year and prepare yearly averages
education_data$Year <- as.integer(sub(" .*", "", education_data$Date))
yearly_education_averages <- education_data %>%
  group_by(Year) %>%
  summarise(across(`Okuma-yazma bilmeyen`:`Acik Ogretim`, mean, na.rm = TRUE))

# Clean up workspace
rm(education_data)
# Load marital status data
marital_data <- read_excel("C:\\Users\\parad\\Documents\\GitHub\\muy665-bahar2024-takim-kodlar-vadisi\\portfolyo\\Calisma2\\Calisma2VeriSet\\Evlilik_Durumu_VS.xlsx", skip = 9)
marital_data <- marital_data[,-2]
colnames(marital_data) <- c("Date", "Hic Evlenmedi", "Evlendi", "Bosandi", "Esi Oldu")

# Extract the year and prepare yearly averages
marital_data$Year <- as.integer(sub(" .*", "", marital_data$Date))
yearly_marriage_averages <- marital_data %>%
  group_by(Year) %>%
  summarise(across(`Hic Evlenmedi`:`Esi Oldu`, mean, na.rm = TRUE))

# Clean up workspace
rm(marital_data)

# Merge the datasets on 'Year'
combined_data <- full_join(yearly_age_averages, yearly_education_averages, by = "Year") %>%
  full_join(yearly_marriage_averages, by = "Year")

# Select only age and marital status columns for correlation
age_marital_correlation <- cor(combined_data[grepl("15-19|20-24|Evlendi|Bosandi|Esi Oldu", names(combined_data))], use = "complete.obs")

# Select only age and education status columns for correlation
age_education_correlation <- cor(combined_data[grepl("15-19|20-24|Yuksekokul veya fakulte|Acik Ogretim", names(combined_data))], use = "complete.obs")

# Select only marital and education status columns for correlation
marital_education_correlation <- cor(combined_data[grepl("Evlendi|Bosandi|Yuksekokul veya fakulte|Acik Ogretim", names(combined_data))], use = "complete.obs")

library(corrplot)

# Visualizing Age and Marital Status Correlation
corrplot(age_marital_correlation, method = "circle")

# Visualizing Age and Education Status Correlation
corrplot(age_education_correlation, method = "circle")

# Visualizing Marital and Education Status Correlation
corrplot(marital_education_correlation, method = "circle")

