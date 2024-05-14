library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
install.packages("corrplot")
# Load age group data
age_data <- read_excel("C:\\Users\\sdemirtas\\Desktop\\D0E analitiDi C6dev 15.05.2024\\yas grubu.xlsx", skip = 9)
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
education_data <- read_excel("../muy665-bahar2024-takim-kodlar-vadisi/portfolyo/??al????ma 2/Calisma2VeriSet/e??itim durumu.xlsx", skip = 9)
education_data <- education_data[,-2]
colnames(education_data) <- c("Date","Okuma-yazma bilmeyen","Okuma yazma bilen fakat bir okul bitirmeyen",	"D0lkokul",	"Ortaokul veya dengi meslek okul",	"Genel lise","Lise dengi meslek okul"	,"YC<ksekokul veya fakC<lte",	"AC'D1k C6Dretim")

# Extract the year and prepare yearly averages
education_data$Year <- as.integer(sub(" .*", "", education_data$Date))
yearly_education_averages <- education_data %>%
  group_by(Year) %>%
  summarise(across(`Okuma-yazma bilmeyen`:`AC'D1k C6Dretim`, mean, na.rm = TRUE))

# Clean up workspace
rm(education_data)
# Load marital status data
marital_data <- read_excel("C:\\Users\\sdemirtas\\Desktop\\D0E analitiDi C6dev 15.05.2024\\evlilik durumuna gC6re.xlsx", skip = 9)
marital_data <- marital_data[,-2]
colnames(marital_data) <- c("Date", "HiC' Evlenmedi", "Evlendi", "BoEandD1", "EEi CldC<")

# Extract the year and prepare yearly averages
marital_data$Year <- as.integer(sub(" .*", "", marital_data$Date))
yearly_marriage_averages <- marital_data %>%
  group_by(Year) %>%
  summarise(across(`HiC' Evlenmedi`:`EEi CldC<`, mean, na.rm = TRUE))

# Clean up workspace
rm(marital_data)

# Merge the datasets on 'Year'
combined_data <- full_join(yearly_age_averages, yearly_education_averages, by = "Year") %>%
  full_join(yearly_marriage_averages, by = "Year")

# Select only age and marital status columns for correlation
age_marital_correlation <- cor(combined_data[grepl("15-19|20-24|Evlendi|BoEandD1|EEi CldC<", names(combined_data))], use = "complete.obs")

# Select only age and education status columns for correlation
age_education_correlation <- cor(combined_data[grepl("15-19|20-24|YC<ksekokul veya fakC<lte|AC'D1k C6Dretim", names(combined_data))], use = "complete.obs")

# Select only marital and education status columns for correlation
marital_education_correlation <- cor(combined_data[grepl("Evlendi|BoEandD1|YC<ksekokul veya fakC<lte|AC'D1k C6Dretim", names(combined_data))], use = "complete.obs")

library(corrplot)

# Visualizing Age and Marital Status Correlation
corrplot(age_marital_correlation, method = "circle")

# Visualizing Age and Education Status Correlation
corrplot(age_education_correlation, method = "circle")

# Visualizing Marital and Education Status Correlation
corrplot(marital_education_correlation, method = "circle")

