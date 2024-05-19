library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load age group data
age_data <- read_excel("C:\\Users\\parad\\Documents\\GitHub\\muy665-bahar2024-takim-kodlar-vadisi\\portfolyo\\Calisma2\\Calisma2VeriSet\\Yas_Grubu_VS.xlsx", skip = 9)
age_data <- age_data[,-2]
colnames(age_data) <- c("Date", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65+")

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
colnames(education_data) <- c("Date","Okuma_yazma_bilmeyen","Okuma_yazma_bilen_fakat_bir_okul_bitirmeyen","Ilkokul","Ortaokul_veya_dengi_meslek_okul","Genel_lise","Lise_dengi_meslek_okul","Yuksekokul_veya_fakulte","Acik_Ogretim")

# Extract the year and prepare yearly averages
education_data$Year <- as.integer(sub(" .*", "", education_data$Date))
yearly_education_averages <- education_data %>%
  group_by(Year) %>%
  summarise(across(Okuma_yazma_bilmeyen:Acik_Ogretim, mean, na.rm = TRUE))

# Clean up workspace
rm(education_data)

# Load marital status data
marital_data <- read_excel("C:\\Users\\parad\\Documents\\GitHub\\muy665-bahar2024-takim-kodlar-vadisi\\portfolyo\\Calisma2\\Calisma2VeriSet\\Evlilik_Durumu_VS.xlsx", skip = 9)
marital_data <- marital_data[,-2]
colnames(marital_data) <- c("Date", "Hic_Evlenmedi", "Evlendi", "Bosandi", "Esi_Oldu")

# Extract the year and prepare yearly averages
marital_data$Year <- as.integer(sub(" .*", "", marital_data$Date))
yearly_marriage_averages <- marital_data %>%
  group_by(Year) %>%
  summarise(across(Hic_Evlenmedi:Esi_Oldu, mean, na.rm = TRUE))

# Clean up workspace
rm(marital_data)

# Yaş ve Evlilik Durumu Korelasyonu
age_averages <- yearly_age_averages %>% select(-Year) %>% rowMeans(na.rm = TRUE)
marital_averages <- yearly_marriage_averages %>% select(-Year) %>% rowMeans(na.rm = TRUE)
correlation_age_marital <- cor(age_averages, marital_averages, use = "complete.obs")

p1 <- ggplot(data.frame(age_averages, marital_averages), aes(x = age_averages, y = marital_averages)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = paste("Correlation between Age Group Averages and Marital Status Averages: ", round(correlation_age_marital, 2)),
       x = "Age Group Unemployment Rate Averages", y = "Marital Status Unemployment Rate Averages") +
  theme_minimal()

print(p1)

# Yaş ve Eğitim Durumu Korelasyonu
education_averages <- yearly_education_averages %>% select(-Year) %>% rowMeans(na.rm = TRUE)
correlation_age_education <- cor(age_averages, education_averages, use = "complete.obs")

p2 <- ggplot(data.frame(age_averages, education_averages), aes(x = age_averages, y = education_averages)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = paste("Correlation between Age Group Averages and Education Status Averages: ", round(correlation_age_education, 2)),
       x = "Age Group Unemployment Rate Averages", y = "Education Status Unemployment Rate Averages") +
  theme_minimal()

print(p2)

# Eğitim ve Evlilik Durumu Korelasyonu
correlation_education_marital <- cor(education_averages, marital_averages, use = "complete.obs")

p3 <- ggplot(data.frame(education_averages, marital_averages), aes(x = education_averages, y = marital_averages)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = paste("Correlation between Education Status Averages and Marital Status Averages: ", round(correlation_education_marital, 2)),
       x = "Education Status Unemployment Rate Averages", y = "Marital Status Unemployment Rate Averages") +
  theme_minimal()

print(p3)
