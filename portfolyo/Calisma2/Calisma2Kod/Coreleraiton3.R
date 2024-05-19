library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

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

# Veri setlerini uzun formata dönüştürme
age_long <- yearly_age_averages %>%
  pivot_longer(cols = -Year, names_to = "Age_Group", values_to = "Age_Average")

education_long <- yearly_education_averages %>%
  pivot_longer(cols = -Year, names_to = "Education_Status", values_to = "Education_Average")

marital_long <- yearly_marriage_averages %>%
  pivot_longer(cols = -Year, names_to = "Marital_Status", values_to = "Marital_Average")

# Yaş ve Evlilik Durumu Korelasyonu
combined_age_marital <- left_join(age_long, marital_long, by = "Year")
correlation_age_marital <- cor(combined_age_marital$Age_Average, combined_age_marital$Marital_Average, use = "complete.obs")

p1 <- ggplot(combined_age_marital, aes(x = Age_Average, y = Marital_Average, color = Age_Group)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_color_brewer(palette = "Set2") +
  labs(title = paste("Correlation between Age Group Averages and Marital Status Averages: ", round(correlation_age_marital, 2)),
       x = "Age Group Unemployment Rate Averages", y = "Marital Status Unemployment Rate Averages") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

print(p1)

# Yaş ve Eğitim Durumu Korelasyonu
combined_age_education <- left_join(age_long, education_long, by = "Year")
correlation_age_education <- cor(combined_age_education$Age_Average, combined_age_education$Education_Average, use = "complete.obs")

p2 <- ggplot(combined_age_education, aes(x = Age_Average, y = Education_Average, color = Age_Group)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_color_brewer(palette = "Set2") +
  labs(title = paste("Correlation between Age Group Averages and Education Status Averages: ", round(correlation_age_education, 2)),
       x = "Age Group Unemployment Rate Averages", y = "Education Status Unemployment Rate Averages") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

print(p2)

# Eğitim ve Evlilik Durumu Korelasyonu
combined_education_marital <- left_join(education_long, marital_long, by = "Year")
correlation_education_marital <- cor(combined_education_marital$Education_Average, combined_education_marital$Marital_Average, use = "complete.obs")

p3 <- ggplot(combined_education_marital, aes(x = Education_Average, y = Marital_Average, color = Education_Status)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_color_brewer(palette = "Set2") +
  labs(title = paste("Correlation between Education Status Averages and Marital Status Averages: ", round(correlation_education_marital, 2)),
       x = "Education Status Unemployment Rate Averages", y = "Marital Status Unemployment Rate Averages") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

print(p3)
