library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)

# Yaş grubu verisini yükleme
age_data <- read_excel("C:\\Users\\parad\\Documents\\GitHub\\muy665-bahar2024-takim-kodlar-vadisi\\portfolyo\\Calisma2\\Calisma2VeriSet\\Yas_Grubu_VS.xlsx", skip = 9)
age_data <- age_data[,-2]  # İkinci sütunu kaldır
colnames(age_data) <- c("Date", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64","65+")

# Yılı çıkar ve yıllık ortalamaları hesapla
age_data$Year <- as.integer(sub(" .*", "", age_data$Date))
yearly_age_averages <- age_data %>%
  group_by(Year) %>%
  summarise(across(`15-19`:`65+`, mean, na.rm = TRUE))

# Çalışma alanını temizle
rm(age_data)

# Eğitim durumu verisini yükleme
education_data <- read_excel("C:\\Users\\parad\\Documents\\GitHub\\muy665-bahar2024-takim-kodlar-vadisi\\portfolyo\\Calisma2\\Calisma2VeriSet\\Egitim_Durum_VS.xlsx", skip = 9)
education_data <- education_data[,-2]  # İkinci sütunu kaldır
colnames(education_data) <- c("Date","Okuma-yazma bilmeyen","Okuma yazma bilen fakat bir okul bitirmeyen",	"Ilkokul",	"Ortaokul veya dengi meslek okul",	"Genel lise","Lise dengi meslek okul"	,"Yuksekokul veya fakulte",	"Acik Ogretim")

# Yılı çıkar ve yıllık ortalamaları hesapla
education_data$Year <- as.integer(sub(" .*", "", education_data$Date))
yearly_education_averages <- education_data %>%
  group_by(Year) %>%
  summarise(across(`Okuma-yazma bilmeyen`:`Acik Ogretim`, mean, na.rm = TRUE))

# Çalışma alanını temizle
rm(education_data)

# Evlilik durumu verisini yükleme
marital_data <- read_excel("C:\\Users\\parad\\Documents\\GitHub\\muy665-bahar2024-takim-kodlar-vadisi\\portfolyo\\Calisma2\\Calisma2VeriSet\\Evlilik_Durumu_VS.xlsx", skip = 9)
marital_data <- marital_data[,-2]  # İkinci sütunu kaldır
colnames(marital_data) <- c("Date", "Hic Evlenmedi", "Evlendi", "Bosandi", "Esi Oldu")

# Yılı çıkar ve yıllık ortalamaları hesapla
marital_data$Year <- as.integer(sub(" .*", "", marital_data$Date))
yearly_marriage_averages <- marital_data %>%
  group_by(Year) %>%
  summarise(across(`Hic Evlenmedi`:`Esi Oldu`, mean, na.rm = TRUE))

# Çalışma alanını temizle
rm(marital_data)

# Veri kümelerini 'Year' sütununda birleştir
combined_data <- full_join(yearly_age_averages, yearly_education_averages, by = "Year") %>%
  full_join(yearly_marriage_averages, by = "Year")

# Yaş ve evlilik durumu sütunlarını seç ve korelasyon hesapla
age_marital_correlation <- cor(combined_data[grepl("15-19|20-24|25-29|30-34|35-39|40-44|45-49|50-54|55-59|60-64|65+|Hic Evlenmedi|Evlendi|Bosandi|Esi Oldu", names(combined_data))], use = "complete.obs")

# Yaş ve eğitim durumu sütunlarını seç ve korelasyon hesapla
age_education_correlation <- cor(combined_data[grepl("15-19|20-24|25-29|30-34|35-39|40-44|45-49|50-54|55-59|60-64|65+|Yuksekokul veya fakulte|Acik Ogretim", names(combined_data))], use = "complete.obs")

# Evlilik ve eğitim durumu sütunlarını seç ve korelasyon hesapla
marital_education_correlation <- cor(combined_data[grepl("Hic Evlenmedi|Evlendi|Bosandi|Esi Oldu", names(combined_data))], use = "complete.obs")

# Korelasyonları görselleştirme
library(corrplot)

# Yaş ve evlilik durumu korelasyonlarını görselleştirme
corrplot(age_marital_correlation, method = "circle")

# Yaş ve eğitim durumu korelasyonlarını görselleştirme
corrplot(age_education_correlation, method = "circle")

# Evlilik ve eğitim durumu korelasyonlarını görselleştirme
corrplot(marital_education_correlation, method = "circle")

