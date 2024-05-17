library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Dosya yolunu düzenleyerek okuma işlemi
education_data <- read_excel("C:/Users/parad/Documents/GitHub/muy665-bahar2024-takim-kodlar-vadisi/portfolyo/Calisma2/Calisma2VeriSet/Egitim_Durum_VS.xlsx", skip = 9)

# İkinci sütunu atlayarak verileri düzenleme
education_data <- education_data[,-2]

# Sütun adlarını belirleme
colnames(education_data) <- c("Date", "Okuma_yazma_bilmeyen", "Okuma_yazma_bilen_fakat_bir_okul_bitirmeyen", "Ilkokul", "Ortaokul_veya_dengi_meslek_okul", "Genel_lise", "Lise_dengi_meslek_okul", "Yuksekokul_veya_fakulte", "Acik_Ogretim")

# Yıl bilgisini Tarih sütunundan çıkarma
education_data$Year <- as.integer(sub(" .*", "", education_data$Date))

# Her eğitim kategorisi için yıllık ortalamaları hesaplama
yearly_education_averages <- education_data %>%
  group_by(Year) %>%
  summarise(across(`Okuma_yazma_bilmeyen`:`Acik_Ogretim`, mean, na.rm = TRUE))

# Verileri yeniden şekillendirme
yearly_education_long <- yearly_education_averages %>%
  pivot_longer(cols = `Okuma_yazma_bilmeyen`:`Acik_Ogretim`, names_to = "Education_Status", values_to = "Average_Unemployment_Rate")

# Yıllık ortalama işsizlik oranlarını eğitim durumu ve yıl bazında görselleştirme
ggplot(yearly_education_long, aes(x = Education_Status, y = Average_Unemployment_Rate, fill = Education_Status)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Yıllık Ortalama İşsizlik Oranları ve Eğitim Durumu", x = "Eğitim Durumu", y = "Ortalama İşsizlik Oranı (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")