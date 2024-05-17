# Gerekli kütüphaneleri yükleme
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Excel dosyasından veri alma
marriage_data <- read_excel("C:/Users/parad/Documents/GitHub/muy665-bahar2024-takim-kodlar-vadisi/portfolyo/Calisma2/Calisma2VeriSet/Evlilik_Durumu_VS.xlsx", skip = 9)

# İkinci sütunu çıkarma
marriage_data <- marriage_data[,-2]

# Sütun adlarını belirleme
colnames(marriage_data) <- c("Date", "Hic_Evlenmedi", "Evlendi", "Bosandi", "Esi_Oldu")

# Tarih sütunundan yılı çıkarma
marriage_data$Year <- as.integer(sub(" .*", "", marriage_data$Date))

# Her evlilik durumu için yıllık ortalamaları hesaplama
yearly_marriage_averages <- marriage_data %>%
  group_by(Year) %>%
  summarise(across(Hic_Evlenmedi:Esi_Oldu, mean, na.rm = TRUE))

# Verileri grafik için uzun formata dönüştürme
yearly_marriage_long <- yearly_marriage_averages %>%
  pivot_longer(cols = Hic_Evlenmedi:Esi_Oldu, names_to = "Marital_Status", values_to = "Average_Rate")

# Yıllık ortalama oranları yıl ve evlilik durumu bazında görselleştirme
ggplot(yearly_marriage_long, aes(x = as.factor(Year), y = Average_Rate, fill = Marital_Status)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = sprintf("%.1f", Average_Rate)), position = position_dodge(width = 0.9), vjust = -0.3, size = 3) +
  labs(title = "Evlilik Durumuna Göre Yıllık Ortalama Oranlar", x = "Yıl", y = "Ortalama Oran (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
