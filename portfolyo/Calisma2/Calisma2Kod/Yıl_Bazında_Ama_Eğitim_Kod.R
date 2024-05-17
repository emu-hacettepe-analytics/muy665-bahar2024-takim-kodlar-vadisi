library(readxl)

# Excel dosyasının yerel olarak depolandığını ve sütun isimlerinin ekran görüntüsünde görüldüğü gibi olduğunu varsayalım
education_data <- read_excel("C:\\Users\\parad\\Documents\\GitHub\\muy665-bahar2024-takim-kodlar-vadisi\\portfolyo\\Calisma2\\Calisma2VeriSet\\Egitim_Durum_VS.xlsx", skip = 9)  # Yolu ve gerektiğinde atlama sayısını ayarlayın
education_data <- education_data[,-2]  # Bu, ikinci sütunu düşürür
# Sütun isimlerini ekran görüntüsüne göre ayarlayın
colnames(education_data) <- c("Tarih", "Okur_yazar_degil", "Okuryazar_okul_degil", "İlkögretim_mezunu_olmayan", "İlkögretim", 
                              "Ortaokul", "Lise_genel", "Meslek_lisesi", "Lisansustu_ya_da_daha_üstü", "Bilinmeyen")
library(dplyr)

# Tarihten yılı çıkarın
education_data$Yıl <- as.integer(sub(" .*", "", education_data$Tarih))

# Her eğitim kategorisi için yıllık ortalamaları hesaplayın
yıllık_education_ortalama <- education_data %>%
  group_by(Yıl) %>%
  summarise(across(Okur_yazar_degil:`Bilinmeyen`, mean, na.rm = TRUE))

# Tüm eğitim kategorilerindeki genel yıllık ortalama hesaplayın
genel_yıllık_ortalama <- education_data %>%
  group_by(Yıl) %>%
  summarise(Genel_Ortalama = mean(c_across(Okur_yazar_degil:`Bilinmeyen`), na.rm = TRUE))
library(tidyr)

# Grafik için verileri yeniden şekillendirin
yıllık_education_uzun <- yıllık_education_ortalama %>%
  pivot_longer(cols = Okur_yazar_degil:`Bilinmeyen`, names_to = "Eğitim_Durumu", values_to = "Ortalama_Işsizlik_Oranı")
library(ggplot2)

ggplot(yıllık_education_uzun, aes(x = as.factor(Yıl), y = Ortalama_Işsizlik_Oranı, fill = Eğitim_Durumu)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Eğitim Kategorisine Göre Yıllık Ortalama İşsizlik Oranları", x = "Yıl", y = "Ortalama İşsizlik Oranı (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))