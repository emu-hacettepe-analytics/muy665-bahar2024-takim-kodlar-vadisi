# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

# Load age group data
age_data <- read_excel("C:\\Users\\parad\\Documents\\GitHub\\muy665-bahar2024-takim-kodlar-vadisi\\portfolyo\\Calisma2\\Calisma2VeriSet\\Yas_Grubu_VS.xlsx", skip = 9)
age_data <- age_data[,-2]  # Remove the second column
colnames(age_data) <- c("Date", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65+")

# Extract the year and calculate yearly averages
age_data$Year <- as.integer(sub(" .*", "", age_data$Date))
yearly_age_averages <- age_data %>%
  group_by(Year) %>%
  summarise(across(`15-19`:`65+`, mean, na.rm = TRUE))

# Clean up the workspace
rm(age_data)

# Load education status data
education_data <- read_excel("C:\\Users\\parad\\Documents\\GitHub\\muy665-bahar2024-takim-kodlar-vadisi\\portfolyo\\Calisma2\\Calisma2VeriSet\\Egitim_Durum_VS.xlsx", skip = 9)
education_data <- education_data[,-2]  # Remove the second column
colnames(education_data) <- c("Date", "Okuma-yazma bilmeyen", "Okuma yazma bilen fakat bir okul bitirmeyen", "Ilkokul", "Ortaokul veya dengi meslek okul", "Genel lise", "Lise dengi meslek okul", "Yuksekokul veya fakulte", "Acik Ogretim")

# Extract the year and calculate yearly averages
education_data$Year <- as.integer(sub(" .*", "", education_data$Date))
yearly_education_averages <- education_data %>%
  group_by(Year) %>%
  summarise(across(`Okuma-yazma bilmeyen`:`Acik Ogretim`, mean, na.rm = TRUE))

# Clean up the workspace
rm(education_data)

# Load marital status data
marital_data <- read_excel("C:\\Users\\parad\\Documents\\GitHub\\muy665-bahar2024-takim-kodlar-vadisi\\portfolyo\\Calisma2\\Calisma2VeriSet\\Evlilik_Durumu_VS.xlsx", skip = 9)
marital_data <- marital_data[,-2]  # Remove the second column
colnames(marital_data) <- c("Date", "Hic Evlenmedi", "Evlendi", "Bosandi", "Esi Oldu")

# Extract the year and calculate yearly averages
marital_data$Year <- as.integer(sub(" .*", "", marital_data$Date))
yearly_marriage_averages <- marital_data %>%
  group_by(Year) %>%
  summarise(across(`Hic Evlenmedi`:`Esi Oldu`, mean, na.rm = TRUE))

# Clean up the workspace
rm(marital_data)

# Combine the data sets by 'Year'
combined_data <- full_join(yearly_age_averages, yearly_education_averages, by = "Year") %>%
  full_join(yearly_marriage_averages, by = "Year")

# Select age and marital status columns and calculate correlations
age_marital_correlation <- cor(combined_data[grepl("15-19|20-24|25-29|30-34|35-39|40-44|45-49|50-54|55-59|60-64|65+|Hic Evlenmedi|Evlendi|Bosandi|Esi Oldu", names(combined_data))], use = "complete.obs")

# Select age and education status columns and calculate correlations
age_education_correlation <- cor(combined_data[grepl("15-19|20-24|25-29|30-34|35-39|40-44|45-49|50-54|55-59|60-64|65+|Okuma-yazma bilmeyen|Okuma yazma bilen fakat bir okul bitirmeyen|Ilkokul|Ortaokul veya dengi meslek okul|Genel lise|Lise dengi meslek okul|Yuksekokul veya fakulte|Acik Ogretim", names(combined_data))], use = "complete.obs")

# Select marital and education status columns and calculate correlations
marital_education_correlation <- cor(combined_data[grepl("Hic Evlenmedi|Evlendi|Bosandi|Esi Oldu|Okuma-yazma bilmeyen|Okuma yazma bilen fakat bir okul bitirmeyen|Ilkokul|Ortaokul veya dengi meslek okul|Genel lise|Lise dengi meslek okul|Yuksekokul veya fakulte|Acik Ogretim", names(combined_data))], use = "complete.obs")

# Function to plot correlation heatmaps
plot_correlation_heatmap <- function(correlation_matrix, title) {
  corr_melt <- melt(correlation_matrix)
  ggplot(corr_melt, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") + # Add a border to tiles
    scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                         midpoint = 0, limit = c(-1, 1), space = "Lab",
                         name = "Correlation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1),
          axis.text.y = element_text(size = 10), # Increase y-axis text size
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 16)) + # Increase title size
    coord_fixed() +
    ggtitle(title)
}

# Visualize age and marital status correlations
plot_correlation_heatmap(age_marital_correlation, "Age and Marital Status Correlation")

# Visualize age and education status correlations
plot_correlation_heatmap(age_education_correlation, "Age and Education Status Correlation")

# Visualize marital and education status correlations
plot_correlation_heatmap(marital_education_correlation, "Marital and Education Status Correlation")
