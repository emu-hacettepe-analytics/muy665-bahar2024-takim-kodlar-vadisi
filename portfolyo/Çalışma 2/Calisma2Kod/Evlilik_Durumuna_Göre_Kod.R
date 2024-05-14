library(readxl)

# Import data from the Excel file
marriage_data <- read_excel("C:\\Users\\sdemirtas\\Desktop\\İş analitiği ödev 15.05.2024\\evlilik durumuna göre.xlsx", skip = 9)  # Adjust the path and skip as necessary
marriage_data <- marriage_data[,-2]  # This drops the second column
# Set the column names based on the screenshot
colnames(marriage_data) <- c("Date", "Hiç Evlenmedi", "Evlendi", "Boşandı", "Eşi Öldü")

library(dplyr)
library(tidyr)
library(ggplot2)

# Extract the year from the Date assuming it's in a format like "2014 Ocak"
marriage_data$Year <- as.integer(sub(" .*", "", marriage_data$Date))

# Calculate the yearly averages for each marital status
yearly_marriage_averages <- marriage_data %>%
  group_by(Year) %>%
  summarise(across(`Hiç Evlenmedi`:`Eşi Öldü`, mean, na.rm = TRUE))

# Reshape the data for plotting individual marital status averages
yearly_marriage_long <- yearly_marriage_averages %>%
  pivot_longer(cols = `Hiç Evlenmedi`:`Eşi Öldü`, names_to = "Marital_Status", values_to = "Average_Unemployment_Rate")
# Plotting the average unemployment rates by year and marital status
ggplot(yearly_marriage_long, aes(x = as.factor(Year), y = Average_Unemployment_Rate, fill = Marital_Status)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = sprintf("%.1f", Average_Unemployment_Rate)), position = position_dodge(width = 0.9), vjust = -0.3, size = 3) +
  labs(title = "Yearly Average Unemployment Rates by Marital Status", x = "Year", y = "Average Unemployment Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

