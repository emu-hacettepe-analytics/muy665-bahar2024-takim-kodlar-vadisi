library(dplyr)
library(ggplot2)
unemployment_data <- read_excel("C:\\Users\\sdemirtas\\Desktop\\İş analitiği ödev 15.05.2024\\yas grubu.xlsx", skip = 9)
unemployment_data <- unemployment_data[,-2]  # This drops the second column
# Adjust path and skip rows as necessary
# Set column names, assuming they represent age groups directly
colnames(unemployment_data) <- c("Date", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65+")
# Extract the year as an integer
unemployment_data$Year <- as.integer(sub(" .*", "", unemployment_data$Date))

# Calculate the average unemployment rate for all age groups combined for each year
annual_averages <- unemployment_data %>%
  group_by(Year) %>%
  summarise(Average_Unemployment_Rate = mean(c_across(`15-19`:`65+`), na.rm = TRUE))

# Plotting the average unemployment rates by year using a bar graph
ggplot(annual_averages, aes(x = as.factor(Year), y = Average_Unemployment_Rate, fill = as.factor(Year))) +
  geom_bar(stat = "identity", width = 0.7) +  # Using bars to represent the average rates
  scale_fill_brewer(palette = "Paired") +  # Optional: adds color using a color palette
  labs(title = "Bütün Yaş gruplarının yıl bazında İşsizlik Oranı", 
       x = "Yıl", 
       y = "Ortalama İşsizlik Oranı (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


                                             
