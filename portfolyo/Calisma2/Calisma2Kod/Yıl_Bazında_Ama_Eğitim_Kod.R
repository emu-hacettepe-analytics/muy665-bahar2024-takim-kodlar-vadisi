library(readxl)

# Assuming the Excel file is stored locally and columns are as seen in the screenshot
education_data <- read_excel("C:\\Users\\sdemirtas\\Desktop\\İş analitiği ödev 15.05.2024\\eğitim durumu.xlsx", skip = 9)  # Adjust the path and skip as necessary
education_data <- education_data[,-2]  # This drops the second column
# Set column names based on your screenshot
colnames(education_data) <- c("Date", "Illiterate", "Literate_no_school", "Incomplete_primary", "Primary", 
                              "Middle_school", "High_school_general", "High_school_vocational", "Undergraduate_or_higher", "Unknown")
library(dplyr)

# Extract the year from the Date
education_data$Year <- as.integer(sub(" .*", "", education_data$Date))

# Calculate the yearly averages for each education category
yearly_education_averages <- education_data %>%
  group_by(Year) %>%
  summarise(across(Illiterate:`Unknown`, mean, na.rm = TRUE))

# Calculate the overall yearly average across all education categories
yearly_averages_overall <- education_data %>%
  group_by(Year) %>%
  summarise(Overall_Average = mean(c_across(Illiterate:`Unknown`), na.rm = TRUE))
library(tidyr)

# Reshape the data for plotting individual education category averages
yearly_education_long <- yearly_education_averages %>%
  pivot_longer(cols = Illiterate:`Unknown`, names_to = "Education_Status", values_to = "Average_Unemployment_Rate")
library(ggplot2)

ggplot(yearly_education_long, aes(x = as.factor(Year), y = Average_Unemployment_Rate, fill = Education_Status)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Yearly Average Unemployment Rates by Education Category", x = "Year", y = "Average Unemployment Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
