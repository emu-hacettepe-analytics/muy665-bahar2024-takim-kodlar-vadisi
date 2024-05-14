# Load required packages
install.packages("dslabs")
library(dslabs)
# Define a function named "custom_summary"
custom_summary <- function(data_vector) {
  # Calculate average
  avg <- mean(data_vector)
  # Calculate median
  med <- median(data_vector)
  # Calculate standard deviation
  std_dev <- sd(data_vector)
  # Find minimum value
  min_val <- min(data_vector)
  # Find maximum value
  max_val <- max(data_vector)
  
  # Store the calculated values in an array
  summary_array <- c(avg, med, std_dev, min_val, max_val)
  
  # Return the array containing summary statistics
  return(summary_array)
}

# Example usage of the custom_summary function
#C MADDESİ
library(dslabs)
# Load the na_example dataset
data(na_example)

# Display the contents of the dataset including NA values
head(na_example)

# Calculate the total number of NA values in the dataset
total_na <- sum(is.na(na_example))
print(paste("Total NA values:", total_na))

# Replace all NA values with course number 665
na_example_filled <- na_example
na_example_filled[is.na(na_example_filled)] <- 665

# Display the updated dataset (no NA values)
head(na_example_filled)

# Calculate the new total number of NA values (should be zero)
new_total_na <- sum(is.na(na_example_filled))
print(paste("New total NA values:", new_total_na))

# Count the number of times the number 665 appears in the updated dataset
count_665 <- sum(na_example_filled == 665)
print(paste("Number of times 665 appears:", count_665))

# Create an empty list to store summary results for each column
summary_results <- list()

# Calculate summary statistics for na_example_filled directly
summary_results <- custom_summary(na_example_filled)

# Print summary results
cat("Summary statistics for na_example_filled:\n")
print(summary_results)


