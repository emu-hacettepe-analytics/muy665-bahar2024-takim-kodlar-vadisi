# Define the custom_summary function
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

# Load the mtcars dataset
data(mtcars)

# Display the first few rows of the mtcars dataset
head(mtcars)

# Create an empty list to store summary results for each column
summary_results <- list()

# Iterate over each column of the mtcars dataset
for (col in colnames(mtcars)) {
  # Apply custom_summary function to each column
  summary_results[[col]] <- custom_summary(mtcars[[col]])
}

# Print summary results for each column
for (i in seq_along(summary_results)) {
  col_name <- names(summary_results)[i]
  cat("Summary statistics for column", col_name, ":\n")
  print(summary_results[[i]])
  cat("\n")
}
#summary_results <- custom_summary(as.vector(as.matrix(mtcars)))
