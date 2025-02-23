
# R-Script: Descriptive Statistics for Categorical Variables

# Load necessary libraries
library(dplyr)
library(tidyr)
library(here)
library(rstudioapi)







# Helper function to calculate summary statistics for a single categorical variable.
# 
# Args:
#   variable: String as variable name.
#
# Returns:
#   data frame.
internal_categorical_summary <- function(variable) {
  # Calculate frequency table
  freq_table <- table(variable)
  
  # Calculate proportions
  proportions <- prop.table(freq_table)
  
  # Create a summary data frame
  summary_df <- data.frame(
    Category = names(freq_table),
    Frequency = as.integer(freq_table),
    Proportion = as.numeric(proportions)
  )
  
  return(summary_df)
}







# Main function to calculate descriptive statistics for categorical variables
# 
# Args:
#   data: Input data as data frame.
#   categorical_vars: Vector with Strings as variable names.
#
# Returns:
#   data frame.
calculate_categorical_stats <- function(data, categorical_vars) {
  # Check if the provided variables exist in the data frame
  if (!all(categorical_vars %in% names(data))) {
    stop("Some categorical variables are not in the data frame.")
  }
  
  # Initialize a list to store results
  stats_list <- list()
  
  # Loop through each categorical variable
  for (var in categorical_vars) {
    # Calculate statistics using the internal helper function
    stats <- internal_categorical_summary(data[[var]])
    stats_list[[var]] <- stats
  }
  
  # Convert list to data frame for better readability
  stats_df <- bind_rows(stats_list, .id = "Variable")
  
  return(stats_df)
}




run_example <- function() {
    # Load example data
    # Read processed Titanic dataset
    # File is located in /data/processed/titanic_cleaned.csv
    file_path <- here("data", "processed", "titanic_cleaned.csv")
    titanic <- read.csv(file_path, stringsAsFactors = FALSE)

    # Call the function for categorical variables
    result <- calculate_categorical_stats(titanic, categorical_vars = c("Pclass", "Embarked"))

    # Print the result
    print(result)
}

# run_example()