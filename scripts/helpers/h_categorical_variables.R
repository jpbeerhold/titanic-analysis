

# Internal helper function to calculate summary statistics for a single categorical variable.
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


