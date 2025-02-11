
# functions/data_utils.R

# Function to clean missing values from a dataset
clean_data <- function(df) {
  df[complete.cases(df), ]  # Removes rows with missing values
}

# Function to calculate mean and standard deviation
calc_summary <- function(df, column) {
  data <- df[[column]]
  list(mean = mean(data, na.rm = TRUE), sd = sd(data, na.rm = TRUE))
}
