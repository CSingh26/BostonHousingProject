# Load necessary libraries
library(dplyr)

# Read the dataset from the CSV file
data <- read.csv("boston.csv")

# Log transform the target variable
data$MEDV <- log(data$MEDV)  # Swapped 'PRICE' with 'MEDV'

# Define constants
CRIME_IDX <- 1
ZN_IDX <- 2
CHAS_IDX <- 3
RM_IDX <- 5
PTRATIO_IDX <- 9

ZILLOW_MEDIAN_PRICE <- 583.3

# Calculate SCALE_FACTOR
SCALE_FACTOR <- ZILLOW_MEDIAN_PRICE / median(data$MEDV)  # Swapped 'PRICE' with 'MEDV'

# Create property_stats as a row of mean values
property_stats <- as.data.frame(t(colMeans(data)))

# Fit a linear regression model
regr <- lm(MEDV ~ ., data = data)  # Swapped 'PRICE' with 'MEDV'

# Get the fitted values
fitted_vals <- predict(regr, newdata = data)

# Calculate MSE and RMSE
MSE <- mean((data$MEDV - fitted_vals)^2)
RMSE <- sqrt(MSE)

# Define a function to get the log estimate
get_log_estimate <- function(nr_rooms, students_per_classroom, next_to_river = FALSE, high_confidence = TRUE) {
  # Configure property
  property_stats[1, RM_IDX] <- nr_rooms
  property_stats[1, PTRATIO_IDX] <- students_per_classroom
  property_stats[1, CHAS_IDX] <- as.numeric(next_to_river)
  
  # Make prediction
  log_estimate <- predict(regr, newdata = property_stats)
  
  # Calculate range
  if (high_confidence) {
    upper_bound <- log_estimate + 2 * RMSE
    lower_bound <- log_estimate - 2 * RMSE
    interval <- 95
  } else {
    upper_bound <- log_estimate + RMSE
    lower_bound <- log_estimate - RMSE
    interval <- 68
  }
  
  return(list(log_estimate = log_estimate, upper_bound = upper_bound, lower_bound = lower_bound, interval = interval))
}

# Define a function to get the dollar estimate
get_dollar_estimate <- function(rm, ptratio, chas = FALSE, large_range = TRUE) {
  if (rm < 1 || ptratio < 1) {
    cat("That is unrealistic. Try Again\n")
    return(invisible(NULL))
  }
  
  estimates <- get_log_estimate(rm, students_per_classroom = ptratio, next_to_river = chas, high_confidence = large_range)
  
  # Convert to today's dollar
  dollar_est <- exp(estimates$log_estimate) * 1000 * SCALE_FACTOR
  dollar_hi <- exp(estimates$upper_bound) * 1000 * SCALE_FACTOR
  dollar_low <- exp(estimates$lower_bound) * 1000 * SCALE_FACTOR
  
  # Round the dollar values to the nearest thousand
  rounded_est <- round(dollar_est, -3)
  rounded_hi <- round(dollar_hi, -3)
  rounded_low <- round(dollar_low, -3)
  
  cat(paste("The estimated value is", rounded_est, "\n"))
  cat(paste("At", estimates$interval, "% the valuation range is\n"))
  cat(paste("USD", rounded_low, "at the lower end to USD", rounded_hi, "at the high end.\n"))
}
