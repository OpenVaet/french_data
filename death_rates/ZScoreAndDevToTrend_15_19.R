# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)

# Defines the data as a named vector
data <- c(
`2010` = 0.30730747135141, `2011` = 0.290240607375452, `2012` = 0.274295520756791, `2013` = 0.243078692480775, `2014` = 0.232006869922881,
`2015` = 0.264874341640685, `2016` = 0.237498019783483, `2017` = 0.225024218708285, `2018` = 0.228124589262432, `2019` = 0.243017306874221,
`2020` = 0.227112778322222, `2021` = 0.217851712606427, `2022` = 0.233240656074768, `2023` = 0.233436616558388
)

# Extracts data for the decade and recent years
decade <- data[as.character(2010:2019)]
recent_years <- data[as.character(2020:2023)]

# Fits a linear model to the decade data
years <- as.numeric(names(decade))
lm_model <- lm(decade ~ years)

# Predicts values for 2020 to 2023 based on the linear model
predicted_values <- predict(lm_model, newdata = data.frame(years = as.numeric(names(recent_years))))

# Calculates deviations from the linear trend
deviations <- recent_years - predicted_values

# Calculates the mean and standard deviation of the decade
decade_mean <- mean(decade)
decade_stddev <- sd(decade)

# Initializes vectors to store the calculated values
z_scores <- numeric(length(recent_years))
percent_deviations <- numeric(length(recent_years))

# Calculates and store the Z-scores, deviations, and percentage of deviations for recent years
for (i in 1:length(recent_years)) {
  year <- names(recent_years)[i]
  z_scores[i] <- (recent_years[year] - decade_mean) / decade_stddev
  deviation <- deviations[year]
  percent_deviations[i] <- (deviation / predicted_values[i]) * 100
  cat(sprintf("Year: %s, Z-score: %f, Deviation from linear trend: %f, Percentage of deviation: %f%%\n",
              year, z_scores[i], deviation, percent_deviations[i]))
}

# Prepare an extended range for predictions, including 2020 to 2023
extended_years <- as.numeric(c(names(decade), names(recent_years)))

# Predict values for the extended range based on the linear model
extended_predicted_values <- predict(lm_model, newdata = data.frame(years = extended_years))

# Create a data frame for plotting actual values
actual_plot_data <- data.frame(
  Year = as.numeric(names(data)),
  Value = data,
  Type = "Actual"
)

# Create a data frame for plotting predicted values
predicted_plot_data <- data.frame(
  Year = extended_years,
  Value = extended_predicted_values,
  Type = "Predicted"
)

# Combine actual and predicted data
plot_data <- rbind(actual_plot_data, predicted_plot_data)

# Create the line chart
p <- ggplot(plot_data, aes(x = Year, y = Value, color = Type)) +
  geom_line(aes(group = Type)) +
  geom_point(data = actual_plot_data, aes(x = Year, y = Value)) +
  theme_minimal() +
  labs(title = "France, Deaths/1000 among 15-19 years old, 2020 to 2023 compared to 2010-2019 Linear Trend", x = "Year", y = "Value") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  scale_y_continuous(labels = scales::comma, limits = c(0, max(plot_data$Value))) +
  scale_x_continuous(breaks = extended_years) # Set breaks to every year

# Add percentage of deviation annotations for 2020 to 2023
for (i in 1:length(recent_years)) {
  year <- as.numeric(names(recent_years)[i])
  deviation_value <- percent_deviations[i]
  p <- p + annotate("text", x = year, y = data[as.character(year)], label = paste0(round(deviation_value, 2), "%"), vjust = -1.5)
}

# Print the plot
print(p)