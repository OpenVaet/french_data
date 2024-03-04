# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)

# Defines the data as a named vector
data <- c(
`2010` = 0.561703011879699, `2011` = 0.556497685047101, `2012` = 0.521331244894379, `2013` = 0.496939116588717, `2014` = 0.501824900551565,
`2015` = 0.482734817586841, `2016` = 0.481055028671246, `2017` = 0.461430778779995, `2018` = 0.458654491184661, `2019` = 0.468912332096182,
`2020` = 0.446369242446454, `2021` = 0.458605469174115, `2022` = 0.475419614605356, `2023` = 0.457413244833492
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
  labs(title = "France, Deaths/1000 among 25-29 years old, 2020 to 2023 compared to 2010-2019 Linear Trend", x = "Year", y = "Value") +
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