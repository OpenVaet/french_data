# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)

# Defines the data as a named vector
data <- c(
`2010` = 51.4302028464163, `2011` = 48.6700736727223, `2012` = 50.0808773951832, `2013` = 47.9235591154449, `2014` = 45.8877666018423,
`2015` = 47.1151118145236, `2016` = 45.6929349148123, `2017` = 44.9167585151808, `2018` = 44.1342090358559, `2019` = 43.4385312613001,
`2020` = 47.2222841746871, `2021` = 45.6152881091932, `2022` = 45.6009741928428, `2023` = 42.6204448369865
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
  labs(title = "France, Deaths/1000 among 80-84 years old, 2020 to 2023 compared to 2010-2019 Linear Trend", x = "Year", y = "Value") +
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