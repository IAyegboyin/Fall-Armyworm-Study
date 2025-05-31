# Library required ----
library(tidyverse)

# Plot mean insect count over time for each blend ----

ggplot(farm_1, aes(x = Day, y = Count, color = Blend)) +
  stat_summary(fun = mean, geom = "line", size = 0.7) +
  stat_summary(fun.data = mean_se, geom = "ribbon", aes(fill = Blend), alpha = 0.2, color = NA) +
  labs(title = paste("Insect Capture Trends on First Farm"),
       x = "Day", y = "Mean Insect Count") +
  theme_minimal()

ggplot(farm_2, aes(x = Day, y = Count, color = Blend)) +
  stat_summary(fun = mean, geom = "line", size = 0.7) +
  stat_summary(fun.data = mean_se, geom = "ribbon", aes(fill = Blend), alpha = 0.2, color = NA) +
  labs(title = paste("Insect Capture Trends on Second Farm"),
       x = "Day", y = "Mean Insect Count") +
  theme_minimal()

# Histogram of count data showing the data is zero inflated ----

ggplot(farm_1, aes(x = Count)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white") +
  labs(title = "Histogram of Insect Counts for First Farm")

ggplot(farm_2, aes(x = Count)) +
  geom_histogram(bins = 15, fill = "green", color = "white") +
  labs(title = "Histogram of Insect Counts for Second Farm")

# Interaction plots for both farms starts here ----
interaction.plot(farm_1$Day, farm_1$Blend, farm_1$Count,
                 legend = TRUE, col = 1:4, lty = 1, lwd = 2,
                 main = "Interaction: Blend × Day (First Farm)",
                 ylab = "Mean FAW Count",
                 xlab = "Number of Days",
                 trace.label = "Blends")

interaction.plot(farm_2$Day, farm_2$Blend, farm_2$Count,
                 legend = TRUE, col = 1:4, lty = 1, lwd = 2,
                 main = "Interaction: Blend × Day (Second Farm)",
                 ylab = "Mean FAW Count",
                 xlab = "Number of Days",
                 trace.label = "Blends")

# Checking if replicates within the same blend behave similarly for the two reps in both farms 
ggplot(farm_1, aes(x = Day, y = Count, group = Replicate, color = Blend)) +
  geom_line(alpha = 0.8) +
  facet_wrap(~ Blend) +
  labs(title = "Replicate Trends Within Blends On First Farm")

ggplot(farm_2, aes(x = Day, y = Count, group = Replicate, color = Blend)) +
  geom_line(alpha = 0.8) +
  facet_wrap(~ Blend) +
  labs(title = "Replicate Trends Within Blends On Second Farm")
