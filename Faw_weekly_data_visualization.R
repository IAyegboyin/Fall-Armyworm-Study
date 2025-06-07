# Introduction----
# 01-06-2025 
# Ismail Ayomide Ayegboyin (ayegboyinismai@gmail.com)
# This data was shared by Dr. M. D. Akinbuluma to check for some trends in the data and the work is still in progress .....


# Extract blend codes from treatment (R1, R2, or Control)
weekly_long <- faw.weekly_clean %>%
  pivot_longer(cols = starts_with("Week"), names_to = "Week", values_to = "Count")

daily_long <- faw.daily_clean  %>%
  pivot_longer(cols = starts_with("Day"), names_to = "Day", values_to = "Count") %>%
  mutate(Day = as.numeric(str_remove(Day, "Day ")))
view(weekly_long)


# First ensure your Week column is numeric in weekly_summary
weekly_summary <- weekly_long %>%
  mutate(Week = as.numeric(str_remove(Week, "Week "))) %>%
  group_by(Treatment, Week) %>%
  summarise(
    Mean = mean(Count),
    SEM = sd(Count)/sqrt(n()),
    .groups = 'drop'
  )

# Convert Day to Week for daily data (adjust days per week as needed)
daily_means <- daily_long %>%
  mutate(Week = ceiling(Day/7)) %>%  # 7 days per week
  group_by(Treatment, Week) %>%
  summarise(
    Daily_Mean = mean(Count),
    .groups = 'drop'
  )

ggplot() +
  # Weekly bars with SEM
  geom_col(
    data = weekly_summary,
    aes(x = Week, y = Mean, fill = Treatment),
    position = position_dodge(0.8),
    width = 0.7
  ) +
  geom_errorbar(
    data = weekly_summary,
    aes(x = Week, ymin = Mean - SEM, ymax = Mean + SEM, group = Treatment),
    position = position_dodge(0.8),
    width = 0.1
  ) +
  # Daily trendlines
  geom_line(
    data = daily_means,
    aes(x = Week, y = Daily_Mean, color = Treatment, group = Treatment),
    size = 0.5,
    position = position_dodge(0.5)
  ) +
  geom_point(
    data = daily_means,
    aes(x = Week, y = Daily_Mean, color = Treatment),
    position = position_dodge(0.8),
    size = 0.1
  ) +
  scale_x_continuous(breaks = 1:max(weekly_summary$Week),
                     labels = paste("Week", 1:max(weekly_summary$Week))) +
  labs(x = "Week", y = "Fall Armyworm Count") +
  theme_minimal()
