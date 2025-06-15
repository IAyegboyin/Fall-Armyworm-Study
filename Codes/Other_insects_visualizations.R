# Introduction----
# 15-06-2025 
# Ismail Ayomide Ayegboyin (ayegboyinismai@gmail.com)
# This data was shared by Dr. M. D. Akinbuluma to check for some trends in the data and the work is still in progress .....
# Data preparation and manipulation ----
other.weekly_summary <- other.weekly_data %>%
  mutate(Week = as.numeric(str_remove(Week, "Week "))) %>%
  group_by(Treatment, Week) %>%
  summarise(
    Mean = mean(Count),
    SEM = sd(Count)/sqrt(n()),
    .groups = 'drop'
  )
other.daily_long <- other.daily_clean  %>%
  pivot_longer(cols = starts_with("Day"), names_to = "Day", values_to = "Count") %>%
  mutate(Day = as.numeric(str_remove(Day, "Day ")))

# Converting Day to Week for daily data (adjust days per week as needed)
other.daily_means <- other.daily_long %>%
  mutate(Week = ceiling(Day/7)) %>%  # 7 days per week
  group_by(Treatment, Week) %>%
  summarise(
    Daily_Mean = mean(Count),
    .groups = 'drop'
  )
# Plot for Mean + SEM and trend lines ----
ggplot() +
  # Weekly bars with SEM
  geom_col(
    data = other.weekly_summary,
    aes(x = Week, y = Mean, fill = Treatment),
    position = position_dodge(0.8),
    width = 0.7
  ) +
  geom_errorbar(
    data = other.weekly_summary,
    aes(x = Week, ymin = Mean - SEM, ymax = Mean + SEM, group = Treatment),
    position = position_dodge(0.8),
    width = 0.1
  ) +
  # Daily trendlines
  geom_line(
    data = other.daily_means,
    aes(x = Week, y = Daily_Mean, color = Treatment, group = Treatment),
    size = 0.5,
    position = position_dodge(0.5)
  ) +
  geom_point(
    data = other.daily_means,
    aes(x = Week, y = Daily_Mean, color = Treatment),
    position = position_dodge(0.8),
    size = 0.1
  ) +
  scale_x_continuous(breaks = 1:max(other.weekly_summary$Week),
                     labels = paste("Week", 1:max(other.weekly_summary$Week))) +
  labs(x = "Week", y = "Other Insects Count", title = "OTHER INSECT WEEKLY") +
  theme_minimal()

# Anova plot with HSD ----
# Create the anova and HSDtest plot here now (PER WEEK)
ggplot(other.weekly.results, aes(x = Week, y = mean, fill = Treatment)) +
  geom_col(position = position_dodge(0.9), width = 0.8) +
  geom_text(aes(label = groups, y = mean + max(mean)*0.05), 
            position = position_dodge(0.9), size = 4) +
  labs(x = "Week", y = "FAW Count", title = "WEEKLY ANOVA FOR OTHER INSECTS") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

# Creating image for the general ANOVA and also the weekly data (Mean + SEM) summary now
other.insect.treatment_means <- other.anova %>%
  group_by(Treatment) %>%
  summarise(mean_count = mean(Count), 
            se = sd(Count)/sqrt(n()))

# Merge the treatment mean with HSD groupings
other.insect.treatment_means <- treatment_means %>%
  left_join(
    other.anova.m.hsd$groups %>%
      as.data.frame() %>%
      rownames_to_column("Treatment") %>%
      rename(hsd_group = groups),
    by = "Treatment"
  )

# Ploting the bar chart, + SEM with HSD letters
ggplot(treatment_means, aes(x = Treatment, y = mean_count, fill = Treatment)) +
  geom_bar(stat = "identity", color = "black", width = 0.3) +
  geom_errorbar(aes(ymin = mean_count - se, ymax = mean_count + se), 
                width = 0.1) +
  geom_text(aes(label = hsd_group, y = mean_count + se + 0.1*max(mean_count)), 
            vjust = 0) +
  labs(title = "Other Insect Count with ANOVA",
       y = "Mean Count",
       x = "Treatment") +
  theme_minimal() +
  theme(legend.position = "none")
