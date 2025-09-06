# Plot 1 ----
# Data preparation and manipulation
weekly_long <- faw.weekly_clean %>%
  pivot_longer(cols = starts_with("Week"), names_to = "Week", values_to = "Count")

daily_long <- faw.daily_clean  %>%
  pivot_longer(cols = starts_with("Day"), names_to = "Day", values_to = "Count") %>%
  mutate(Day = as.numeric(str_remove(Day, "Day ")))
view(weekly_long)


weekly_summary <- weekly_long %>%
  mutate(Week = as.numeric(str_remove(Week, "Week "))) %>%
  group_by(Treatment, Week) %>%
  summarise(
    Mean = mean(Count),
    SEM = sd(Count)/sqrt(n()),
    .groups = 'drop'
  )

# Converting Day to Week for daily data (adjust days per week as needed)
daily_means <- daily_long %>%
  mutate(Week = ceiling(Day/7)) %>%  # 7 days per week
  group_by(Treatment, Week) %>%
  summarise(
    Daily_Mean = mean(Count),
    .groups = 'drop'
  )
# Define a consistent color palette for all treatments
treatment_colors <- c(
  "Blend1" = "#1F77B4",  # Blue
  "Blend2" = "#FF7F0E",  # Orange
  "Blend3" = "#2CA02C",  # Green
  "Blend4" = "#D62728"   # Red
)
# Create the plot with axis lines and horizontal grid
ggplot() +
  # 1. Bar chart with SEM error bars
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
    width = 0.2,
    color = "black",
    linewidth = 0.5
  ) +
  # 2. Trendline with points
  geom_line(
    data = daily_means,
    aes(x = Week, y = Daily_Mean, group = Treatment, color = Treatment),
    position = position_dodge(0.8),
    linewidth = 0.8,
    show.legend = FALSE
  ) +
  geom_point(
    data = daily_means,
    aes(x = Week, y = Daily_Mean, group = Treatment, color = Treatment),
    position = position_dodge(0.8),
    size = 2,
    show.legend = FALSE
  ) +
  # Color scales
  scale_fill_manual(
    name = "Treatment",
    values = treatment_colors,
    labels = c(
      "Blend 1: Z9-14:OAC + Z7-12:OAC", 
      "Blend 2: Z9-14:OAC + Z7-12:OAC + \nZ11-16:OAC + Z9-12:OAC + E7-12:OAC", 
      "Blend 3: Z9-14:OAC + E7-12:OAC", 
      "N-Hexane"
    )
  ) +
  scale_color_manual(values = treatment_colors) +
  # Axis settings
  scale_x_continuous(
    breaks = 1:max(weekly_summary$Week),
    labels = paste("Week", 1:max(weekly_summary$Week))
  ) +
  labs(x = " ", 
       y = "Average Count of Male Fall Armyworm" ) +
     #  title = "Attraction of Male Fall Armyworm to Different Pheromone Blends Across Weeks") +
  theme_minimal() +
  theme(
    # Axis lines
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    
    # Grid lines (horizontal only)
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    
    # Legend customization
    legend.position = c(0.75, 0.75),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.text = element_text(size = 12),
    legend.key.height = unit(0.8, "cm"),
    
    # Plot margins
    plot.margin = margin(15, 15, 15, 15)
  ) 
# Plot 2 ----
# Prepare data from your Kruskal-Wallis output
# 1. Extract data from kruskal_test object
plot_data <- data.frame(
  Treatment = factor(c("Blend1", "Blend2", "Blend3", "Blend4"), 
                     levels = c("Blend1", "Blend2", "Blend3", "Blend4")),
  Mean = c(9.33, 6.67, 0.58, 1.00),
  Group = c("a", "b", "c", "c")  # Significance letters
)

# Calculate letter positions (fixed offset above max value)
y_max <- max(plot_data$Mean)
plot_data$LetterHeight <- plot_data$Mean + (0.1 * y_max)  # 10% of max value as offset

# Create the plot
ggplot(plot_data, aes(x = Treatment, y = Mean, fill = Treatment)) +
  geom_col(width = 0.4, color = "black", alpha = 0.8) +
  geom_text(aes(y = LetterHeight, label = Group), 
            size = 5, fontface = "bold", vjust = 0) +  # vjust=0 places text above the bar
  
  # Custom colors and labels
  scale_fill_manual(
    name = "Treatment",
    values = c("Blend1" = "#1F77B4", "Blend2" = "#FF7F0E",
               "Blend3" = "#2CA02C", "Blend4" = "#D62728"),
    labels = c(
      "Blend 1: Z9-14:OAC + Z7-12:OAC", 
      "Blend 2: Z9-14:OAC + Z7-12:OAC + \nZ11-16:OAC + Z9-12:OAC + E7-12:OAC", 
      "Blend 3: Z9-14:OAC + E7-12:OAC", 
      "N-Hexane"
    )
  ) +
  
  # Labels and titles
  labs(x = " ", 
       y = "Average Count of Male Fall Armyworm",
       caption = "Different letters indicate significant differences (Kruskal-Wallis test, p < 0.05)") +
  theme_minimal() +
  theme(
    # Axis lines
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    
    # Grid lines (horizontal only)
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    
    # Legend customization
    legend.position = c(0.75, 0.75),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.text = element_text(size = 12),
    legend.key.height = unit(0.8, "cm"),
    
    # Plot margins
    plot.margin = margin(15, 15, 15, 15)
  ) 
  
  # Ensure y-axis starts at 0 and has some headroom
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))  # 10% headroom at top
# Plot 3 ----
    # Read and prepare data
  data <- other.weekly_data
  view(data)
  
  group_letters <- non.target.weekly.kruskal.results %>%
    dplyr::select(, -mean)
 #  view(group_letters)
  # Calculate summary statistics
  summary_data <- data %>%
    group_by(Week, Treatment) %>%
    summarise(
      Mean = mean(Count),
      SEM = sd(Count)/sqrt(n()),
      .groups = "drop"
    ) %>%
    left_join(group_letters, by = c("Week", "Treatment"))
  
  # Create the plot (same visualization code as before)
  ggplot(summary_data, aes(x = Week, y = Mean, fill = Treatment)) +
    geom_col(position = position_dodge(0.8), width = 0.7, color = "black") +
    # geom_errorbar(
     #  aes(ymin = pmax(Mean - SEM, 0), ymax = Mean + SEM),
     #  position = position_dodge(0.8),
      # width = 0.2,
      # color = "black",
      # linewidth = 0.5
    # ) +
    geom_text(
      aes(y = Mean + 0.3, label = groups),
      position = position_dodge(0.8),
      vjust = -0.5,
      size = 4,
      fontface = "bold"
    ) +
    scale_fill_manual(
      values = c("Blend1" = "#1F77B4", "Blend2" = "#FF7F0E",
                 "Blend3" = "#2CA02C", "Blend4" = "#D62728"),
      labels = c(
        "Blend 1: Z9-14:OAC + Z7-12:OAC", 
        "Blend 2: Z9-14:OAC + Z7-12:OAC + \nZ11-16:OAC + Z9-12:OAC + E7-12:OAC", 
        "Blend 3: Z9-14:OAC + E7-12:OAC", 
        "n-Hexane"
      )
    ) +
    labs(
     # title = "Weekly Non-Target Insect  by Treatment",
    # subtitle = "Kruskal-Wallis with agricolae grouping (α = 0.05)",
      x = " ",
      y = "Average Count of Non-Target Insect"
    ) +
    theme_minimal() +
    theme(
      # Axis lines
      axis.line = element_line(color = "black", linewidth = 0.5),
      axis.line.y = element_line(color = "black", linewidth = 0.5),
      axis.line.x = element_line(color = "black", linewidth = 0.5),
      
      # Grid lines (horizontal only)
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor.y = element_blank(),
      
      # Legend customization
      legend.position = c(0.75, 0.75),
      legend.background = element_rect(fill = "white", color = "black"),
      legend.text = element_text(size = 12),
      legend.key.height = unit(0.8, "cm"),
      
      # Plot margins
      plot.margin = margin(15, 15, 15, 15)
    )
  
# Plot 4 ----
  # Data preparation and manipulation
  weekly_long_2 <- faw.weekly_clean_2 %>%
    pivot_longer(cols = starts_with("Week"), names_to = "Week", values_to = "Count")
  
  daily_long_2 <- faw.daily_clean_2  %>%
    pivot_longer(cols = starts_with("Day"), names_to = "Day", values_to = "Count") %>%
    mutate(Day = as.numeric(str_remove(Day, "Day ")))
  view(weekly_long_2)
  
  
  weekly_summary_2 <- weekly_long_2 %>%
    mutate(Week = as.numeric(str_remove(Week, "Week "))) %>%
    group_by(Treatment, Week) %>%
    summarise(
      Mean = mean(Count),
      SEM = sd(Count)/sqrt(n()),
      .groups = 'drop'
    )
  
  # Converting Day to Week for daily data (adjust days per week as needed)
  daily_means_2 <- daily_long_2 %>%
    mutate(Week = ceiling(Day/7)) %>%  # 7 days per week
    group_by(Treatment, Week) %>%
    summarise(
      Daily_Mean = mean(Count),
      .groups = 'drop'
    )
  # Define a consistent color palette for all treatments
  
  treatment_colors_2 <- c(
    "Blend1" = "#4477AA",
    "Blend2" = "#EE6677",
    "Blend3" = "#228833",
    "n-Hexane" = "#CCBB44",
    "Female" = "#AA3377"
  )
  
  # Create the plot with axis lines and horizontal grid
  ggplot() +
    # 1. Bar chart with SEM error bars
    geom_col(
      data = weekly_summary_2,
      aes(x = Week, y = Mean, fill = Treatment),
      position = position_dodge(0.8),
      width = 0.7
    ) +
    geom_errorbar(
      data = weekly_summary_2,
      aes(x = Week, ymin = Mean - SEM, ymax = Mean + SEM, group = Treatment),
      position = position_dodge(0.8),
      width = 0.2,
      color = "black",
      linewidth = 0.5
    ) +
    # 2. Trendline with points
    geom_line(
      data = daily_means_2,
      aes(x = Week, y = Daily_Mean, group = Treatment, color = Treatment),
      position = position_dodge(0.8),
      linewidth = 0.8,
      show.legend = FALSE
    ) +
    geom_point(
      data = daily_means_2,
      aes(x = Week, y = Daily_Mean, group = Treatment, color = Treatment),
      position = position_dodge(0.8),
      size = 1,
      show.legend = FALSE
    ) +
    # Color scales
    scale_fill_manual(
      name = "Treatment",
      values = treatment_colors_2,
      labels = c(
        "Blend 1: Z9-14:OAC + Z7-12:OAC", 
        "Blend 2: Z9-14:OAC + Z7-12:OAC + \nZ11-16:OAC + Z9-12:OAC + E7-12:OAC", 
        "Blend 3: Z9-14:OAC + E7-12:OAC", 
        "Female",
        "n-Hexane"
      )
    ) +
    scale_color_manual(values = treatment_colors_2) +
    # Axis settings
    scale_x_continuous(
      breaks = 1:max(weekly_summary$Week),
      labels = paste("Week", 1:max(weekly_summary$Week))
    ) +
    labs(x = " ", 
         y = "Average Count of Male Fall Armyworm" ) +
    #  title = "Attraction of Male Fall Armyworm to Different Pheromone Blends Across Weeks") +
    theme_minimal() +
    theme(
      # Axis lines
      axis.line = element_line(color = "black", linewidth = 0.5),
      axis.line.y = element_line(color = "black", linewidth = 0.5),
      axis.line.x = element_line(color = "black", linewidth = 0.5),
      
      # Grid lines (horizontal only)
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor.y = element_blank(),
      
      # Legend customization
      legend.position = c(0.75, 0.75),
      legend.background = element_rect(fill = "white", color = "black"),
      legend.text = element_text(size = 12),
      legend.key.height = unit(0.8, "cm"),
      
      # Plot margins
      plot.margin = margin(15, 15, 15, 15)
    ) 
# Plot 5 ----
  # 1. Create the correct plot_data (with 5 treatments)
  plot_data <- data.frame(
    Treatment = factor(c("Blend1", "Blend2", "Blend3", "Female", "n-Hexane"), 
                       levels = c("Blend1", "Blend2", "Blend3", "Female", "n-Hexane")),
    Mean = c(2.33, 2.50, 0.92, 2.71, 0.25),
    Group = c("a", "a", "b", "a", "c")
  )
  
  # 2. Calculate letter positions (using the correct dataframe)
  y_max <- max(plot_data$Mean)
  plot_data$LetterHeight <- plot_data$Mean + (0.1 * y_max)  # 10% of max value as offset
  
  # 3. Define colors (make sure treatment_colors_2 is properly defined)
  treatment_colors_2 <- c(
    "Blend1" = "#4477AA",
    "Blend2" = "#EE6677",
    "Blend3" = "#228833",
    "n-Hexane" = "#CCBB44",
    "Female" = "#AA3377"
  )
  
  # 4. Create the plot
  ggplot(plot_data, aes(x = Treatment, y = Mean, fill = Treatment)) +
    geom_col(width = 0.4, color = "black", alpha = 0.8) +
    geom_text(aes(y = LetterHeight, label = Group), 
              size = 5, fontface = "bold", vjust = 0) +
    
    scale_fill_manual(
      name = "Treatment",
      values = treatment_colors_2,
      labels = c(
        "Blend 1: Z9-14:OAC + Z7-12:OAC", 
        "Blend 2: Z9-14:OAC + Z7-12:OAC +\nZ11-16:OAC + Z9-12:OAC + E7-12:OAC", 
        "Blend 3: Z9-14:OAC + E7-12:OAC", 
        "Female",
        "n-Hexane"
      )
    ) +
    
    labs(x = NULL, 
         y = "Average Count of Male Fall Armyworm",
         caption = "Different letters indicate significant differences (Kruskal-Wallis test, p < 0.05)") +
    
    theme_minimal() +
    theme(
      axis.line = element_line(color = "black", linewidth = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      legend.position = c(0.85, 0.87),
      legend.background = element_rect(fill = "white", color = "black"),
      legend.text = element_text(size = 10)  # Slightly smaller for multi-line labels
    ) +
    
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)))  # Slightly more headroom for letters
# Plot 6 ----
  # Read and prepare data
  data_2 <- other.weekly_data_2
  # view(data_2)
  
  group_letters_2 <- non.target.weekly.kruskal.results_2 %>%
    dplyr::select(, -mean)
  #  view(group_letters)
  # Calculate summary statistics
  summary_data_2 <- data_2 %>%
    group_by(Week, Treatment) %>%
    summarise(
      Mean = mean(Count),
      SEM = sd(Count)/sqrt(n()),
      .groups = "drop"
    ) %>%
    left_join(group_letters_2, by = c("Week", "Treatment"))
  
  # Create the plot (same visualization code as before)
  ggplot(summary_data_2, aes(x = Week, y = Mean, fill = Treatment)) +
    geom_col(position = position_dodge(0.8), width = 0.7, color = "black") +
    # geom_errorbar(
    #  aes(ymin = pmax(Mean - SEM, 0), ymax = Mean + SEM),
    #  position = position_dodge(0.8),
    # width = 0.2,
    # color = "black",
    # linewidth = 0.5
    # ) +
    geom_text(
      aes(y = Mean + 0.3, label = groups),
      position = position_dodge(0.8),
      vjust = -0.5,
      size = 4,
      fontface = "bold"
    ) +
    scale_fill_manual(
      name = "Treatment",
      values = treatment_colors_2,
      labels = c(
        "Blend 1: Z9-14:OAC + Z7-12:OAC", 
        "Blend 2: Z9-14:OAC + Z7-12:OAC +\nZ11-16:OAC + Z9-12:OAC + E7-12:OAC", 
        "Blend 3: Z9-14:OAC + E7-12:OAC", 
        "Female",
        "n-Hexane"
      )
    ) +
    labs(
      # title = "Weekly Non-Target Insect  by Treatment",
      # subtitle = "Kruskal-Wallis with agricolae grouping (α = 0.05)",
      x = " ",
      y = "Average Count of Non-Target Insect"
    ) +
    theme_minimal() +
    theme(
      # Axis lines
      axis.line = element_line(color = "black", linewidth = 0.5),
      axis.line.y = element_line(color = "black", linewidth = 0.5),
      axis.line.x = element_line(color = "black", linewidth = 0.5),
      
      # Grid lines (horizontal only)
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor.y = element_blank(),
      
      # Legend customization
      legend.position = c(0.90, 0.90),
      legend.background = element_rect(fill = "white", color = "black"),
      legend.text = element_text(size = 12),
      legend.key.height = unit(0.8, "cm"),
      
      # Plot margins
      plot.margin = margin(15, 15, 15, 15)
    )
  
  df_total <- weekly_data %>%
    group_by(Treatment, Count) %>%
    summarise(total_catch = sum(Count), .groups = "drop")
  
  ggplot(df_total, aes(x = Treatment, y = total_catch, fill = Treatment)) +
    geom_boxplot() +
    labs(title = "Total FAW Catch per Treatment",
         x = "Treatment", y = "Total Moths Caught") +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggplot(weekly_data, aes(x = Week, y = Treatment, size = Count, fill = Count)) +
    geom_point(shape = 21, color = "black", alpha = 0.8) +
    scale_size(range = c(2, 15)) +
    scale_fill_viridis_c(option = "C") +
    labs(title = "Visual cue of weekly dynamics per treatment",
         x = "Week", y = "Treatment", size = "Count") +
    theme_minimal()
  
  ggplot(weekly_data, aes(x = Count, y = reorder(Treatment, Count), fill = Treatment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ Week, ncol = 2) +
    labs(title = "FAW Trap Counts by Treatment per Week",
         x = "Moths Caught", y = "Treatment") +
    theme_minimal()
  
  ggplot(weekly_data, aes(x = Week, y = Treatment, fill = Count)) +
    geom_tile(color = "white") +
   
    scale_fill_viridis_c(option = "C") +
    labs(title = "FAW Trap Counts with Labels",
         x = "Week", y = "Treatment") +
    theme_minimal()
  
  
  
  
# Seasonal Variation plots ----
  seasonal_difference_plot <- faw.season %>%
    mutate(Week = as.numeric(str_remove(Week, "Week "))) %>%
    group_by(season) %>%
    summarise(
    Mean = mean(Count),
    SEM = sd(Count)/sqrt(n()),
    .groups = 'drop'
  )

ggplot() +
  geom_errorbar(
    data = seasonal_difference_plot,
    aes(x = season, ymin = Mean - SEM, ymax = Mean + SEM),
    position = position_dodge(0.8),
    width = 0.2,
    color = "black",
    linewidth = 0.5
  ) +
  geom_col(
    data = seasonal_difference_plot,
    aes(x = season, y = Mean, fill = season),
    position = position_dodge(0.8),
    width = 0.3
  ) +
  labs(x = NULL, 
       y = "Average Count of Male Fall Armyworm") +
  scale_fill_manual(
    name = "Season",
    values = c("season 1" = "steelblue", "season 2" = "tomato"), # pick your colors
    labels = c("Season 1", "Season 2")
  ) +
  theme_minimal() + 
  theme(
    # Axis lines
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    
    # Grid lines (horizontal only)
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    
    # Legend customization
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.text = element_text(size = 12),
    legend.key.height = unit(0.8, "cm"),
    
    # Plot margins
    plot.margin = margin(15, 15, 15, 15)
  ) 
  