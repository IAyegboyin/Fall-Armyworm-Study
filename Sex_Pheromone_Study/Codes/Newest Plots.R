plot_data <- data.frame(
  Treatment = factor(c("Minimal Z-Blend", "Full Blend", "Minimal E-Blend", "Control"), 
                     levels = c("Full Blend", "Minimal Z-Blend", "Minimal E-Blend", "Control")),
  Mean = c(9.33, 6.67, 0.58, 1.00),
  SE = c(1.25, 1.10, 0.25, 0.35),  # Standard errors
  Group = c("a", "b", "c", "c")
)

# Calculate dynamic position for letters
y_max <- max(plot_data$Mean + plot_data$SE)
plot_data$LetterHeight <- plot_data$Mean + plot_data$SE + (0.05 * y_max)

# Define my colours
grey_palette <- c( "#FFC000", "#C65B13", "#FEFF01", "#F2F2F2")

ggplot(plot_data, aes(x = Treatment, y = Mean, fill = Treatment)) +
  geom_col(width = 0.3, color = "black", alpha = 0.9) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                width = 0.1, linewidth = 0.6, color = "black") +
  geom_text(aes(y = LetterHeight, label = Group),
            size = 10, #fontface = "bold", 
            vjust = 0) +
  scale_fill_manual(
    # name = "Treatment",
    values = grey_palette
    # labels = c(
    #"Minimal Z-Blend", 
    #  "Full Blend", 
    #  "Minimal E-Blend", 
    #    "Control)
  ) +
  scale_y_continuous(
    limits = c(0, 12)
  ) +
  labs(x = " ", 
       y = "Mean Fall Armyworm Male/Day") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    axis.line = element_line(color = "black", linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "none",
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20, face = "bold"),
    legend.key.height = unit(0.8, "cm"),
    plot.margin = margin(15, 15, 15, 15),
    plot.caption = element_text(size = 16, face = "bold")
  )


# Relabel treatments for the weekly summary for season 1 
weekly_summary$Treatment <- recode(
  weekly_summary$Treatment,
  "Blend1" = "Minimal Z-Blend",
  "Blend2" = "Full Blend",
  "Blend3" = "Minimal E-Blend",
  "Blend4" = "Control"
)
# Setting factors here 
weekly_summary$Treatment <- factor(
  weekly_summary$Treatment,
  levels = c( "Full Blend", "Minimal Z-Blend", "Minimal E-Blend", "Control")
)

# Step 3: Define colors
grey_palette <- c(
  "Minimal Z-Blend" = "#C65B13",
  "Full Blend"      = "#FFC000",
  "Minimal E-Blend" = "#FEFF01",
  "Control"         = "#F2F1F1"
)

# Step 4: Plot
ggplot(weekly_summary, aes(x = Week, y = Mean, fill = Treatment)) +
  
  geom_col(
    position = position_dodge(0.8),
    width = 0.7,
    color = "black"
  ) +
  
  geom_errorbar(
    aes(
      ymin = Mean - SEM,
      ymax = Mean + SEM
    ),
    position = position_dodge(0.8),
    width = 0.2,
    color = "black",
    linewidth = 0.5
  ) +
  
  scale_fill_manual(
    name = "Treatment",
    values = grey_palette
  ) +
  
  scale_x_continuous(
    breaks = sort(unique(weekly_summary$Week))
  ) +
  
  labs(
    x = "Weeks",
    y = "Mean Fall Armyworm Male/Week"
  ) +
  
  theme_classic() +
  theme(
    axis.text.x  = element_text(size = 20, face = "bold"),
    axis.text.y  = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    
    axis.line = element_line(color = "black", linewidth = 0.5),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    legend.position = c(0.90, 0.75),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text  = element_text(size = 20, face = "bold"),
    legend.key.height = unit(0.8, "cm"),
    
    plot.margin = margin(15, 15, 15, 15)
  )


# Plot 3 starts here 
kruskal_summary <- data.frame(
  Treatment = c("Minimal Z-Blend", "Full Blend", "Minimal E-Blend", "Control"),
  Mean = c(1.54, 2.21, 0.50, 0.67),
  SEM = c(2.06, 3.37, 1.22, 1.13),
  Group = c("a", "a", "b", "ab")
)

# Ensure consistent factor order
kruskal_summary$Treatment <- factor(
  kruskal_summary$Treatment,
  levels = c("Full Blend", "Minimal Z-Blend", "Minimal E-Blend", "Control")
)

# Define color palette
grey_palette <- c(
  "Minimal Z-Blend" = "#C65B13",
  "Full Blend" = "#FFC000",
  "Minimal E-Blend" = "#FEFF01",
  "Control" = "#F2F2F2"
)

# ---------------------------
# 2️⃣ Plot 1: Letters very close to SEM bar
# ---------------------------

ggplot(kruskal_summary, aes(x = Treatment, y = Mean, fill = Treatment)) +
  geom_col(width = 0.3, color = "black", alpha = 1) +
  geom_errorbar(aes(ymin = Mean - SEM/3.5, ymax = Mean + SEM/3.5),
                width = 0.1, linewidth = 0.5, alpha = 1) +
  
  # Letters just barely above error bar
  geom_text(aes(y = Mean + SEM * 0.3, label = Group),
            size = 10, vjust = 0) +
  scale_fill_manual(values = grey_palette) +
  labs(
    x = " ",
    y = "Mean Non-target Insects/Day"
  ) +
  theme_classic(base_size = 13, base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    axis.line = element_line(color = "black", linewidth = 0.5),
    panel.grid.major.y = element_line(color = "white", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    legend.position = "none",
    plot.margin = margin(15, 15, 15, 15)
  )


# season two plots starts here -----

weekly_long_2 <- faw.weekly_clean_2 %>%
  pivot_longer(cols = starts_with("Week"), names_to = "Week", values_to = "Count")

daily_long_2 <- faw.daily_clean_2 %>%
  pivot_longer(cols = starts_with("Day"), names_to = "Day", values_to = "Count") %>%
  mutate(Day = as.numeric(str_remove(Day, "Day ")))

weekly_summary_2 <- weekly_long_2 %>%
  mutate(Week = as.numeric(str_remove(Week, "Week "))) %>%
  group_by(Treatment, Week) %>%
  summarise(
    Mean = mean(Count),
    SEM = sd(Count) / sqrt(n()),
    .groups = 'drop'
  )

# Grayscale palette ---
grey_palette <-  c(
  "Blend1" = "#C65B13",
  "Blend2" = "#FFC000",
  "Blend3" = "#FEFF01",
  "Female" = "#92D14F",
  "n-Hexane" = "#F2F2F2"
)
weekly_summary_2$Treatment <- factor(
  weekly_summary_2$Treatment,
  levels = c("Blend2", "Blend1", "Blend3", "n-Hexane", "Female")
)

ggplot() +
  geom_col(
    data = weekly_summary_2,
    aes(x = Week, y = Mean, fill = Treatment),
    position = position_dodge(0.8),
    width = 0.7,
    color = "black",
    alpha = 0.9
  ) +
  geom_errorbar(
    data = weekly_summary_2,
    aes(
      x = Week,
      ymin = Mean - SEM,
      ymax = Mean + SEM,
      group = Treatment
    ),
    position = position_dodge(0.8),
    width = 0.2,
    color = "black",
    linewidth = 0.5
  ) +
  scale_fill_manual(
    name = "Treatment",
    values = grey_palette,
    labels = c(
      "Full Blend",
      "Minimal Z-Blend",
      "Minimal E-Blend",
      "Control",
      "Live FAW Female"
    )
  ) +
  scale_x_continuous(
    breaks = sort(unique(weekly_summary_2$Week)),
    labels = sort(unique(weekly_summary_2$Week))
  ) +
  scale_y_continuous(
    limits = c(0, 30)
  ) +
  labs(
    x = "Weeks",
    y = "Mean Fall Armyworm Male/Week"
  ) +
  
  theme_classic() +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    
    axis.line = element_line(color = "black", linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    legend.position = c(0.90, 0.75),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20, face = "bold"),
    legend.key.height = unit(0.8, "cm"),
    
    plot.margin = margin(15, 15, 15, 15)
  )


summary_data <- other.kruskal_2 %>%
  group_by(Treatment) %>%
  summarise(
    mean = mean(Count, na.rm = TRUE),
    sd = sd(Count, na.rm = TRUE),
    n = n(),
    sem = sd / sqrt(n)
  )
groups <- non.target.kruskal_2$groups %>%
  tibble::rownames_to_column("Treatment") %>%
  dplyr::rename(group = groups)

plot_data <- left_join(summary_data, groups, by = "Treatment")

plot_data$Treatment <- factor(
  plot_data$Treatment,
  levels = c("Blend3", "n-Hexane", "Blend2", "Blend1", "Female")
)

ggplot(plot_data, aes(x = Treatment, y = mean, fill = Treatment)) +
  geom_col(width = 0.3, color = "black") +
  geom_errorbar(
    aes(ymin = mean - sem, ymax = mean + sem),
    width = 0.1,
    linewidth = 0.8,
    color = "black"
  ) +
  geom_text(
    aes(label = group, y = mean + sem + 0.2),
    size = 10,
    #fontface = "bold",
    vjust = 0
  ) +
  scale_fill_manual(values = c(
    "Blend1" = "#F2F2F2",
    "Blend2" = "#FEFF01",
    "Blend3" = "#FFC000",
    "n-Hexane" = "#C65B13",
    "Female" = "#92D14F"
  )) +
  
  scale_x_discrete(labels = c(
    "Blend1"   = "Control",
    "Blend2"   = "Minimal E-Blend",
    "Blend3"   = "Full Blend",
    "n-Hexane" = "Minimal Z-Blend",
    "Female"   = "Live FAW Female"
  )) +
  labs(
    y = "Mean Non-Target Insects/Day",
    x = NULL
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    axis.line = element_line(color = "black", linewidth = 0.5),
    panel.grid.major.y = element_line(color = "white", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    legend.position = "none",
    plot.margin = margin(15, 15, 15, 15)
  )


################.   8888888888

faw_summary_2 <- faw.kruskal_2 %>%
  group_by(Treatment) %>%
  summarise(
    Mean = mean(Count),
    SEM = sd(Count) / sqrt(n()),
    .groups = 'drop'
  )

# Add Kruskal letters
letters_faw_2 <- data.frame(
  Treatment = c("Blend1", "Blend2", "Blend3", "n-Hexane", "Female"),
  groups = c("a", "a", "b", "b", "a")
)

# Merge with summary data
faw_plot_data_2 <- faw_summary_2 %>%
  left_join(letters_faw_2, by = "Treatment")

# Set factor order (Female last)
faw_plot_data_2$Treatment <- factor(
  faw_plot_data_2$Treatment,
  levels = c("Blend2", "Blend1", "Blend3", "n-Hexane", "Female")
)

# Define color palette
grey_palette <- c(
  "Blend1" = "#C65B13",
  "Blend2" = "#FFC000",
  "Blend3" = "#FEFF01",
  "n-Hexane" = "#F2F2F2",
  "Female" = "#92D14F"
)

ggplot(faw_plot_data_2, aes(x = Treatment, y = Mean, fill = Treatment)) +
  geom_col(width = 0.3, color = "black") +
  geom_errorbar(
    aes(ymin = Mean - SEM, ymax = Mean + SEM),
    width = 0.1,
    linewidth = 0.7,
    color = "black"
  ) +
  geom_text(
    aes(label = groups, y = Mean + SEM + 0.1),
    size = 10,
    #fontface = "bold",
    vjust = 0
  ) +
  scale_fill_manual(values = grey_palette) +
  scale_x_discrete(labels = c(
    "Blend1" = "Minimal Z-Blend",
    "Blend2" = "Full Blend",
    "Blend3" = "Minimal E-Blend",
    "n-Hexane" = "Control",
    "Female" = "Female"
  )) +
  labs(
    y = "Mean Fall Armyworm/Day",
    x = NULL
  ) +
  scale_y_continuous(
    limits = c(0, 12)
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    axis.line = element_line(color = "black", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    legend.position = "none",
    plot.margin = margin(15, 15, 15, 15)
  )


seasonal_total_plot <- faw.season %>%
  mutate(Week = as.numeric(str_remove(Week, "Week "))) %>%
  group_by(season) %>%
  summarise(
    Total = sum(Count),
    .groups = "drop"
  )

ggplot(seasonal_total_plot, aes(x = season, y = Total, fill = season)) +
  geom_col(width = 0.35) +
  geom_text(
    aes(label = Total),
    vjust = -0.6,
    size = 7,
    fontface = "bold"
  ) +
  labs(
    x = NULL,
    y = "Total Fall Armyworm Males Trapped"
  ) +
  scale_x_discrete(
    labels = c(
      "season 1" = "Dry Season",
      "season 2" = "Rainy Season"
    )
  ) +
  scale_fill_manual(
    values = c(
      "season 1" = "#FFC06E",
      "season 2" = "#92D14A"
    )
  ) +
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    
    axis.line = element_line(color = "black", linewidth = 0.5),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "white", linewidth = 0),
    panel.grid.minor.y = element_blank(),
    
    legend.position = "none",
    
    plot.margin = margin(15, 15, 15, 15)
  )


seasonal_mean_plot <- faw.season %>%
  mutate(Week = as.numeric(str_remove(Week, "Week "))) %>%
  group_by(season) %>%
  summarise(
    Mean = mean(Count, na.rm = TRUE),
    SEM  = sd(Count, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

ggplot(seasonal_mean_plot, aes(x = season, y = Mean, fill = season)) +
  
  geom_errorbar(
    aes(ymin = Mean - SEM, ymax = Mean + SEM),
    width = 0.1,
    linewidth = 0.4,
    alpha = 0.7
  ) +
  
  geom_col(width = 0.2) +
  labs(
    x = NULL,
    y = "Mean Fall Armyworm Males Trapped"
  ) +
  
  scale_x_discrete(
    labels = c(
      "season 1" = "Dry Season",
      "season 2" = "Rainy Season"
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "season 1" = "#FFC06E",
      "season 2" = "#92D14A"
    )
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    
    axis.line = element_line(color = "black", linewidth = 0.5),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    legend.position = "none",
    
    plot.margin = margin(15, 15, 15, 15)
  )



# # # # # # # # # # # # 

# Season 1 - Non-target insects total
other.total.season1 <- other.weekly_clean %>%
  pivot_longer(
    cols = starts_with("Week"),
    names_to = "Week",
    values_to = "Count"
  ) %>%
  group_by(Treatment) %>%
  summarise(
    Total_Catch = sum(Count, na.rm = TRUE),
    .groups = "drop"
  )

print(other.total.season1)

# Season 2 - Non-target insects total
other.total.season2 <- other.weekly_clean_2 %>%
  pivot_longer(
    cols = starts_with("Week"),
    names_to = "Week",
    values_to = "Count"
  ) %>%
  group_by(Treatment) %>%
  summarise(
    Total_Catch = sum(Count, na.rm = TRUE),
    .groups = "drop"
  )

print(other.total.season2)

faw.total.season1$Season <- "Season 1"
faw.total.season2$Season <- "Season 2"

faw.total.combined <- bind_rows(faw.total.season1, faw.total.season2)

print(faw.total.combined)

# Season 1
other.season1 <- other.weekly_clean %>%
  pivot_longer(
    cols = starts_with("Week"),
    names_to = "Week",
    values_to = "Count"
  ) %>%
  mutate(season = "season 1")

# Season 2
other.season2 <- other.weekly_clean_2 %>%
  pivot_longer(
    cols = starts_with("Week"),
    names_to = "Week",
    values_to = "Count"
  ) %>%
  mutate(season = "season 2")

# Combine both seasons
other.season <- bind_rows(other.season1, other.season2)

seasonal_total_plot_nt <- other.season %>%
  mutate(Week = as.numeric(str_remove(Week, "Week "))) %>%
  group_by(season) %>%
  summarise(
    Total = sum(Count, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(seasonal_total_plot_nt, aes(x = season, y = Total, fill = season)) +
  
  geom_col(width = 0.35) +
  
  # 🔹 TOTAL LABELS
  geom_text(
    aes(label = Total),
    vjust = -0.6,
    size = 7,
    fontface = "bold"
  ) +
  
  labs(
    x = NULL,
    y = "Total Non-target Insects Trapped"
  ) +
  
  scale_x_discrete(
    labels = c(
      "season 1" = "Dry Season",
      "season 2" = "Rainy Season"
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "season 1" = "#FFC06E",
      "season 2" = "#92D14A"
    )
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    
    axis.line = element_line(color = "black", linewidth = 0.5),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "white", linewidth = 0),
    panel.grid.minor.y = element_blank(),
    
    legend.position = "none",
    
    plot.margin = margin(15, 15, 15, 15)
  )

seasonal_mean_plot_nt <- other.season %>%
  mutate(Week = as.numeric(str_remove(Week, "Week "))) %>%
  group_by(season) %>%
  summarise(
    Mean = mean(Count, na.rm = TRUE),
    SEM = sd(Count, na.rm = TRUE) / sqrt(n()),  # optional but recommended
    .groups = "drop"
  )

scale_y_continuous(
  limits = c(0, 5)
)


ggplot(seasonal_mean_plot_nt, aes(x = season, y = Mean, fill = season)) +
  
  # 🔹 ERROR BARS (put AFTER bars for clarity)
  geom_errorbar(
    aes(ymin = Mean - SEM, ymax = Mean + SEM),
    width = 0.1,
    linewidth = 0.7,
    alpha = 0.7
  ) +
  geom_col(width = 0.2) +
  labs(
    x = NULL,
    y = "Mean Non-target Insects Trapped"
  ) +
  
  scale_x_discrete(
    labels = c(
      "season 1" = "Dry Season",
      "season 2" = "Rainy Season"
    )
  ) +
  
  scale_y_continuous(
    limits = c(0, 2)
  ) +   # ✅ FIX: added +
  
  scale_fill_manual(
    values = c(
      "season 1" = "#FFC06E",
      "season 2" = "#92D14A"
    )
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    
    axis.line = element_line(color = "black", linewidth = 0.5),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "white", linewidth = 0),
    panel.grid.minor.y = element_blank(),
    
    legend.position = "none",
    
    plot.margin = margin(15, 15, 15, 15)
  )


weekly_summary$Season <- "season 1"
weekly_summary_2$Season <- "season 2"

combined_summary <- bind_rows(weekly_summary, weekly_summary_2)

total_per_week <- combined_summary %>%
  group_by(Season, Week) %>%
  summarise(
    Total = sum(Mean),
    SEM_total = sqrt(sum(SEM^2)), # combine SEMs assuming independence
    .groups = 'drop'
  )

ggplot(total_per_week, aes(x = factor(Week), y = Total, fill = Season)) +
  
  geom_col(width = 0.35, position = position_dodge(width = 0.5)) +
  
  geom_errorbar(
    aes(ymin = Total - SEM_total, ymax = Total + SEM_total),
    width = 0.15,
    linewidth = 0.4,
    position = position_dodge(width = 0.5),
    alpha = 0.7
  ) +
  
  scale_fill_manual(
    values = c(
      "season 1" = "#FFC06E",  # Dry Season color
      "season 2" = "#92D14A"   # Rainy Season color
    ),
    labels = c(
      "season 1" = "Dry",
      "season 2" = "Rainy"
    )
  ) +
  
  labs(
    x = "Weeks",
    y = "Fall Armyworm Male/Week",
    fill = "Season",
    title = NULL
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    axis.title.x = element_text(size = 22, face = "bold"),
    
    axis.line = element_line(color = "black", linewidth = 0.5),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    legend.position = c(0.90, 0.75),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14, face = "bold"),
    legend.background = element_rect(fill = "white", color = "black"),
    
    plot.margin = margin(15, 15, 15, 15)
  )
