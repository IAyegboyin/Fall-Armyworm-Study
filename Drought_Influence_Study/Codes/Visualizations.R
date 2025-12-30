# .............
# Visualizations .......
# ..........................................................................

# summarise before pivoting
eggmass_total <- oviposition %>%
  group_by(cage_id, treatment) %>%
  summarise(total_eggmass = mean(total_eggmass, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = treatment, values_from = total_eggmass)

# do the same for other eggs
other_total <- oviposition %>%
  group_by(cage_id, treatment) %>%
  summarise(total_other_eggs = mean(total_other_eggs, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = treatment, values_from = total_other_eggs)

ggplot(eggmass_total, aes(x = 1, y = no_drought)) +
  geom_point(color="blue", size=3) +
  geom_point(aes(y = drought), color="red", size=3) +
  geom_segment(aes(x=1, xend=1.5, y=no_drought, yend=drought),
               color="gray") +
  scale_x_continuous(breaks=c(1,1.5), labels=c("No drought","Drought")) +
  labs(title="Paired eggmass counts per cage",
       y="Total eggmass") +
  theme_classic()

# .............
# ..........................................................................
ovi_long <- oviposition %>%
  dplyr::select(cage_id, treatment, eggmass_24h, eggmass_48h, total_eggmass,
                other_eggs_24h, other_eggs_48h, total_other_eggs) %>%
  pivot_longer(
    cols = -c(cage_id, treatment),
    names_to = "variable",
    values_to = "count"
  )

ggplot(ovi_long, aes(x = treatment, y = count, fill = treatment)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.7) +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Egg-laying patterns across drought treatments",
    y = "Egg count",
    x = "Treatment"
  )

view (ovi_long)
ovi_summary <- ovi_long %>%
  group_by(treatment, variable) %>%
  summarise(
    mean = mean(count, na.rm = TRUE),
    sem = sd(count, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

ggplot(ovi_summary, aes(x = mean, y = variable, fill = treatment)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(
    aes(xmin = mean - sem, xmax = mean + sem),
    position = position_dodge(0.8),
    width = 0.3
  ) +
  theme_classic() +
  labs(
    # title = "Egg-laying patterns across drought treatments",
    x = "Mean ± SEM (egg count)",
    y = "Variable"
  ) +
  theme(
    legend.position = "top",
    text = element_text(size = 12)
  )

ggplot(oviposition, aes(x = treatment, y = total_eggmass, fill = treatment)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  theme_classic() +
  labs(title = "Distribution of Eggmass", y = "Total Eggmass", x = "Treatment")

# .............
# Larva data
# ..........................................................................
ggplot(larva, aes(x = pot_size, y = plant_height_cm, fill = pot_size)) +
  geom_boxplot(alpha = 0.6, width = 0.4) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  theme_classic() +
  labs(title = "Plant Height by Pot Size", x = "Pot Size", y = "Plant Height (cm)")

larva %>%
  group_by(treatment) %>%
  summarise(mean_damage = mean(avg_leaf_damage, na.rm = TRUE),
            sem_damage = sd(avg_leaf_damage, na.rm = TRUE) / sqrt(n())) %>%
  ggplot(aes(x = mean_damage, y = treatment, fill = treatment)) +
  geom_col(width = 0.5, alpha = 1) +
  geom_errorbarh(aes(xmin = mean_damage - sem_damage, xmax = mean_damage + sem_damage),
                 height = 0.2, color = "black") +
  theme_classic() +
  labs(#title = "Average Leaf Damage by Treatment",
       x = "Average Leaf Damage (%)", y = "") +
  theme(
    # Axis text and title sizes
    axis.text.x = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    
    # Axis lines
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    
    # Grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "white", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    
    # Legend customization
    legend.position = c(0.90, 0.95),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20,  face = "bold"),
    legend.key.height = unit(0.8, "cm"),
    
    # Plot margins
    plot.margin = margin(15, 15, 15, 15)
  )


larva_long <- larva %>%
  pivot_longer(cols = starts_with("leaf_damage"),
               names_to = "timepoint",
               values_to = "damage") %>%
  mutate(timepoint = recode(timepoint,
                            leaf_damage_1 = "24h",
                            leaf_damage_2 = "48h",
                            leaf_damage_3 = "120h"),
         timepoint = factor(timepoint, levels = c("24h", "48h", "120h")))
view(larva_long)
ggplot(larva_long, aes(x = timepoint, y = damage, color = treatment, group = treatment)) +
  stat_summary(fun = mean, geom = "line", size = 0.8) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  theme_minimal() +
  labs(title = "Leaf Damage Progression Over Time",
       x = "Time after Infestation",
       y = "Leaf Damage (%)")

larva %>%
  group_by(treatment) %>%
  summarise(pupation_rate = mean(pupation == "yes", na.rm = TRUE),
            adult_emergence_rate = mean(adult_emergence %in% c("M","F"), na.rm = TRUE)) %>%
  pivot_longer(-treatment, names_to = "stage", values_to = "rate") %>%
  ggplot(aes(x = treatment, y = rate, fill = stage)) +
  geom_col(position = position_dodge(), width = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  labs(title = "Pupation & Adult Emergence Rates by Treatment", y = "Proportion", x = "Treatment")


sex_summary <- larva %>%
  filter(adult_emergence %in% c("M","F")) %>%
  group_by(treatment, adult_emergence) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(treatment) %>%
  mutate(prop = count / sum(count))

ggplot(sex_summary, aes(x = treatment, y = prop, fill = adult_emergence)) +
  geom_col(position = "fill", width = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  labs(title = "Sex Ratio of Adult Emergence by Treatment",
       x = "Treatment", y = "Proportion (%)", fill = "Sex")

ggplot(sex_summary, aes(x = treatment, y = count, fill = adult_emergence)) +
  geom_col(position = position_dodge(), width = 0.5) +
  theme_minimal() +
  labs(title = "Adult Emergence (Counts) by Treatment",
       x = "Treatment", y = "Number of Adults", fill = "Sex")

x <- larva %>%
  dplyr::select(treatment, larval_weight_gain)%>%
  group_by(treatment) %>%
  summarise(
    mean_lw = mean(larval_weight_gain, na.rm = TRUE),
    sem_lw = sd(larval_weight_gain, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

ggplot(x, aes(x = treatment, y = mean_lw, fill = treatment)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean_lw - sem_lw, ymax = mean_lw + sem_lw), 
                width = 0.2, color = "black", size = 0.7) +
  labs(title = "Average Larval Weight Gain by Treatment",
       x = "Treatment",
       y = "Mean Larval Weight Gain (g)") +
  scale_fill_manual(values = c("drought" = "lightcoral", "no drought" = "lightblue")) +
  theme_classic() +
  theme(legend.position = "none")

larva_summary <- larva %>%
  group_by(treatment) %>%
  summarise(
    start_mean = mean(larval_weight_start, na.rm = TRUE),
    start_se   = sd(larval_weight_start, na.rm = TRUE) / sqrt(n()),
    end_mean   = mean(larval_weight_end, na.rm = TRUE),
    end_se     = sd(larval_weight_end, na.rm = TRUE) / sqrt(n())
  )

# Reshape for plotting
larva_long <- larva_summary %>%
  tidyr::pivot_longer(
    cols = c(start_mean, end_mean),
    names_to = "time",
    values_to = "mean_weight"
  ) %>%
  mutate(
    se = ifelse(time == "start_mean", start_se, end_se),
    time = ifelse(time == "start_mean", "Start_30-7-2025", "End_5-8-25"),
    time = factor(time, levels = c("Start_30-7-2025", "End_5-8-25")) # ensures Start comes first
  )

# Plot mean ± SEM by treatment
ggplot(larva_long, aes(x = time, y = mean_weight, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.5) +
  geom_errorbar(aes(ymin = mean_weight - se, ymax = mean_weight + se),
                position = position_dodge(width = 0.6), width = 0.2) +
  labs(x = "Time", y = "Larval weight (g)")+
       # title = "Average larval weight at the beginning and end of the Experiment") +
  theme_classic(base_size = 14) +
  theme(
    # Axis text and title sizes
    axis.text.x = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    
    # Axis lines
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    
    # Grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "white", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    
    # Legend customization
    legend.position = c(0.40, 0.95),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20,  face = "bold"),
    legend.key.height = unit(0.8, "cm"),
    
    # Plot margins
    plot.margin = margin(15, 15, 15, 15)
  )

# Summarize by cage_id and treatment
eggmass_summary <- oviposition %>%
  group_by(cage_id, treatment) %>%
  summarise(
    eggmass_24h = sum(eggmass_24h, na.rm = TRUE),
    eggmass_48h = sum(eggmass_48h, na.rm = TRUE),
    .groups = "drop"
  )

# Reshape for plotting (24h vs 48h)
eggmass_long <- eggmass_summary %>%
  pivot_longer(cols = starts_with("eggmass"),
               names_to = "time",
               values_to = "eggmass") %>%
  mutate(time = recode(time,
                       "eggmass_24h" = "24h",
                       "eggmass_48h" = "48h"))

# Calculate mean ± SEM across cages
eggmass_stats <- eggmass_long %>%
  group_by(treatment, time) %>%
  summarise(
    mean_eggmass = mean(eggmass, na.rm = TRUE),
    se = sd(eggmass, na.rm = TRUE) / sqrt(n()), 
    .groups = "drop"
  )

view(eggmass_long)

# Plot bar chart with SEM + jittered cage values
ggplot() +
  geom_bar(data = eggmass_stats,
           aes(x = time, y = mean_eggmass, fill = treatment),
           stat = "identity", position = position_dodge(width = 0.6), width = 0.5,
           alpha = 0.5) +
  geom_errorbar(data = eggmass_stats,
                aes(x = time, ymin = mean_eggmass - se, ymax = mean_eggmass + se, group = treatment),
                position = position_dodge(width = 0.6), width = 0.2) +
  geom_jitter(data = eggmass_long,
              aes(x = time, y = eggmass, color = treatment),
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6),
              size = 4, alpha = 0.7) +
  labs(x = "Time after release", 
       y = "Number of egg masses",
       title = "Fall armyworm egg laying (n = 7 cages)",
       subtitle = "Bars = mean ± SEM, points = cage values") +
  theme_classic(base_size = 14)


#Collapse within each cage × treatment
eggmass_cage <- oviposition %>%
  group_by(cage_id, treatment) %>%
  summarise(
    eggmass_24h = sum(eggmass_24h, na.rm = TRUE),
    eggmass_48h = sum(eggmass_48h, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(eggmass_24h, eggmass_48h),
               names_to = "time", values_to = "eggmass") %>%
  mutate(
    time = ifelse(time == "eggmass_24h", "24h", "48h")
  )

# Summary for bars (mean ± SEM across cages)
eggmass_stats <- eggmass_cage %>%
  group_by(treatment, time) %>%
  summarise(
    mean_eggmass = mean(eggmass, na.rm = TRUE),
    se = sd(eggmass, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Plot
ggplot() +
  geom_bar(data = eggmass_stats,
           aes(x = time, y = mean_eggmass, fill = treatment),
           stat = "identity", position = position_dodge(width = 0.6), width = 0.5,
           alpha = 0.6) +
  geom_errorbar(data = eggmass_stats,
                aes(x = time, ymin = mean_eggmass - se, ymax = mean_eggmass + se, group = treatment),
                position = position_dodge(width = 0.6), width = 0.2) +
  geom_jitter(data = eggmass_cage,
              aes(x = time, y = eggmass, color = treatment),
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6),
              size = 2, alpha = 0.8, show.legend = FALSE) +
  labs(x = "Duration of count", 
       y = "Number of egg masses")+
       # title = "Fall armyworm egg laying (n = 11)") +
  theme_classic(base_size = 14) +
  scale_color_manual(values = c("black", "black"))

# .............
# Small check on the drought imposed on the soil weight ...........
# ..........................................................................
soil_weight <- read_excel("drought data_10-9-25.xlsx",
                    sheet = "larva_expt")%>%
  dplyr::select(treatment, wet_weight_g, `pot_weight to be maintained`, `potweight_g_1-8-25`)%>%
rename(
  Treatment = treatment ,
  Pot_weight_1 = wet_weight_g,
  Pot_weight_2 = `pot_weight to be maintained`,
  Pot_weight_3 = `potweight_g_1-8-25`)

soil_weight_long <- soil_weight %>%
  pivot_longer(cols = c(Pot_weight_1,Pot_weight_2, Pot_weight_3),
               names_to = "Timepoint",
               values_to = "Pot_Weight") %>%
  mutate(Timepoint = recode(Timepoint,
                            Pot_weight_1 = "Wet_weight_16-07-2025",
                            Pot_weight_2 = "30/07/2025",
                            Pot_weight_3 = "01/08/2025"),
         Timepoint = factor(Timepoint, levels = c("Wet_weight_16-07-2025", 
                                                  "30/07/2025", 
                                                  "01/08/2025")))


ggplot(soil_weight_long, aes(x = Timepoint, y = Pot_Weight, fill = Treatment)) +
  stat_summary(
    fun = mean,
    geom = "col",
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    position = position_dodge(width = 0.8),
    width = 0.4,
    linewidth = 0.9
  ) +
  theme_minimal(base_size = 13) +
  labs(
    # title = "Drought Imposition on Larva Experiment",
    x = "Date",
    y = "Average Pot Weight"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    
    # Axis text and title sizes
    axis.text.x = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    
    # Axis lines
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    
    # Grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "white", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    
    # Legend customization
    legend.position = c(0.50, 0.95),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.text = element_text(size = 16, face = "bold"),
    legend.key.height = unit(0.8, "cm"),
    legend.box = "horizontal",
    legend.title = element_text(size = 16, face = "bold"),
    
    # Plot margins
    plot.margin = margin(15, 15, 15, 15)
  )


soil_weight_ovi <- read_excel("drought data_10-9-25.xlsx",
                          sheet = "watering regime_ovipo expt")%>%
  dplyr::select(22, 5, 8, 10, 12, 14, 16, 18, 20) %>%
  filter(if_all(everything(), ~ .x != "x"))%>%
rename(
  Treatment = status,
  Pot_weight_1 = wet_weight_g,
  Pot_weight_2 = `weight_18-8-25`,
  Pot_weight_3 = `weight_20-8-25`,
  Pot_weight_4 = `weight_22-8-2025`,
  Pot_weight_5 = `weight_25-8`,
  Pot_weight_6 = `weight_26-8`,
  Pot_weight_7 = `weight on 27-8-25`,
  Pot_weight_8 = `weight_28-8-25`)%>%
mutate(across(-Treatment, as.numeric))

soil_weight_ovi_long <- soil_weight_ovi %>%
  pivot_longer(cols = c(Pot_weight_1,Pot_weight_2, Pot_weight_3, 
                        Pot_weight_4, Pot_weight_5, Pot_weight_6, 
                        Pot_weight_7, Pot_weight_8),
               names_to = "Timepoint",
               values_to = "Pot_Weight")%>%
  mutate(Timepoint = recode(Timepoint,
                            Pot_weight_1 = "Wet_weight_11-08-2025",
                            Pot_weight_2 = "18/08/2025",
                            Pot_weight_3 = "20/08/2025", 
                            Pot_weight_4 = "22/08/2025",
                            Pot_weight_5 = "25/08/2025", 
                            Pot_weight_6 = "26/08/2025",
                            Pot_weight_7 = "27/08/2025",
                            Pot_weight_8 = "28/08/2025"),
         Timepoint = factor(Timepoint, levels = c("Wet_weight_11-08-2025", 
                                                  "18/08/2025", 
                                                  "20/08/2025",
                                                  "22/08/2025",
                                                  "25/08/2025",
                                                  "26/08/2025",
                                                  "27/08/2025",
                                                  "28/08/2025"
                                                  )))

ggplot(soil_weight_ovi_long, aes(x = Timepoint, y = Pot_Weight, fill = Treatment)) +
  stat_summary(
    fun = mean,
    geom = "col",
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    position = position_dodge(width = 0.8),
    width = 0.2,
    linewidth = 0.5
  ) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Drought Imposition on Oviposition Experiment",
    x = "Date",
    y = "Average Pot Weight"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.title = element_blank()
  )+
theme(
  # Axis text and title sizes
  axis.text.x = element_text(size = 16, face = "bold"),
  axis.text.y = element_text(size = 16, face = "bold"),
  axis.title.x = element_text(size = 18, face = "bold"),
  axis.title.y = element_text(size = 18, face = "bold"),
  
  # Axis lines
  axis.line = element_line(color = "black", linewidth = 0.5),
  axis.line.y = element_line(color = "black", linewidth = 0.5),
  axis.line.x = element_line(color = "black", linewidth = 0.5),
  
  # Grid lines
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_line(color = "white", linewidth = 0.3),
  panel.grid.minor.y = element_blank(),
  
  # Legend customization
  legend.position = c(0.90, 0.95),
  legend.background = element_rect(fill = "white", color = "black"),
  legend.title = element_text(size = 16, face = "bold"),
  legend.text = element_text(size = 16,  face = "bold"),
  legend.key.height = unit(0.8, "cm"),
  
  # Plot margins
  plot.margin = margin(15, 15, 15, 15)
)


