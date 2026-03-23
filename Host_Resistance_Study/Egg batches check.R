# -----------------------------------------------------
# 1️⃣ Filter egg data and create presence variable
# -----------------------------------------------------

egg_data <- period_ent_data %>%
  filter(Parameter == "Eggs") %>%
  mutate(
    Value_pct = as.numeric(Value_pct),
    egg_present = ifelse(Value_pct >= 1, 1, 0)
  )

# -----------------------------------------------------
# 2️⃣ Ensure Period is correctly ordered
# -----------------------------------------------------

egg_data$Period <- factor(
  egg_data$Period,
  levels = c("8–21", "22–35", "36–49", "50–63")
)

# -----------------------------------------------------
# 3️⃣ Summarise egg presence (%) per Variety × Period × Variety_Type
# -----------------------------------------------------

eggs_summary <- egg_data %>%
  group_by(Variety_Type, Variety, Period) %>%
  summarise(
    egg_pct = mean(egg_present, na.rm = TRUE) * 100,
    total_eggs = sum(Value_pct, na.rm = TRUE),
    n = sum(!is.na(egg_present)),
    .groups = "drop"
  )

# -----------------------------------------------------
# 0️⃣ Ensure Variety is factor with desired order
# -----------------------------------------------------
eggs_summary$Variety <- factor(eggs_summary$Variety, levels = ordered_names)

# Split again by Variety Type
eggs_opv <- eggs_summary %>% filter(Variety_Type == "OPV")
eggs_hv  <- eggs_summary %>% filter(Variety_Type == "HV")

# -----------------------------------------------------
# 1️⃣ Plot: OPV (Variety-colored bars) with y-scale
# -----------------------------------------------------
ggplot(eggs_opv, aes(x = Variety, y = egg_pct, fill = Variety)) +
  geom_col(position = position_dodge(width = 0.5), width = 0.5) +
  scale_fill_manual(values = variety_cols) +
  scale_y_continuous(limits = c(0, 4)) +  # ensures y-axis goes from 0 to 100%
  labs(
    title = "FAW Egg Batch Presence (%) on Open Pollinated Varieties (1–63 DAS)",
    y = "Egg Batches (%)",
    x = "Variety"
  ) +
  theme_classic(base_size = 13, base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "none"
  )

# -----------------------------------------------------
# 2️⃣ Plot: HV (Variety-colored bars) with y-scale
# -----------------------------------------------------
ggplot(eggs_hv, aes(x = Variety, y = egg_pct, fill = Variety)) +
  geom_col(position = position_dodge(width = 0.5), width = 0.5) +
  scale_fill_manual(values = variety_cols) +
  scale_y_continuous(limits = c(0, 4)) +  # same scale as OPV
  labs(
    title = "FAW Egg Batch Presence (%) on Hybrid Varieties (1–63 DAS)",
    y = "Egg Batches (%)",
    x = "Variety"
  ) +
  theme_classic(base_size = 13, base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "none"
  )