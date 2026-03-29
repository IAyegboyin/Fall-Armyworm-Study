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
  group_by(Variety_Type, Variety) %>%
  summarise(
    total_eggs = sum(Value_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    egg_pct = total_eggs / sum(total_eggs) * 100
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
  geom_col(width = 0.3) +
  scale_fill_manual(values = variety_cols) +
  labs(
    title = "FAW Egg Batches (%) on Open Pollinated Varieties (1–63 DAS)",
    y = "Total Egg Batches (%)",
    x = "Variety"
  ) +
  scale_y_continuous(limits = c(0, 30), expand = c(0,5)) +
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
  geom_col(width = 0.3) +
  scale_fill_manual(values = variety_cols) +
  labs(
    title = "FAW Egg Batches (%) on Hybrid Varieties (1–63 DAS)",
    y = "Total Egg Batches (%)",
    x = "Variety"
  ) +
  scale_y_continuous(limits = c(0, 30), expand = c(0,5)) +
  theme_classic(base_size = 13, base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "none"
  )


egg_type_summary <- egg_data %>%
  group_by(Variety_Type) %>%
  summarise(
    total_eggs = sum(Value_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    egg_pct = total_eggs / sum(total_eggs) * 100,
    Variety_Type = recode(
      Variety_Type,
      "HV"  = "Hybrid",
      "OPV" = "Open Pollinated"
    )
  )

ggplot(egg_type_summary,
       aes(x = Variety_Type, y = egg_pct, fill = Variety_Type)) +
  geom_col(width = 0.14) +
  scale_fill_manual(
    values = c(
      "Hybrid" = "#66a61e",
      "Open Pollinated" = "#d95f02"
    )
  ) +
  scale_y_continuous(limits = c(0, 60), expand = c(0,5)) +
  labs(
    title = "FAW Egg Batches (%) by Maize Variety Type (1–63 DAS)",
    y = "Total Egg Batches (%)",
    x = "Maize Variety Type"
  ) +
  theme_classic(base_size = 13, base_family = "Times New Roman") +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "none"
  )