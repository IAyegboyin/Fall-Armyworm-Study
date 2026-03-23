# Introductory part ----
# Date: February 14th, 2026
# Author: Ismail A. AYEGBOYIN
# This is a real time analysis script of the lab trials for varietal resistance of maize study

# Loading packages ----
require(tidyverse)
require(readxl)
library(lme4)
library(emmeans)
library(performance)
library(multcompView)
library(multcomp) 
# Data validations and preparation ----
# Variety mapping & order
# mapping from V-codes to variety names
variety_map <- c(
  "V1"  = "Sammaz 15",   "V2"  = "Oba Super 4",  "V3"  = "Sammaz 52",
  "V4"  = "Sammaz 60",   "V5"  = "SeedCo 719",      "V6"  = "SeedCo 649",
  "V7"  = "Sammaz 66",   "V8"  = "Sammaz 59",   "V9"  = "Oba Super 6",
  "V10" = "Oba Super 15","V11" = "Sammaz 51"
)
opv_varieties <- c(
  "Sammaz 15", "Sammaz 52", "Sammaz 60",
  "Sammaz 66", "Sammaz 59", "Sammaz 51"
)
hv_varieties <- c(
  "Oba Super 4", "SeedCo 719", "SeedCo 649","Oba Super 6",
  "Oba Super 15"
)
# User-specified order by V-code (this is the order you requested)
ordered_codes <- c("V1","V11","V3","V8","V4","V7","V2","V9","V10","V6","V5")
# Human-readable ordered levels (used for factor levels everywhere)
ordered_names <- unname(variety_map[ordered_codes])

# Davis 0–9 scale to percentage conversion
davis_to_pct <- c(
  "0" = 0,
  "1" = 5,
  "2" = 15,
  "3" = 25,
  "4" = 40,
  "5" = 55,
  "6" = 70,
  "7" = 80,
  "8" = 90,
  "9" = 100
)

variety_type_cols <- c(
  "HV" = "#66a61e",   # green
  "OPV" = "#d95f02"  # orange
)

# Explicit variety colours (keys match variety names)
variety_cols <- set_names(
  c("#d95f02","#1f78b4","#b2df8a","#1b9e77","#fb9a99","#7570b3","#666666","#e7298a","#a6761d","#e6ab02","#66a61e"),
  ordered_names
)


week_cols <- c(
  "2 WAS" = "#1b9e77",  # green
  "3 WAS" = "#d95f02",  # orange
  "4 WAS" = "#7570b3",  # purple
  "5 WAS" = "#e7298a",   # pink
  "6 WAS" = "#e6ab02",
  "7 WAS" = "#66a61e",
  "8 WAS" = "#1f78b4"
)

# Week colours (explicit mapping)
period_cols <- c(
  "8-21 DAS" = "#1b9e77",   # green
  "22-35 DAS" = "#d95f02",   # orange
  "36-49 DAS" = "#7570b3",    # purple
  "50-63 DAS" = "#e7298a"
)

# Loading, Cleaning and Manipulation of data -----
setwd("/Users/Esmael/Desktop/Data Science Library/Data for play/Fall-Armyworm-Study/Host_Resistance_Study/Data")

dmg_trial_1 <- read_excel("ismail_lab_damage_assessment_trial_1.xlsx",
                    sheet = "Sheet1")%>%
              dplyr::select(!c(Variety, Replicate)) %>%
              rename(
              Variety    = `Variety name`,
              dmg_1 = `LDA DAY1`,
              dmg_2 = `LDA DAY2`,
              dmg_3 = `LDA DAY3`,
              dmg_4 = `LDA DAY4`,
              dmg_5 = `LDA DAY5`) %>%
  mutate(
                Variety = recode(as.character(Variety), !!!variety_map),
                Variety = factor(Variety, levels = ordered_names),
                Variety_Type = if_else(Variety %in% opv_varieties, "OPV", "HV")
              )

str(dmg_trial_1)


lar_trial_1 <- read_excel("ismail_lab_damage_assessment_trial_1.xlsx",
                       sheet = "Sheet2")%>%
  dplyr::select(!c(Variety)) %>%
rename(
  Variety    = `Variety name`,
  lw_initial = `Larval weight Day 0`,
  lw_final = `Larval weight Day 7`) %>%
  mutate(
    Variety = recode(as.character(Variety), !!!variety_map),
    Variety = factor(Variety, levels = ordered_names),
    Variety_Type = if_else(Variety %in% opv_varieties, "OPV", "HV")
  )

str(lar_trial_1)

# second trial data loading ....

# working data now to long format ....
dmg_trial_1_long <- dmg_trial_1 %>%
  
  # Recreate replicate ID (since original was removed)
  mutate(Replicate = row_number()) %>%
  
  # Pivot all damage day columns (dmg_1 to dmg_5)
  pivot_longer(
    cols = matches("^dmg_\\d+$"),
    names_to = c("variable", "Day"),
    names_pattern = "(.*)_(\\d+)",
    values_to = "Damage_Score"
  ) %>%
  
  mutate(
    # Convert Day to numeric
    Day = as.numeric(Day),
    
    # Add Trial label
    Trial = "Trial1",
    
    # Ensure character before mapping
    Damage_Score = as.character(Damage_Score),
    
    # Convert Davis 0–9 scale to percentage
    Damage_pct = as.numeric(davis_to_pct[Damage_Score]),
    
    # Create grouped damage period (optional but useful)
    Damage_Period = factor(
      case_when(
        Day %in% c(1, 2) ~ "Early Damage",
        Day %in% c(3, 4) ~ "Mid Damage",
        Day %in% c(5)    ~ "Late Damage",
        TRUE ~ NA_character_
      ),
      levels = c("Early Damage", "Mid Damage", "Late Damage")
    )
  ) %>%
  
  # Reorder columns cleanly
  dplyr::select(
    Variety,
    Variety_Type,
    Replicate,
    Trial,
    Day,
    Damage_Period,
    Damage_Score,
    Damage_pct
  )

lar_trial_1 <- lar_trial_1 %>%
  mutate(
    Trial = "Trial1",
    Weight_Gain = lw_final - lw_initial
  )


interaction_model <- aov(Damage ~ Variety * Trial, data = dmg_trial_1_long)
summary(interaction_model)
weight_interaction <- aov(Weight_Gain ~ Variety * Trial, data = lar_trial_1_long)
summary(weight_interaction)


dmg_plot_data <- dmg_trial_1_long %>%
  group_by(Variety_Type, Variety, Day) %>%
  summarise(
    mean_damage = mean(Damage_pct, na.rm = TRUE),
    se_damage = sd(Damage_pct, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

ggplot(
  filter(dmg_plot_data, Variety_Type == "OPV"),
  aes(x = Day, y = mean_damage, color = Variety, group = Variety)
) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Damage Progression in OPV Varieties",
    x = "Day After Infestation",
    y = "Mean Damage (%)"
  ) +
  theme_classic(base_size = 13)


ggplot(
  filter(dmg_plot_data, Variety_Type == "HV"),
  aes(x = Day, y = mean_damage, color = Variety, group = Variety)
) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Damage Progression in Hybrid Varieties (HV)",
    x = "Day After Infestation",
    y = "Mean Damage Score (%)"
  ) +
  theme_classic(base_size = 13)


#bar charts ...
ggplot(
  filter(dmg_plot_data, Variety_Type == "OPV"),
  aes(x = Variety,
      y = mean_damage,
      fill = Variety)
) +
  geom_col(width = 0.5) +
  geom_errorbar(aes(ymin = mean_damage - se_damage,
                    ymax = mean_damage + se_damage),
                width = 0.15) +
  scale_fill_manual(values = variety_cols) +
  facet_wrap(~ Day, nrow = 3) +
  labs(
    title = "Damage Severity in OPV Varieties Across Days",
    x = "Variety",
    y = "Mean Damage Score (%)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none",
    strip.text = element_text(face = "bold")
  )


ggplot(
  filter(dmg_plot_data, Variety_Type == "HV"),
  aes(x = Variety,
      y = mean_damage,
      fill = Variety)
) +
  geom_col(width = 0.5) +
  geom_errorbar(aes(ymin = mean_damage - se_damage,
                    ymax = mean_damage + se_damage),
                width = 0.15) +
  scale_fill_manual(values = variety_cols) +
  facet_wrap(~ Day, nrow = 3) +
  labs(
    title = "Damage Severity in Hybrid Varieties (HV) Across Days",
    x = "Variety",
    y = "Mean Damage Score (%)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none",
    strip.text = element_text(face = "bold")
  )

# larva 
weight_plot_data <- lar_all %>%
  group_by(Variety_Type, Variety) %>%
  summarise(
    mean_gain = mean(Weight_Gain, na.rm = TRUE),
    se_gain = sd(Weight_Gain, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

ggplot(
  filter(weight_plot_data, Variety_Type == "OPV"),
  aes(x = reorder(Variety, mean_gain), 
      y = mean_gain,
      fill = Variety)
) +
  geom_col(width = 0.5) +
  geom_errorbar(aes(ymin = mean_gain - se_gain,
                    ymax = mean_gain + se_gain),
                width = 0.2) +
  scale_fill_manual(values = variety_cols) +
  labs(
    title = "Larval Weight Gain on OPV Varieties",
    x = "Variety",
    y = "Mean Weight Gain"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none"
  )

ggplot(
  filter(weight_plot_data, Variety_Type == "HV"),
  aes(x = reorder(Variety, mean_gain), 
      y = mean_gain,
      fill = Variety)
) +
  geom_col(width = 0.5) +
  geom_errorbar(aes(ymin = mean_gain - se_gain,
                    ymax = mean_gain + se_gain),
                width = 0.2) +
  scale_fill_manual(values = variety_cols) +
  labs(
    title = "Larval Weight Gain on Hybrid Varieties (HV)",
    x = "Variety",
    y = "Mean Weight Gain"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none"
  )






