# ============================================================
# Introductory part ----
# Date: November 25th, 2025
# Author: Ismail A. AYEGBOYIN
# Real-time analysis script of the host plant resistance trial
# 11 maize varieties (Open pollinated and Hybrid)
# ============================================================

# Varieties:
# V1 - Sammaz 15
# V2 - Oba Super 4
# V3 - Sammaz 52
# V4 - Sammaz 60
# V5 - SC 719
# V6 - SC 649
# V7 - Sammaz 66
# V8 - Sammaz 59
# V9 - Oba Super 6
# V10 - Oba Super 15
# V11 - Sammaz 51

# ============================================================
# Loading packages ----
# ============================================================
library(tidyverse)
library(readxl)
library(googlesheets4)
library(pheatmap)
library(reshape2)
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(multcompView)
library(ggpubr)


# gs4_auth()  # authenticate Google account (interactive)

# ---------------------------
# 1) Variety mapping & order
# ---------------------------
# mapping from V-codes to human names
variety_map <- c(
  "V1"  = "Sammaz 15",   "V2"  = "Oba Super 4",  "V3"  = "Sammaz 52",
  "V4"  = "Sammaz 60",   "V5"  = "SC 719",      "V6"  = "SC 649",
  "V7"  = "Sammaz 66",   "V8"  = "Sammaz 59",   "V9"  = "Oba Super 6",
  "V10" = "Oba Super 15","V11" = "Sammaz 51"
)

opv_varieties <- c(
  "Sammaz 15", "Sammaz 52", "Sammaz 60",
  "Sammaz 66", "Sammaz 59", "Sammaz 51"
)

# User-specified order by V-code (this is the order you requested)
ordered_codes <- c("V1","V11","V3","V8","V4","V7","V2","V9","V10","V6","V5")

# Human-readable ordered levels (used for factor levels everywhere)
ordered_names <- unname(variety_map[ordered_codes])

# Week colours (explicit mapping)
period_cols <- c(
  "1-21 DAS" = "#1b9e77",   # green
  "22-35 DAS" = "#d95f02",   # orange
  "36-49 DAS" = "#7570b3",    # purple
  "50-63 DAS" = "#e7298a"
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

# Optional: explicit variety colours (keys match human names)
variety_cols <- set_names(
  c("#d95f02","#1f78b4","#b2df8a","#1b9e77","#fb9a99","#7570b3","#666666","#e7298a","#a6761d","#e6ab02","#66a61e"),
  ordered_names
)

Weekly_Monitoring <- read_sheet(
  "https://docs.google.com/spreadsheets/d/14KYjkx49VFTbC8AI7G0ev2uZNpbIKD3OwCbWAQxlWco/edit?gid=0#gid=0",
  sheet = "Weekly_Monitoring"
)
# glimpse(Weekly_Monitoring)

# Cleaning weekly data -----
weekly_ent_clean_data <- Weekly_Monitoring %>%
  mutate(across(
    where(is.list),
    ~ suppressWarnings(
      map_dbl(.x, function(v){
        if (is.null(v)) return(NA_real_)
        if (is.numeric(v)) return(v)
        if (is.character(v)) return(as.numeric(v))
        if (is.list(v)) return(as.numeric(v[[1]]))
        return(NA_real_)
      })
    )
  )) %>%
  rename(
    dmg_2WAS    = `2025-10-25_Damage(0-5)/Severity`,
    dmg_3WAS    = `2025-10-31_Damage(0-5)/Severity`,
    dmg_4WAS    = `2025-11-07_Damage(0-5)/Severity`,
    dmg_5WAS    = `2025-11-14_Damage(0-5)/Severity`,
    dmg_6WAS    = `2025-11-21_Damage(0-5)/Severity`,
    dmg_7WAS    = `2025-11-28_Damage(0-5)/Severity`,
    dmg_8WAS    = `2025-12-05_Damage(0-5)/Severity`,
    dmg_9WAS    = `2025-12-12_Damage(0-5)/Severity`,
    
    larvae_2WAS = `2025-10-25_Larvae_count`,
    larvae_3WAS = `2025-10-31_Larvae_count`,
    larvae_4WAS = `2025-11-07_Larvae_count`,
    larvae_5WAS = `2025-11-14_Larvae_count`,
    larvae_6WAS = `2025-11-21_Larvae_count`,
    larvae_7WAS = `2025-11-28_Larvae_count`,
    larvae_8WAS = `2025-12-05_Larvae_count`,
    larvae_9WAS = `2025-12-12_Larvae_count`,
    
    eggs_2WAS   = `2025-10-25_Egg_batches_count`,
    eggs_3WAS   = `2025-10-31_Egg_batches_count`,
    eggs_4WAS   = `2025-11-07_Egg_batches_count`,
    eggs_5WAS   = `2025-11-14_Egg_batches_count`,
    eggs_6WAS   = `2025-11-21_Egg_batches_count`,
    eggs_7WAS   = `2025-11-28_Egg_batches_count`,
    eggs_8WAS   = `2025-12-05_Egg_batches_count`,
    eggs_9WAS   = `2025-12-12_Egg_batches_count`,
    
    Pred_2WAS   = `2025-10-25_Generalist Predators`,
    Pred_3WAS   = `2025-10-31_Generalist Predators`,
    Pred_4WAS   = `2025-11-07_Generalist Predators`,
    Pred_5WAS   = `2025-11-14_Generalist Predators`,
    Pred_6WAS   = `2025-11-21_Generalist Predators`,
    Pred_7WAS   = `2025-11-28_Generalist Predators`,
    Pred_8WAS   = `2025-12-05_Generalist Predators`,
    Pred_9WAS   = `2025-12-12_Generalist Predators`
  ) %>%
  mutate(
    Variety = recode(as.character(Variety), !!!variety_map),
    Variety = factor(Variety, levels = ordered_names),
    Variety_Type = if_else(Variety %in% opv_varieties, "OPV", "HV")
  )

long_weekly_ent_clean_data <- weekly_ent_clean_data %>%
  pivot_longer(
    cols = matches("_(\\d)WAS$"),
    names_to = c("variable", "WAS"),
    names_pattern = "(.*)_(\\d)WAS",
    values_to = "value"
  ) %>% 
  mutate(
    WAS = as.numeric(WAS),
    Period = case_when(
      WAS %in% c(2,3) ~ "1-21 DAS",
      WAS %in% c(4,5) ~ "22-35 DAS",
      WAS %in% c(6,7) ~ "36-49 DAS",
      WAS %in% c(8,9) ~ "50-63 DAS",
      TRUE ~ NA_character_
    )
  )

# view(long_weekly_ent_clean_data)

severity_to_pct <- c(
  "0" = 0,
  "1" = 10,
  "2" = 20,
  "3" = 40,
  "4" = 60,
  "5" = 90,
  "6" = 100
)

period_ent_data <- long_weekly_ent_clean_data %>%
  mutate(
    Parameter = case_when(
      grepl("dmg", variable)   ~ "Damage",
      grepl("larvae", variable) ~ "Larvae",
      grepl("eggs", variable)   ~ "Eggs",
      TRUE ~ variable
    ),
    Value_pct = case_when(
      Parameter == "Damage" ~ severity_to_pct[as.character(value)],
      TRUE ~ as.numeric(value)
    )
  ) %>%
  dplyr::select(
    Plant_ID, Variety, Plot,
    Parameter, Period, WAS,
    Value_pct, Variety_Type
  )

period_collapsed <- period_ent_data %>%
  group_by(Plant_ID, Variety, Plot, Parameter, Period) %>%
  summarise(Value_pct = mean(Value_pct, na.rm = TRUE), .groups = "drop")

# ============================================================
# 6) Summaries & plots (Damage, Incidence, Larvae, Eggs, Cumulative)
# ============================================================
# Damage summary

# Summary table for damage by Variety × Period
damage_summary <- period_ent_data %>%
  filter(Parameter == "Damage") %>%
  group_by(Variety, Period) %>%
  summarise(
    mean_damage = mean(Value_pct, na.rm = TRUE),
    se_damage   = sd(Value_pct, na.rm = TRUE) / sqrt(sum(!is.na(Value_pct))),
    .groups     = "drop"
  )
# view(damage_summary)
# Plot: Damage (%) across varieties by Period
ggplot(damage_summary, aes(x = Variety, y = mean_damage, fill = Period)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_errorbar(
    aes(ymin = mean_damage - se_damage,
        ymax = mean_damage + se_damage),
    width = 0.18,
    position = position_dodge(width = 0.75)
  ) +
  scale_fill_manual(values = period_cols, name = "Period") +
  labs(
    title = "Fall Armyworm Damage (%) Across Varieties (Days After Sowing)",
    x = "Variety",
    y = "Mean Damage (%)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title  = element_text(face = "bold", size = 15)
  )

# Incidence (%) 
incidence_summary <- period_ent_data %>%
  filter(Parameter == "Damage") %>%
  mutate(inc = ifelse(Value_pct > 0, 1, 0)) %>%   # fixed: use Value_pct
  group_by(Variety, Period) %>%
  summarise(
    incidence = mean(inc, na.rm = TRUE) * 100,
    .groups = "drop"
  )
ggplot(incidence_summary, aes(x = Variety, y = incidence, fill = Period)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  scale_fill_manual(values = period_cols, name = "Period") +
  labs(
    title = "FAW Incidence (%) Across Varieties (1–63 DAS)",
    y = "Incidence (%)",
    x = "Variety"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(face = "bold", size = 15)
  )

# Mean larvae per plant
larvae_summary <- period_ent_data %>%
  filter(Parameter == "Larvae") %>%
  group_by(Variety, Period) %>%
  summarise(mean_larvae = mean(Value_pct, na.rm = TRUE),
            se_larvae = sd(Value_pct, na.rm = TRUE) / sqrt(sum(!is.na(Value_pct))),
            .groups = "drop")
ggplot(larvae_summary, aes(x = Variety, y = mean_larvae, fill = Period)) +
  geom_col(position = position_dodge(width = .8), width = 0.72) +
  geom_errorbar(aes(ymin = mean_larvae - se_larvae, ymax = mean_larvae + se_larvae),
  width = 0.2, position = position_dodge(width = .8)) +
  scale_fill_manual(values = period_cols, name = "Week") +
  labs(title = "Mean FAW Larvae per Plant (1–63 DAS)",
       y = "Mean larvae per plant", x = "Variety") +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(face = "bold", size = 15)
  )
# Egg batch presence (%)
# Summary of egg batch presence (%) by Variety × Period
eggs_summary <- period_ent_data %>%
  filter(Parameter == "Eggs") %>%
  mutate(
    egg_present = ifelse(Value_pct >= 1, 1, 0)   # Eggs present if count ≥ 1
  ) %>%
  group_by(Variety, Period) %>%
  summarise(
    egg_pct = mean(egg_present, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Plot: Egg batch presence across varieties by Period
ggplot(eggs_summary, aes(x = Variety, y = egg_pct, fill = Period)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  scale_fill_manual(values = period_cols, name = "Period") +
  labs(
    title = "FAW Egg Batch Presence (%) Across Varieties (1–63 DAS)",
    y = "Plots with Egg Batches (%)",
    x = "Variety"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(face = "bold", size = 15)
  )

# Cumulative larvae totals
larvae_totals <- weekly_ent_clean_data %>%
  group_by(Variety) %>%
  summarise(
    total_2WAS = sum(as.numeric(larvae_2WAS), na.rm = TRUE),
    total_3WAS = sum(as.numeric(larvae_3WAS), na.rm = TRUE),
    total_4WAS = sum(as.numeric(larvae_4WAS), na.rm = TRUE),
    total_5WAS = sum(as.numeric(larvae_5WAS), na.rm = TRUE),
    total_6WAS = sum(as.numeric(larvae_6WAS), na.rm = TRUE),
    total_7WAS = sum(as.numeric(larvae_7WAS), na.rm = TRUE),
    total_8WAS = sum(as.numeric(larvae_8WAS), na.rm = TRUE),
    total_9WAS = sum(as.numeric(larvae_9WAS), na.rm = TRUE),
    total_all = total_2WAS + total_3WAS + total_4WAS + total_5WAS + 
      total_6WAS + total_7WAS + total_8WAS + total_9WAS, 
    .groups = "drop"
  )
# sum(larvae_totals$total_all)

ggplot(larvae_totals, aes(x = Variety, y = total_all)) +
  geom_col(fill = "#e6550d", width = 0.7) +
  geom_text(aes(label = total_all), vjust = -0.3, size = 3) +
  labs(title = "Total FAW Larval Count per Variety (1 - 63 DAS)",
       y = "Total larvae (count)", x = "Variety") +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(face = "bold", size = 15)
  )

# OPV/HYBRID: group-specific damage plots 
# ============================================================
opv_set <- c("Sammaz 15","Sammaz 52","Sammaz 60","Sammaz 66","Sammaz 59","Sammaz 51")
hybrid_set    <- c("Oba Super 4","Oba Super 6","Oba Super 15","SC 719","SC 649")

plot_group_damage <- function(dset, varset, title) {
  dset %>%
    filter(Variety %in% varset) %>%
    ggplot(aes(x = factor(Variety, levels = ordered_names), y = mean_damage, fill = Period)) +
    geom_col(position = position_dodge(width = .8), width = 0.72) +
    geom_errorbar(
      aes(ymin = mean_damage - se_damage,
          ymax = mean_damage + se_damage),
      width = 0.18,
      position = position_dodge(width = 0.75)
    )+
    scale_fill_manual(values = period_cols, name = "Week") +
    labs(title = title, y = "Mean Damage (%)", x = "Variety") +
    theme_classic(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      plot.title = element_text(face = "bold", size = 15)
    )
}

plot_group_damage(damage_summary, opv_set, "Open Pollinated Varieties — FAW Damage (1-63 DAS)")
plot_group_damage(damage_summary, hybrid_set,    "Hybrid Varieties — FAW Damage (1–63 DAS)")

# -----
# Creating single period plots
damage_plot <- function(period_label) {
  ggplot(
    damage_summary %>% filter(Period == period_label),
    aes(x = Variety, y = mean_damage, fill = Period)
  ) +
    geom_col(width = 0.7) +
    geom_errorbar(aes(ymin = mean_damage - se_damage,
                      ymax = mean_damage + se_damage),
                  width = 0.2) +
    scale_fill_manual(values = period_cols) +
    labs(
      title = paste("FAW Damage (%) at", period_label),
      x = "Variety",
      y = "Mean Damage (%)"
    ) +
    theme_classic(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      plot.title = element_text(face = "bold", size = 15)
    )
}

# Plots for 21 DAS, 35 DAS, 49 DAS ----
plot_damage_21DAS <- damage_plot("1-21 DAS")
plot_damage_35DAS <- damage_plot("22-35 DAS")
plot_damage_49DAS <- damage_plot("36-49 DAS")
plot_damage_63DAS <- damage_plot("50-63 DAS")

# Display all
plot_damage_21DAS
plot_damage_35DAS
plot_damage_49DAS
plot_damage_63DAS

# Correlation: Damage vs Larvae -----
damage_larvae <- period_ent_data %>%
  filter(Parameter %in% c("Damage","Larvae")) %>%
  pivot_wider(names_from = Parameter, values_from = Value_pct) %>%
  drop_na(Damage, Larvae)

ggscatter(damage_larvae, x = "Damage", y = "Larvae",
          color = "Variety",
          palette = if(exists("variety_cols")) variety_cols else NULL,
          add = "reg.line",
          add.params = list(color = "black", linetype = 2),
          conf.int = FALSE,
          cor.coef = TRUE,
          cor.method = "pearson",
          shape = NA) +
  facet_wrap(~Variety, scales = "free") +
  labs(title = "FAW Damage vs Larvae per Variety",
       x = "Damage (%)", y = "Larvae count") +
theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title  = element_text(face = "bold", size = 15)
  )

# heatmaps ----
damage_mat <- damage_summary %>%
  dplyr::select(Variety, Period, mean_damage) %>%
  pivot_wider(names_from = Period, values_from = mean_damage) %>%
  as.data.frame()

# Set row names
rownames(damage_mat) <- damage_mat$Variety
damage_mat$Variety <- NULL

# Heatmap - Damage (%)
pheatmap(
  damage_mat,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  color = colorRampPalette(c("white","yellow","red"))(100),
  main = "Heatmap of FAW Damage (%) Across Varieties",
  fontsize = 10,
  border_color = "grey80"
)

pheatmap(
  damage_mat,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  color = colorRampPalette(c("white","orange","red","black"))(120),
  main = "Clustering of Varieties Based on FAW Damage (%)",
  fontsize = 11,
  border_color = NA,
  scale = "row"   # Standardizes rows
)

# Prepare larvae matrix
larvae_mat <- larvae_summary %>%
  dplyr::select(Variety, Period, mean_larvae) %>%
  pivot_wider(names_from = Period, values_from = mean_larvae) %>%
  as.data.frame()

rownames(larvae_mat) <- larvae_mat$Variety
larvae_mat$Variety <- NULL

# Heatmap - Larvae
pheatmap(
  larvae_mat,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  color = colorRampPalette(c("white","lightblue","blue","navy"))(100),
  main = "Heatmap of FAW Larvae per Plant",
  fontsize = 10,
  border_color = "grey80"
)
pheatmap(
  larvae_mat,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  color = colorRampPalette(c("white","skyblue","blue","black"))(120),
  main = "Clustering of Varieties Based on Larval Infestation",
  fontsize = 11,
  border_color = NA,
  scale = "row"   # Helps compare patterns, not absolute values
)


# Generalized Linear Mixed Modelling 
damage_dat <- period_ent_data %>%
  filter(Parameter == "Damage") %>%
  drop_na(Value_pct)
larvae_dat <- period_ent_data %>%
  filter(Parameter == "Larvae") %>%
  drop_na(Value_pct)

# Mixed model: Damage percentage
damage_mod <- lmer(Value_pct ~ Variety * Period + (1 | Plot), data = damage_dat)
damage_emm <- emmeans(damage_mod, ~ Variety | Period)
damage_cld <- cld(damage_emm, Letters = letters, adjust = "tukey")
print(damage_cld)


# Mixed model: Larva counts on each species
larva_mod <- lmer(Value_pct ~ Variety * Period + (1 | Plot), data = larvae_dat)
larva_emm <- emmeans(larva_mod, ~ Variety | Period)
larva_cld <- cld(larva_emm, Letters = letters, adjust = "tukey")
print(larva_cld)

ggplot(as.data.frame(damage_cld),
       aes(x = Variety, y = emmean, fill = Period)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
                width = 0.2, position = position_dodge(0.8)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = .group),
            position = position_dodge(width = 0.8),
            vjust = -4.7, size = 6, color="black") +
  labs(title = "Damage Severity on Varieties across Periods",
       y = "Mean Damage (%)", x = "Variety") +
  scale_fill_manual(values = period_cols, name = "Period")+
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title  = element_text( face= "bold", size = 15)
  )

ggplot(as.data.frame(larva_cld),
       aes(x = Variety, y = emmean, fill = Period)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
                width = 0.2, position = position_dodge(0.8)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = .group),
            position = position_dodge(width = 0.8),
            vjust = -4.7, size = 6, color="black") +
  labs(title = "Larva Abundance on Varieties across Periods",
       y = "Mean", x = "Variety") +
  scale_fill_manual(values = period_cols, name = "Period")+
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title  = element_text( face= "bold", size = 15)
  )


# Objective 2 starts here ----
damage_type_summary <- period_ent_data %>%
  filter(Parameter == "Damage") %>%
  group_by(Variety_Type, Period) %>%
  summarise(
    mean_damage = mean(Value_pct, na.rm = TRUE),
    se_damage   = sd(Value_pct, na.rm = TRUE) / sqrt(sum(!is.na(Value_pct))),
    .groups = "drop"
  )


ggplot(damage_type_summary,
       aes(x = Variety_Type, y = mean_damage, fill = Period)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(
    aes(ymin = mean_damage - se_damage,
        ymax = mean_damage + se_damage),
    width = 0.2,
    position = position_dodge(0.7)
  ) +
  scale_fill_manual(values = period_cols) +
  labs(
    title = "FAW Damage (%) — OPV vs Hybrid Varieties",
    x = "Variety Type",
    y = "Mean Damage (%)"
  ) +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))



damage_mod_type <- lmer(
  Value_pct ~ Variety_Type * Period + (1 | Plot),
  data = damage_dat
)

anova(damage_mod_type)

emm_type <- emmeans(damage_mod_type, ~ Variety_Type | Period)
cld(emm_type, Letters = letters, adjust = "tukey")


larva_mod_type <- lmer(
  Value_pct ~ Variety_Type * Period + (1 | Plot),
  data = larvae_dat
)
anova(larva_mod_type)
emm_type <- emmeans(larva_mod_type, ~ Variety_Type | Period)
cld(emm_type, Letters = letters, adjust = "tukey")

