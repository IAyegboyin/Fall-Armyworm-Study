# Introductory part ----
# Date: January 3oth, 2026
# Author: Ismail A. AYEGBOYIN
# Real-time analysis script of the host plant resistance trial
# 11 maize varieties (Open pollinated and Hybrid)

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

# Loading packages ----
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

# Colour codes for uniform colours ---
period_cols <- c(
  "8-21 DAS" = "#1b9e77", 
  "22-35 DAS" = "#d95f02",
  "36-49 DAS" = "#7570b3",  
  "50-63 DAS" = "#e7298a"
)
variety_type_cols <- c(
  "HV" = "#66a61e",
  "OPV" = "#d95f02"  
)
week_cols <- c(
  "2 WAS" = "#1b9e77",
  "3 WAS" = "#d95f02",  
  "4 WAS" = "#7570b3",  
  "5 WAS" = "#e7298a",  
  "6 WAS" = "#e6ab02",
  "7 WAS" = "#66a61e",
  "8 WAS" = "#1f78b4"
)
# variety colours (keys match variety names)
variety_cols <- set_names(
  c("#d95f02","#1f78b4","#b2df8a","#1b9e77","#fb9a99","#7570b3","#666666","#e7298a","#a6761d","#e6ab02","#66a61e"),
  ordered_names
)

# Loading, cleaning and preparing data from Google Sheet ----
Weekly_Monitoring <- read_sheet(
  "https://docs.google.com/spreadsheets/d/14KYjkx49VFTbC8AI7G0ev2uZNpbIKD3OwCbWAQxlWco/edit?gid=0#gid=0",
  sheet = "Weekly_Monitoring"
)
# glimpse(Weekly_Monitoring) - just to check the data 

# built a function to clean  weekly data
clean_weekly_ent_data <- function(data,
                                  variety_map,
                                  ordered_names,
                                  opv_varieties) {
  
  data %>%
    mutate(across(
      where(is.list),
      ~ suppressWarnings(
        purrr::map_dbl(.x, function(v) {
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
      Pred_9WAS   = `2025-12-12_Generalist Predators`,
    
      lmort_2WAS   = `2025-10-25_Larva Mortality`,
      lmort_3WAS   = `2025-10-31_Larva Mortality`,
      lmort_4WAS   = `2025-11-07_Larva Mortality`,
      lmort_5WAS   = `2025-11-14_Larva Mortality`,
      lmort_6WAS   = `2025-11-21_Larva Mortality`,
      lmort_7WAS   = `2025-11-28_Larva Mortality`,
      lmort_8WAS   = `2025-12-05_Larva Mortality`,
      lmort_9WAS   = `2025-12-12_Larva Mortality`,
      
      notes_2WAS   = `2025-10-25_Notes`,
      notes_3WAS   = `2025-10-31_Notes`,
      notes_4WAS   = `2025-11-07_Notes`,
      notes_5WAS   = `2025-11-14_Notes`,
      notes_6WAS   = `2025-11-21_Notes`,
      notes_7WAS   = `2025-11-28_Notes`,
      notes_8WAS   = `2025-12-05_Notes`,
      notes_9WAS   = `2025-12-12_Notes`
    ) %>%
    
    mutate(
      Variety = recode(as.character(Variety), !!!variety_map),
      Variety = factor(Variety, levels = ordered_names),
      Variety_Type = if_else(Variety %in% opv_varieties, "OPV", "HV")
    )
}

weekly_ent_clean_data <- clean_weekly_ent_data(
  data = Weekly_Monitoring,
  variety_map = variety_map,
  ordered_names = ordered_names,
  opv_varieties = opv_varieties
)
long_weekly_ent_clean_data <- weekly_ent_clean_data %>%
  dplyr::select(-matches("^notes_\\d+WAS$")) %>% # 👈 remove all notes columns because they are characters 
  pivot_longer(
    cols = matches("_(\\d)WAS$"),
    names_to = c("variable", "WAS"),
    names_pattern = "(.*)_(\\d)WAS",
    values_to = "value"
  ) %>%
  mutate(
    WAS = as.numeric(WAS),
    Period = factor(
      case_when(
        WAS %in% c(2,3) ~ "8-21 DAS",
        WAS %in% c(4,5) ~ "22-35 DAS",
        WAS %in% c(6,7) ~ "36-49 DAS",
        WAS %in% c(8,9) ~ "50-63 DAS",
        TRUE ~ NA_character_
      ),
      levels = c("8-21 DAS", "22-35 DAS", "36-49 DAS", "50-63 DAS")
    )
  )

# view(long_weekly_ent_clean_data) just glimpsing again 

# changing my damage scores on the field to percentage from the field
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
      grepl("pred", variable) ~ "Predator_Presence",
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
# view(period_ent_data)

period_collapsed <- period_ent_data %>%
  group_by(Plant_ID, Variety, Plot, Parameter, Period) %>%
  summarise(Value_pct = mean(Value_pct, na.rm = TRUE), .groups = "drop")

# view(period_collapsed)

# calling data by variety types 
opv_ent_data <- period_ent_data %>%
  filter(Variety_Type == "OPV") # open pollinated varieties
hv_ent_data <- period_ent_data %>%
  filter(Variety_Type == "HV") # hybrid varieties

# Objective 1: to evaluate the field resistance of eleven maize varieties to Spodoptera frugiperda ----
summarise_by_period <- function(data,
                                parameter_name,
                                value_col = "Value_pct") {
  
  data %>%
    filter(Parameter == parameter_name) %>%
    group_by(Variety, Variety_Type, Period) %>%  # Keep period here, I will need it later 
    summarise(
      mean = mean(.data[[value_col]], na.rm = TRUE),
      se   = sd(.data[[value_col]], na.rm = TRUE) /
        sqrt(sum(!is.na(.data[[value_col]]))),
      .groups = "drop"
    )
}

# summaries (mean and SE) of varieties by types (OPV et HV)
opv_dmg_summary  <- summarise_by_period(opv_ent_data, "Damage")
opv_lar_summary  <- summarise_by_period(opv_ent_data, "Larvae")
hv_dmg_summary  <- summarise_by_period(hv_ent_data, "Damage")
hv_lar_summary  <- summarise_by_period(hv_ent_data, "Larvae")

# plotting damage (%) of different variety types by period
get_variety_type_label <- function(data) {
  types <- unique(na.omit(data$Variety_Type))
  if (length(types) == 1) {
    if (types == "OPV") return("Open-Pollinated Varieties")
    if (types == "HV")  return("Hybrid Varieties")
  }
  "Varieties"
}

plot_by_period <- function(summary_data,
                           y_label,
                           base_title,
                           ylim_max) {
  
  variety_label <- get_variety_type_label(summary_data)
  
  ggplot(summary_data,
         aes(x = Variety, y = mean, fill = Variety)) +
    
    geom_col(width = 0.65) +
    
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      width = 0.18
    ) +
    
    facet_wrap(
      ~ Period,
      nrow = 2,
      labeller = labeller(
        Period = c(
          "8-21 DAS"  = "8–21 DAS",
          "22-35 DAS" = "22–35 DAS",
          "36-49 DAS" = "36–49 DAS",
          "50-63 DAS" = "50–63 DAS"
        )
      )
    ) +
    
    scale_y_continuous(
      limits = c(0, ylim_max),
      expand = c(0, 0)
    ) +
    
    scale_fill_manual(
      values = variety_cols,
      name   = "Variety"
    ) +
    
    labs(
      title = paste(base_title, "in", variety_label, "(DAS – Days After Sowing)"),
      x = "Maize Variety",
      y = y_label
    ) +
    
    theme_classic(base_size = 13, base_family = "Times New Roman") +
    
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      plot.title  = element_text(face = "bold", size = 15),
      strip.text  = element_text(face = "bold")
    )
}

# plots for summaries
plot_by_period(
  summary_data = opv_dmg_summary,
  y_label      = "Mean Damage (%)",
  base_title   = "Fall Armyworm Damage",
  ylim_max     = 90
)
plot_by_period(
  summary_data = hv_dmg_summary,
  y_label      = "Mean Damage (%)",
  base_title   = "Fall Armyworm Damage",
  ylim_max     = 90
)
plot_by_period(
  summary_data = opv_lar_summary,
  y_label      = "Mean Fall Armyworm",
  base_title   = "Average Fall Armyworm Count",
  ylim_max     = 1.2
)
plot_by_period(
  summary_data = hv_lar_summary,
  y_label      = "Mean Fall Armyworm",
  base_title   = "Average Fall Armyworm Count",
  ylim_max     = 1.2
)


plot_damage_larvae_trend <- function(data,
                                     scale_factor = 100,
                                     variety_colors = NULL,
                                     title_prefix = "Trend of FAW Damage and Larval Counts") {
  
  trend_summary <- data %>%
    dplyr::filter(Parameter %in% c("Damage", "Larvae")) %>%
    mutate(
      Period = factor(
        Period,
        levels = c("8-21 DAS","22-35 DAS","36-49 DAS","50-63 DAS"),
        labels = c("8-21","22-35","36-49","50-63")
      ),
      Value = as.numeric(Value_pct)
    ) %>%
    group_by(Variety, Period, Parameter) %>%
    summarise(
      mean_value = mean(Value, na.rm = TRUE),
      se_value   = sd(Value, na.rm = TRUE) / sqrt(sum(!is.na(Value))),
      .groups = "drop"
    ) %>%
    mutate(
      plot_value = ifelse(Parameter == "Larvae",
                          mean_value * scale_factor,
                          mean_value)
    )
  
  ggplot(trend_summary,
         aes(x = Period,
             y = plot_value,
             color = Parameter,
             group = Parameter)) +
    
    geom_line(size = 1) +
    geom_point(size = 2.5) +
    
    facet_wrap(~Variety) +
    
    scale_y_continuous(
      name = "Mean Damage (%)",
      limits = c(0,100),
      expand = c(0,0),
      sec.axis = sec_axis(~./scale_factor,
                          name = "Mean Larval Count")
    ) +
    scale_color_manual(
      values = c(
        "Damage" = "#D55E00",
        "Larvae" = "#0072B2"
      )
    ) +
    labs(
      x = "Days After Sowing (DAS)",
      color = "Parameter",
      title = title_prefix
    ) +
    theme_classic(base_size = 13, base_family = "Times New Roman") +
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 15),
      legend.position = "top"
    )
}

plot_damage_larvae_trend(
  data = opv_ent_data,
  title_prefix = "Trend of FAW Damage and Larvae in OPV Varieties"
)
plot_damage_larvae_trend(
  data = hv_ent_data,
  title_prefix = "Trend of FAW Damage and Larvae in Hybrid Varieties"
)

plot_cumulative_damage_cum_larvae <- function(data,
                                              title_prefix = "Cumulative FAW Damage and Larval Counts") {
  
  # 🔹 Clean + correct summarisation
  trend_summary <- data %>%
    filter(Parameter %in% c("Damage", "Larvae")) %>%
    mutate(
      Period = factor(
        Period,
        levels = c("8-21 DAS","22-35 DAS","36-49 DAS","50-63 DAS"),
        labels = c("8-21","22-35","36-49","50-63")
      ),
      Value = as.numeric(Value_pct)
    ) %>%
    group_by(Variety, Period, Parameter) %>%
    summarise(
      value = if (first(Parameter) == "Larvae") {
        sum(Value, na.rm = TRUE)     # ✅ total larvae
      } else {
        mean(Value, na.rm = TRUE)    # ✅ mean damage
      },
      .groups = "drop"
    ) %>%
    arrange(Variety, Parameter, Period) %>%
    group_by(Variety, Parameter) %>%
    mutate(
      cum_value = cumsum(value),
      cum_value = ifelse(Parameter == "Damage",
                         pmin(cum_value, 100),  # capping the damage here ...
                         cum_value)
    ) %>%
    ungroup()
  # 🔹 Auto scale larva count 
  max_larvae <- max(trend_summary$cum_value[trend_summary$Parameter == "Larvae"], 
                    na.rm = TRUE)
  scale_factor <- 100 / max_larvae
  trend_summary <- trend_summary %>%
    mutate(
      plot_value = ifelse(Parameter == "Larvae",
                          cum_value * scale_factor,
                          cum_value)
    )
  # plot here 
  ggplot(trend_summary,
         aes(x = Period,
             y = plot_value,
             color = Parameter,
             group = Parameter)) +
    
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    facet_wrap(~Variety) +
    scale_y_continuous(
      name = "Cumulative Damage (%)",
      expand = c(0,0),
      sec.axis = sec_axis(~./scale_factor,
                          name = "Cumulative Larval Count")
    ) +
    scale_color_manual(
      values = c(
        "Damage" = "#D55E00",
        "Larvae" = "#0072B2"
      )
    ) +
    labs(
      x = "Days After Sowing (DAS)",
      color = "Parameter",
      title = title_prefix
    ) +
    theme_classic(base_size = 13, base_family = "Times New Roman") +
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 15),
      legend.position = "top"
    )
}
plot_cumulative_damage_cum_larvae(
  data = opv_ent_data,
  title_prefix = "Cumulative FAW Damage and Larvae in OPV Varieties"
)
plot_cumulative_damage_cum_larvae(
  data = hv_ent_data,
  title_prefix = "Cumulative FAW Damage and Larvae in Hybrid Varieties"
)



# damage incidence of damage in variety type 
plot_faw_incidence <- function(summary_data,
                                     parameter_name = "Damage",
                                     title_suffix = "(1–63 DAS)") {
  
  variety_label <- get_variety_type_label(summary_data)
  
  incidence_summary <- summary_data %>%
    dplyr::filter(Parameter == parameter_name) %>%
    dplyr::mutate(inc = ifelse(Value_pct > 0, 1, 0)) %>%
    dplyr::group_by(Variety, Period) %>%
    dplyr::summarise(
      incidence = mean(inc, na.rm = TRUE) * 100,
      .groups = "drop"
    )
  
  ggplot(incidence_summary,
         aes(x = Variety, y = incidence, fill = Variety)) +
    
    geom_col(width = 0.65) +
    
    facet_wrap(
      ~ Period,
      nrow = 2,
      labeller = labeller(
        Period = c(
          "8-21 DAS"  = "8–21 DAS",
          "22-35 DAS" = "22–35 DAS",
          "36-49 DAS" = "36–49 DAS",
          "50-63 DAS" = "50–63 DAS"
        )
      )
    ) +
    scale_fill_manual(
      values = variety_cols,
      name   = "Variety"
    ) +
    labs(
      title = paste("FAW Incidence (%) Across", variety_label, title_suffix),
      x = "Variety",
      y = "Incidence (%)"
    ) +
    
    theme_classic(base_size = 13, base_family = "Times New Roman") +
    
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      plot.title  = element_text(face = "bold", size = 15),
      strip.text  = element_text(face = "bold")
    )
}

plot_faw_incidence(hv_ent_data)
plot_faw_incidence(opv_ent_data)



# Correlation: Damage vs Larvae
plot_dmg_lar_correlation <- function(data,
                                     param_x = "Damage",
                                     param_y = "Larvae",
                                     cor_method = "pearson",
                                     point_size = 2.5,
                                     point_shape = 19,
                                     palette = if (exists("variety_cols")) variety_cols else NULL) {
  
  # Determine type for title
  variety_type_label <- data %>%
    pull(Variety_Type) %>%
    unique()
  variety_title <- case_when(
    all(variety_type_label == "OPV") ~ "Open Pollinated Varieties",
    all(variety_type_label == "HV")  ~ "Hybrid Varieties",
    TRUE                              ~ "Maize Varieties"
  )
  # Prepare correlation data
  cor_data <- data %>%
    filter(Parameter %in% c(param_x, param_y)) %>%
    pivot_wider(names_from = Parameter, values_from = Value_pct) %>%
    group_by(Plant_ID, Variety, WAS, Period) %>%
     summarise(
      Damage = mean(Damage, na.rm = TRUE),
      Larvae = mean(Larvae, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    drop_na(Damage, Larvae)
  
  # Plot
  ggscatter(
    cor_data,
    x = param_x,
    y = param_y,
    color = "Variety",
    point = TRUE,
    palette = palette,
    add = "reg.line",
    add.params = list(color = "black", linetype = 1),
    conf.int = FALSE,
    cor.coef = TRUE,
    cor.method = cor_method,
    shape = point_shape,
    size  = point_size,
    position = position_jitter(width = 0.5, height = 0.1)
  ) +
    facet_wrap(~Variety, scales = "free") +
    labs(
      title = paste("FAW Damage vs Larvae in", variety_title),
      x = "Damage (%)",
      y = "Larvae count"
    ) +
    theme_classic(base_size = 13, base_family = "Times New Roman") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      plot.title  = element_text(face = "bold", size = 15)
    )
}
plot_dmg_lar_correlation(opv_ent_data)
plot_dmg_lar_correlation(hv_ent_data)


  
plot_faw_larvae_totals <- function(data,
                                   value_col = "Value_pct",
                                   parameter_name = "Larvae") {
  
  # Variety labeling again
  variety_label <- if (exists("get_variety_type_label")) {
    get_variety_type_label(data)
  } else {
    ""
  }
  # Prepare larvae data
  larvae_data <- data %>%
    filter(Parameter == parameter_name) %>%
    mutate(Value = as.numeric(.data[[value_col]]))
  
  # Total larvae per Variety (bar plot)
  larvae_totals_variety <- larvae_data %>%
    group_by(Variety) %>%
    summarise(
      total_all = sum(Value, na.rm = TRUE),
      .groups = "drop"
    )
  bar_plot <- ggplot(larvae_totals_variety,
                     aes(x = Variety, y = total_all, fill = Variety)) +
    geom_col(width = 0.5) +
    geom_text(aes(label = round(total_all, 1)), vjust = -0.3, size = 3) +
    scale_fill_manual(values = variety_cols) +
    labs(
      title = paste("Total FAW Larval Load per Variety on", variety_label, "(1–63 DAS)"),
      x = "Variety",
      y = "Total larvae (count)"
    ) +
    theme_classic(base_size = 13, base_family = "Times New Roman") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      plot.title  = element_text(face = "bold", size = 15),
      legend.position = "none"
    )
# Violin plot of raw larvae distribution
  larvae_raw <- larvae_data %>%
    filter(!is.na(Value))
  violin_plot <- ggplot(larvae_raw, aes(x = Variety, y = Value, fill = Variety)) +
    geom_violin(alpha = 0.7, trim = FALSE) +
    scale_fill_manual(values = variety_cols) +
    labs(
      title = paste("Distribution of FAW Larvae per Variety on", variety_label, "(1–63 DAS)"),
      x = "Variety",
      y = "Larvae count per observation"
    ) +
    theme_classic(base_size = 13, base_family = "Times New Roman") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      plot.title  = element_text(face = "bold", size = 15),
      legend.position = "none"
    )
  print(bar_plot)
  print(violin_plot)
}
plot_faw_larvae_totals(opv_ent_data)
plot_faw_larvae_totals(hv_ent_data)

# generalised linear mixed model here now to check for significant differences
run_glmm_analysis <- function(data, 
                              parameter, 
                              title_prefix, 
                              ylab, 
                              ylim_max) {
  dat <- data %>%
    filter(Parameter == parameter) %>%
    drop_na(Value_pct)
  # raw summary (mean + se)
  summary_df <- dat %>%
    group_by(Variety, Period) %>%
    summarise(
      mean = mean(Value_pct, na.rm = TRUE),
      se   = sd(Value_pct, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  # model
  mod <- lmer(Value_pct ~ Variety * Period + (1 | Plot), 
              data = dat)
  # 4️⃣ Estimated mm
  emm <- emmeans(mod, ~ Variety | Period)
  cld_res <- cld(emm, Letters = letters, adjust = "tukey")
  cld_df <- as.data.frame(cld_res) %>%
    dplyr::select(Variety, Period, .group) %>%
    mutate(.group = gsub(" ", "", .group))
  # Merge CLD with raw summary
  plot_df <- left_join(summary_df, cld_df,
                       by = c("Variety", "Period"))
  # Plot
  plt <- ggplot(plot_df,
                aes(x = Variety, y = mean, fill = Variety)) +
    geom_col(width = 0.7) +
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      width = 0.2,
      linewidth = 0.7
    ) +
    geom_text(
      aes(y = mean + se + (0.05 * ylim_max),
          label = .group),
      size = 5
    ) +
    facet_wrap(
      ~ Period,
      nrow = 2,
      labeller = labeller(
        Period = c(
          "8-21 DAS"  = "8–21 DAS",
          "22-35 DAS" = "22–35 DAS",
          "36-49 DAS" = "36–49 DAS",
          "50-63 DAS" = "50–63 DAS"
        )
      )
    ) +
    scale_fill_manual(
      values = variety_cols,
      name = "Variety"
    ) +
    coord_cartesian(ylim = c(0, ylim_max)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = title_prefix,
      y = ylab,
      x = "Maize Variety"
    ) +
    theme_classic(base_size = 13, base_family = "Times New Roman") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      plot.title  = element_text(face = "bold", size = 15),
      strip.text  = element_text(face = "bold")
    )
  return(list(
    data = dat,
    summary = summary_df,
    model = mod,
    cld = cld_res,
    plot = plt
  ))
}

opv_damage <- run_glmm_analysis(
  data = opv_ent_data,
  parameter = "Damage",
  title_prefix = "Damage Severity on Open Pollinated Varieties",
  ylab = "Mean Damage (%)",
  ylim_max = 90
)
opv_damage$plot
opv_damage$cld

opv_larvae <- run_glmm_analysis(
  data = opv_ent_data,
  parameter = "Larvae",
  title_prefix = "Larva Abundance on Open Pollinated Varieties",
  ylab = "Mean FAW Larva Count",
  ylim_max = 1.2
)
opv_larvae$plot
opv_larvae$cld

hv_damage <- run_glmm_analysis(
  data = hv_ent_data,
  parameter = "Damage",
  title_prefix = "Damage Severity on Hybrid Varieties",
  ylab = "Mean Damage (%)",
  ylim_max = 90
)
hv_damage$plot
hv_damage$cld
hv_larvae <- run_glmm_analysis(
  data = hv_ent_data,
  parameter = "Larvae",
  title_prefix = "Larva Abundance on Hybrid Varieties",
  ylab = "Mean FAW Larva Count",
  ylim_max = 1.25
)
hv_larvae$plot
hv_larvae$cld


# heat map plots starts here ---
plot_faw_heatmap <- function(data,
                             variety_col = "Variety",
                             period_col  = "Period",
                             value_col   = "mean",
                             cluster_rows = TRUE,
                             cluster_cols = TRUE,
                             scale_rows   = FALSE,
                             title        = "FAW Heatmap",
                             palette      = c("white", "lightblue", "blue", "navy"),
                             n_colors     = 100,
                             fontsize     = 20,
                             border_color = "grey80") {

  mat <- data %>%
    dplyr::select(
      !!sym(variety_col),
      !!sym(period_col),
      !!sym(value_col)
    ) %>%
    tidyr::pivot_wider(
      names_from  = !!sym(period_col),
      values_from = !!sym(value_col)
    ) %>%
    as.data.frame()
  
  rownames(mat) <- mat[[variety_col]]
  mat[[variety_col]] <- NULL
  
  # Heatmap plot
  pheatmap::pheatmap(
    mat,
    cluster_rows = cluster_rows,
    cluster_cols = cluster_cols,
    scale        = ifelse(scale_rows, "row", "none"),
    color        = colorRampPalette(palette)(n_colors),
    main         = title,
    fontsize     = fontsize,
    border_color = border_color
  )
}


plot_faw_heatmap(
  data = opv_lar_summary,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  scale_rows = FALSE,
  title = "Clustering of Open Pollinated Varieties Based on Larval Infestation",
  palette = c("white","skyblue","blue","black"),
  n_colors = 120,
  fontsize = 11,
  border_color = NA
)
plot_faw_heatmap(
  data = opv_dmg_summary,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  scale_rows = FALSE,
  title = "Clustering of Open Pollinated Varieties Based on FAW Damage",
  palette = c("white","skyblue","blue","black"),
  n_colors = 120,
  fontsize = 11,
  border_color = NA
)
plot_faw_heatmap(
  data = hv_lar_summary,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  scale_rows = FALSE,
  title = "Clustering of Hybrid Varieties Based on Larval Infestation",
  palette = c("white","skyblue","blue","black"),
  n_colors = 120,
  fontsize = 11,
  border_color = NA
)
plot_faw_heatmap(
  data = hv_dmg_summary,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  scale_rows = FALSE,
  title = "Clustering of Hybrid Varieties Based on FAW Damage",
  palette = c("white","skyblue","blue","black"),
  n_colors = 120,
  fontsize = 11,
  border_color = NA
)




# Objective 2: to compare the resistance of hybrid and open-pollinated maize varieties to Spodoptera frugiperda ----

summarise_by_variety_type <- function(data,
                                      parameter_name,
                                      value_col = "Value_pct") {
  
  data %>%
    filter(Parameter == parameter_name) %>%
    group_by(Variety_Type, Period) %>%
    summarise(
      mean = mean(.data[[value_col]], na.rm = TRUE),
      se   = sd(.data[[value_col]], na.rm = TRUE) /
        sqrt(sum(!is.na(.data[[value_col]]))),
      .groups = "drop"
    )
}
# Summaries
vt_dmg_summary <- summarise_by_variety_type(period_ent_data, "Damage")
vt_lar_summary <- summarise_by_variety_type(period_ent_data, "Larvae")


plot_variety_type_by_period <- function(summary_data,
                                              y_label,
                                              base_title) {
  
  ggplot(summary_data,
         aes(x = Variety_Type, y = mean, fill = Variety_Type)) +
    
    geom_col(width = 0.4) +
    
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      width = 0.2
    ) +
    
    facet_wrap(
      ~ Period,
      nrow = 2,
      labeller = labeller(
        Period = c(
          "8-21 DAS"  = "8–21 DAS",
          "22-35 DAS" = "22–35 DAS",
          "36-49 DAS" = "36–49 DAS",
          "50-63 DAS" = "50–63 DAS"
        )
      )
    ) +
    
    scale_fill_manual(
      values = variety_type_cols,  # e.g. OPV vs Hybrid colours
      guide = "none"
    ) +
    
    labs(
      title = base_title,
      x = "Variety Type",
      y = y_label
    ) +
    
    theme_classic(base_size = 13, base_family = "Times New Roman") +
    
    theme(
      plot.title = element_text(face = "bold", size = 15),
      strip.text = element_text(face = "bold")
    )
}

# Damage
plot_variety_type_by_period(
  vt_dmg_summary,
  y_label = "Mean Damage (%)",
  base_title = "Comparison of FAW Damage Between Open Pollinated and Hybrid Varieties"
)
# Larvae
plot_variety_type_by_period(
  vt_lar_summary,
  y_label = "Mean Larval Count",
  base_title = "Comparison of FAW Larval Abundance Between Open Pollinated and Hybrid Varieties"
)

plot_variety_type_trend <- function(data,
                                    scale_factor = 100,
                                    title_prefix = "Trend of FAW Damage and Larval Counts") {
  
  trend_summary <- data %>%
    filter(Parameter %in% c("Damage","Larvae")) %>%
    mutate(
      Period = factor(
        Period,
        levels = c("8-21 DAS","22-35 DAS","36-49 DAS","50-63 DAS"),
        labels = c("8–21","22–35","36–49","50–63")
      ),
      Value = as.numeric(Value_pct)
    ) %>%
    
    group_by(Variety_Type, Period, Parameter) %>%
    summarise(
      mean_value = mean(Value, na.rm = TRUE),
      se_value   = sd(Value, na.rm = TRUE) /
        sqrt(sum(!is.na(Value))),
      .groups = "drop"
    ) %>%
    mutate(
      plot_value = ifelse(Parameter == "Larvae",
                          mean_value * scale_factor,
                          mean_value)
    )
  
  
  ggplot(
    trend_summary,
    aes(x = Period,
                 y = plot_value,
                 color = Parameter,
                 group = Parameter)
  ) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    #geom_errorbar(
      #aes(
        #ymin = plot_value - se_value,
       # ymax = plot_value + se_value
     # ),
    #  width = 0.15
   # ) +
    facet_wrap(
      ~ Variety_Type,
      labeller = ggplot2::labeller(
        Variety_Type = c(
          "HV"  = "Hybrid",
          "OPV" = "Open Pollinated"
        )
      )
    ) +
    scale_y_continuous(
      name = "Mean Damage (%)",
      limits = c(0,100),
      expand = c(0,0),
      sec.axis = ggplot2::sec_axis(
        ~./scale_factor,
        name = "Mean Larval Count"
      )
    ) +
    scale_color_manual(
      values = c(
        "Damage" = "#D55E00",
        "Larvae" = "#0072B2"
      )
    ) +
    labs(
      x = "Days After Sowing (DAS)",
      color = "Parameter",
      title = title_prefix
    ) +
    theme_classic(
      base_size = 13,
      base_family = "Times New Roman"
    ) +
    theme(
      strip.text = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold", size = 15),
      legend.position = "top"
    )
}
plot_variety_type_trend(
  period_ent_data,
  title_prefix = "Trend of FAW Damage and Larvae in Maize Variety Types"
)


# incidence per variety types
incidence_variety_type <- period_ent_data %>%
  filter(Parameter == "Damage") %>%
  mutate(incidence = ifelse(Value_pct > 0, 1, 0)) %>%
  group_by(Variety_Type, Period) %>%
  summarise(
    incidence_pct = mean(incidence, na.rm = TRUE) * 100,
    .groups = "drop"
  )
ggplot(incidence_variety_type,
       aes(x = Variety_Type, y = incidence_pct, fill = Variety_Type)) +
  geom_col(width = 0.4) +
  facet_wrap(
    ~ Period,
    nrow = 2,
    labeller = labeller(
      Period = c(
        "8-21 DAS"  = "8–21 DAS",
        "22-35 DAS" = "22–35 DAS",
        "36-49 DAS" = "36–49 DAS",
        "50-63 DAS" = "50–63 DAS"
      )
    )
  ) +
  scale_fill_manual(
    values = variety_type_cols,
    guide  = "none"
  ) +
  labs(
    title = "FAW Larval Incidence (%) in Open Pollinated and Hybrid Varieties",
    x = "Variety Type",
    y = "Incidence (%)"
  ) +
  theme_classic(base_size = 13, base_family = "Times New Roman") +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    strip.text = element_text(face = "bold")
  )


plot_faw_larvae_variety_type_full <- function(data,
                                              value_col = "Value_pct",
                                              parameter_name = "Larvae",
                                              variety_type_cols = c("Open Pollinated" = "#d95f02",
                                                                    "Hybrid" = "#1b9e77")) {
  
  
  larvae_data <- data %>%
    filter(Parameter == parameter_name) %>%
    mutate(
      Value = as.numeric(.data[[value_col]]),
      Variety_Type = factor(
        Variety_Type,
        levels = c("HV", "OPV"),
        labels = c("Hybrid", "Open Pollinated")
      )
    ) %>%
    filter(!is.na(Value))
  
  #--------------------------------------------------
  # 2️⃣ Poisson GLM for Variety Type
  #--------------------------------------------------
  glm_model <- glm(
    Value ~ Variety_Type,
    family = poisson(link = "log"),
    data = larvae_data
  )
  
  cat("\n==============================\n")
  cat("Poisson GLM Results\n")
  cat("==============================\n")
  print(summary(glm_model))
  print(anova(glm_model, test = "Chisq"))
  
  dispersion_value <- deviance(glm_model) / df.residual(glm_model)
  cat("\nOverdispersion ratio:", dispersion_value, "\n")
  
  #--------------------------------------------------
  # 3️⃣ Violin Plot (Distribution)
  #--------------------------------------------------
  violin_plot <- ggplot(larvae_data,
                        aes(x = Variety_Type, y = Value, fill = Variety_Type)) +
    geom_violin(trim = FALSE, alpha = 0.7) +
    stat_summary(fun = mean, geom = "point", size = 3, color = "black") +
    scale_fill_manual(values = variety_type_cols) +
    labs(
      title = "Distribution of FAW Larval Load (1–63 DAS)",
      x = "Maize Variety Type",
      y = "Larvae per observation"
    ) +
    theme_classic(base_size = 13, base_family = "Times New Roman") +
    theme(plot.title = element_text(face = "bold", size = 15),
          legend.position = "none")
  
  print(violin_plot)
  
  #--------------------------------------------------
  # 4️⃣ Total Larvae per Variety Type
  #--------------------------------------------------
  total_type <- larvae_data %>%
    group_by(Variety_Type) %>%
    summarise(total_all = sum(Value, na.rm = TRUE),
              .groups = "drop")
  
  total_plot <- ggplot(total_type,
                       aes(x = Variety_Type, y = total_all, fill = Variety_Type)) +
    geom_col(width = 0.4) +
    scale_y_continuous(
      limits = c(0, 300)
    )+
    geom_text(aes(label = round(total_all, 1)), vjust = -0.3, size = 4) +
    scale_fill_manual(values = variety_type_cols) +
    labs(
      title = "Total FAW Larval Load (1–63 DAS)",
      x = "Maize Variety Type",
      y = "Total larvae"
    ) +
    theme_classic(base_size = 13, base_family = "Times New Roman") +
    theme(plot.title = element_text(face = "bold", size = 15),
          legend.position = "none")
  print(total_plot)
  
  #--------------------------------------------------
  # 5️⃣ Mean ± SEM per Variety Type
  #--------------------------------------------------
  summary_type <- larvae_data %>%
    group_by(Variety_Type) %>%
    summarise(
      mean_count = mean(Value),
      sem = sd(Value) / sqrt(n()),
      .groups = "drop"
    )
  
  sem_plot <- ggplot(summary_type,
                     aes(x = Variety_Type, y = mean_count, fill = Variety_Type)) +
    geom_col(width = 0.4) +
    geom_errorbar(aes(ymin = mean_count - sem,
                      ymax = mean_count + sem),
                  width = 0.2) +
    scale_y_continuous(
      limits = c(0, 0.6)
    ) +
    scale_fill_manual(values = variety_type_cols) +
    labs(
      title = "Mean FAW Larval Load ± SEM (1–63 DAS)",
      x = "Maize Variety Type",
      y = "Mean larvae count"
    ) +
    theme_classic(base_size = 13, base_family = "Times New Roman") +
    theme(plot.title = element_text(face = "bold", size = 15),
          legend.position = "none")
  print(sem_plot)
  
  #--------------------------------------------------
  # Return model + plots
  #--------------------------------------------------
  return(list(
    model = glm_model,
    dispersion = dispersion_value,
    violin_plot = violin_plot,
    total_plot = total_plot,
    sem_plot = sem_plot
  ))
}

results <- plot_faw_larvae_variety_type_full(period_ent_data)

# model
run_glmm_variety_type <- function(data, 
                                  title_prefix, 
                                  parameter, 
                                  ylab, 
                                  ylim_max) {
  dat <- data %>%
    filter(Parameter == parameter) %>%
    drop_na(Value_pct)
  mod <- lme4::lmer(
    Value_pct ~ Variety_Type * Period + (1 | Plot),
    data = dat
  )
  # Estimated marginal means + CLD letters
  emm <- emmeans::emmeans(mod, ~ Variety_Type | Period)
  cld_res <- multcomp::cld(
    emm,
    Letters = letters,
    adjust = "sidak"
  )
  cld_df <- as.data.frame(cld_res)
  # Raw mean + SEM for plotting
  raw_sum <- dat %>%
    group_by(Variety_Type, Period) %>%
    summarise(
      mean = mean(Value_pct, na.rm = TRUE),
      sd   = sd(Value_pct, na.rm = TRUE),
      n    = n(),
      sem  = sd / sqrt(n),
      .groups = "drop"
    )
  # Merging the computed letters with raw statistics
  plot_df <- raw_sum %>%
    left_join(
      cld_df %>%
        dplyr::select(Variety_Type, Period, .group),
      by = c("Variety_Type", "Period")
    )
  # Create faceted plot
  plt <- ggplot(
    plot_df,
    aes(x = Variety_Type, y = mean, fill = Variety_Type)
  ) +
    geom_col(width = 0.2) +
    geom_errorbar(
      ggplot2::aes(ymin = mean - sem, ymax = mean + sem),
      width = 0.08
    ) +
    geom_text(aes(y = mean + sem, label = .group),
      vjust = -0.4,
      size = 6,
      color = "black"
    ) +
    facet_wrap(
      ~ Period,
      nrow = 1,
      labeller = labeller(
        Period = c(
          "8-21 DAS"  = "8–21 DAS",
          "22-35 DAS" = "22–35 DAS",
          "36-49 DAS" = "36–49 DAS",
          "50-63 DAS" = "50–63 DAS"
        )
      )
    ) +
    coord_cartesian(ylim = c(0, ylim_max)) +
    scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    scale_fill_manual(
      name = "Maize Variety Type",
      values = variety_type_cols,
      labels = c(
        "HV"  = "Hybrid",
        "OPV" = "Open Pollinated"
      )
    ) +
    scale_x_discrete(
      labels = c(
        "HV"  = "Hybrid",
        "OPV" = "Open Pollinated"
      )
    ) +
    labs(
      title = title_prefix,
      x = "Maize Variety Type",
      y = ylab
    ) +
    theme_classic(
      base_size = 13,
      base_family = "Times New Roman"
    ) +
    theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
      plot.title  = ggplot2::element_text(face = "bold", size = 15),
      strip.text  = ggplot2::element_text(face = "bold")
    )
  return(list(
    model = mod,
    emm   = emm,
    cld   = cld_res,
    plot  = plt
  ))
}

vt_damage <- run_glmm_variety_type(
  period_ent_data,
  title_prefix = "Damage Severity on Maize Varieties",
  parameter = "Damage",
  ylab = "Damage (%)",
  ylim_max = 90
)
vt_larvae <- run_glmm_variety_type(
  period_ent_data,
  title_prefix = "Average Larval Count on Maize Varieties",
  parameter = "Larvae",
  ylab = "Average Larvae Count",
  ylim_max = 1.2
)
vt_damage$plot
vt_damage$cld

vt_larvae$plot
vt_larvae$cld

# egg data analysis ----
egg_data <- period_ent_data %>%
  filter(Parameter == "Eggs")%>%
  dplyr::mutate(
    log_Larvae = log(Value_pct + 1)
  )
  egg_data$Value_pct <- as.numeric(egg_data$Value_pct)
# view(egg_data)
# Objective 3: to examine the relationship between plant physiological traits and FAW resistance ----