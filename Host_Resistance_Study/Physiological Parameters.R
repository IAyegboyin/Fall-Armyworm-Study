# Loading, cleaning and preparing data from Google Sheet ----
physio_prmtr <- read_sheet(
  "https://docs.google.com/spreadsheets/d/14KYjkx49VFTbC8AI7G0ev2uZNpbIKD3OwCbWAQxlWco/edit?gid=0#gid=0",
  sheet = "Physiological_data"
)
# glimpse(physio_prmtr) - just to check the data 

physio_prmtr_clean_data <- physio_prmtr %>%
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
    plantheight_2WAS    = `2025-10-25_PlantHeight_cm`,
    plantheight_4WAS    = `2025-11-07_PlantHeight_cm`,
    plantheight_6WAS   = `2025-11-21_PlantHeight_cm`,
    
    leaflength_2WAS    = `2025-10-25_LeafLength_cm`,
    leaflength_4WAS    = `2025-11-07_LeafLength_cm`,
    leaflength_6WAS   = `2025-11-21_LeafLength_cm`,
  
    leafbreadth_2WAS    = `2025-10-25_LeafBreadth_cm`,
    leafbreadth_4WAS    = `2025-11-07_LeafBreadth_cm`,
    leafbreadth_6WAS   = `2025-11-21_LeafBreadth_cm`,
    
    leafarea_2WAS    = `2025-10-25_LeafArea_cm2`,
    leafarea_4WAS    = `2025-11-07_LeafArea_cm2`,
    leafarea_6WAS   = `2025-11-21_LeafArea_cm2`,
    
    nol_2WAS    = `2025-10-25_Number_of_Leaves`,
    nol_4WAS    = `2025-11-07_Number_of_Leaves`,
    nol_6WAS   = `2025-11-21_Number_of_Leaves`
  ) %>%
  mutate(
    Variety = recode(as.character(Variety), !!!variety_map),
    Variety = factor(Variety, levels = ordered_names),
    Variety_Type = if_else(Variety %in% opv_varieties, "Open Pollinated", "Hybrid")
  )
# view(physio_prmtr_clean_data)

long_physio_prmtr_clean_data <- physio_prmtr_clean_data %>%
  pivot_longer(
    cols = matches("_(\\d)WAS$"),
    names_to = c("variable", "WAS"),
    names_pattern = "(.*)_(\\d)WAS",
    values_to = "value"
  ) %>% 
  mutate(
    WAS = as.numeric(WAS),
    Period = case_when(
      WAS %in% c(2) ~ "14 DAS",
      WAS %in% c(4) ~ "28 DAS",
      WAS %in% c(6) ~ "42 DAS",
      TRUE ~ NA_character_
    )
  )%>%
  dplyr::select(-contains("Notes", ignore.case = TRUE)) # since there was no notes taken on the field in  this sheet
# view(long_physio_prmtr_clean_data)


physio_summary <- long_physio_prmtr_clean_data %>%
  group_by(variable, Variety, Variety_Type, Period) %>%
  summarise(
    Mean = mean(value, na.rm = TRUE),
    SD   = sd(value, na.rm = TRUE),
    N    = sum(!is.na(value)),
    SE   = SD / sqrt(N),
    .groups = "drop"
  )
physio_summary

run_physio_mixed_analysis <- function(data,
                                      parameter,
                                      variety_group,
                                      title_prefix,
                                      ylab,
                                      ylim_max) {
  
  # 1️⃣ Filter data
  dat <- data %>%
    dplyr::filter(
      variable == parameter,
      Variety_Type == variety_group
    ) %>%
    tidyr::drop_na(value)
  
  # 2️⃣ RAW SUMMARY (Mean + SEM)
  summary_df <- dat %>%
    group_by(Variety, Period) %>%
    summarise(
      mean = mean(value, na.rm = TRUE),
      se   = sd(value, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  # 3️⃣ Mixed model
  mod <- lmer(value ~ Variety * Period + (1 | Plot), data = dat)
  
  # 4️⃣ Estimated marginal means
  emm <- emmeans(mod, ~ Variety | Period)
  
  cld_res <- cld(
    emm,
    Letters = letters,
    adjust = "tukey"
  )
  
  cld_df <- as.data.frame(cld_res) %>%
    dplyr::select(Variety, Period, .group) %>%
    mutate(.group = gsub(" ", "", .group))
  
  # 5️⃣ Merge CLD with raw summary
  plot_df <- left_join(summary_df, cld_df,
                       by = c("Variety", "Period"))
  plt <- ggplot(plot_df,
                aes(x = Variety, y = mean, fill = Variety)) +
    
    geom_col(width = 0.35) +
    
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      width = 0.16,
      linewidth = 0.5
    ) +
    
    geom_text(
      aes(y = mean + se + (0.05 * ylim_max),
          label = .group),
      size = 5
    ) +
    
    facet_wrap(~Period, nrow = 1) +
    
    scale_fill_manual(values = variety_cols) +
    
    coord_cartesian(ylim = c(0, ylim_max)) +
    
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    
    labs(
      title = paste(title_prefix),
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


opv_height <- run_physio_mixed_analysis(
  data = long_physio_prmtr_clean_data,
  parameter = "plantheight",
  variety_group = "Open Pollinated",
  title_prefix = "Plant Height of Open Pollinated Maize Varieties",
  ylab = "Mean Plant Height (cm)",
  ylim_max = 300
)
opv_height$plot
opv_height$cld

hv_height <- run_physio_mixed_analysis(
  data = long_physio_prmtr_clean_data,
  parameter = "plantheight",
  variety_group = "Hybrid",
  title_prefix = "Plant Height of Hybrid Maize Varieties",
  ylab = "Mean Plant Height (cm)",
  ylim_max = 300
)
hv_height$plot
hv_height$cld


opv_leafarea <- run_physio_mixed_analysis(
  data = long_physio_prmtr_clean_data,
  parameter = "leafarea",
  variety_group = "Open Pollinated",
  title_prefix = "Leaf Area of Open Pollinated Maize Varieties",
  ylab = "Mean Leaf Area (cm²)",
  ylim_max = 800
)
opv_leafarea$plot
opv_leafarea$cld

hv_leafarea <- run_physio_mixed_analysis(
  data = long_physio_prmtr_clean_data,
  parameter = "leafarea",
  variety_group = "Hybrid",
  title_prefix = "Leaf Area of Hybrid Maize Varieties",
  ylab = "Mean Leaf Area (cm²)",
  ylim_max = 800
)
hv_leafarea$plot
hv_leafarea$cld

opv_nol <- run_physio_mixed_analysis(
  data = long_physio_prmtr_clean_data,
  parameter = "nol",
  variety_group = "Open Pollinated",
  title_prefix = "Number of Leaves of Open Pollinated Maize Varieties",
  ylab = "Mean Number of Leaves",
  ylim_max = 20
)
opv_nol$plot
opv_nol$cld

hv_nol <- run_physio_mixed_analysis(
  data = long_physio_prmtr_clean_data,
  parameter = "nol",
  variety_group = "Hybrid",
  title_prefix = "Number of Leaves of Hybrid Maize Varieties",
  ylab = "Mean Number of Leaves",
  ylim_max = 20
)
hv_nol$plot
hv_nol$cld


run_physio_type_analysis <- function(data, 
                                     parameter, 
                                     title_prefix, 
                                     ylab, 
                                     ylim_max) {
  
  # 1️⃣ Filter data
  dat <- data %>%
   filter(variable == parameter) %>%
   drop_na(value)
  
  # 2️⃣ Raw summary
  summary_df <- dat %>%
    group_by(Variety_Type, Period) %>%
    summarise(
      mean = mean(value, na.rm = TRUE),
      se   = sd(value, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  # 3️⃣ Mixed model
  mod <- lmer(value ~ Variety_Type * Period + (1 | Plot), data = dat)
  
  # 4️⃣ Estimated marginal means
  emm <- emmeans(mod, ~ Variety_Type | Period)
  
  cld_res <- cld(
    emm,
    Letters = letters,
    adjust = "tukey"
  )
  
  cld_df <- as.data.frame(cld_res) %>%
    dplyr::select(Variety_Type, Period, .group) %>%
    mutate(.group = gsub(" ", "", .group))
  
  # 5️⃣ Merge
  plot_df <- left_join(summary_df, cld_df,
                       by = c("Variety_Type", "Period"))
  
  # 6️⃣ Plot
  plt <- ggplot(plot_df,
                aes(x = Variety_Type, y = mean, fill = Variety_Type)) +
    
    geom_col(width = 0.3) +
    
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      width = 0.1,
      linewidth = 0.6
    ) +
    
    geom_text(
      aes(y = mean + se + (0.05 * ylim_max),
          label = .group),
      size = 5
    ) +
    
    facet_wrap(~Period, nrow = 1) +
    
    scale_fill_manual(
      values = c(
        "Hybrid"  = "#1b9e77",
        "Open Pollinated" = "#d95f02"
      )
    ) +
    coord_cartesian(ylim = c(0, ylim_max)) +
    
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    
    labs(
      title = title_prefix,
      y = ylab,
      x = "Variety Type"
    ) +
    
    theme_classic(base_size = 13, base_family = "Times New Roman") +
    
    theme(
      plot.title = element_text(face = "bold", size = 15),
      strip.text = element_text(face = "bold"),
      legend.position = "None"
    )
  
  return(list(
    data = dat,
    summary = summary_df,
    model = mod,
    cld = cld_res,
    plot = plt
  ))
}

type_height <- run_physio_type_analysis(
  data = long_physio_prmtr_clean_data,
  parameter = "plantheight",
  title_prefix = "Plant Height of Maize Variety Types",
  ylab = "Mean Plant Height (cm)",
  ylim_max = 300
)
type_height$plot
type_height$cld

type_leafarea <- run_physio_type_analysis(
  data = long_physio_prmtr_clean_data,
  parameter = "leafarea",
  title_prefix = "Leaf Area of Maize Variety Types",
  ylab = "Mean Leaf Area (cm²)",
  ylim_max = 800
)
type_leafarea$plot
type_leafarea$cld

type_nol <- run_physio_type_analysis(
  data = long_physio_prmtr_clean_data,
  parameter = "nol",
  title_prefix = "Number of Leaves of Maize Variety Types",
  ylab = "Mean Number of Leaves",
  ylim_max = 20
)
type_nol$plot
type_nol$cld


#     #   #    #     #      #     #     #   


physio_wide <- long_physio_prmtr_clean_data %>%
  mutate(
    Plant_ID = as.character(Plant_ID),
    Variety  = as.character(Variety),
    Period  = case_when(
      WAS == 2 ~ "8-21 DAS",
      WAS == 4 ~ "22-35 DAS",
      WAS == 6 ~ "36-49 DAS",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(Plant_ID, Variety, Period, variable) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = variable, values_from = value)
# Standardize period in Entomology
ent_wide <- period_collapsed %>%
  # Convert IDs and Variety to character
  mutate(
    Plant_ID = as.character(Plant_ID),
    Variety  = as.character(Variety),
    Period   = as.character(Period)  # ensure period is character
  ) %>%
  # Group and summarise
  group_by(Plant_ID, Variety, Period, Parameter) %>%
  summarise(Value_pct = mean(Value_pct, na.rm = TRUE), .groups = "drop") %>%
  # Pivot wider so each Parameter becomes a column
  pivot_wider(names_from = Parameter, values_from = Value_pct)
# Merge Physio + Entomology
combined_data <- physio_wide %>%
  left_join(ent_wide, by = c("Plant_ID", "Variety", "Period"))%>%
  mutate(
    Variety = recode(as.character(Variety), !!!variety_map),
    Variety = factor(Variety, levels = ordered_names),
    Variety_Type = if_else(Variety %in% opv_varieties, "OPV", "HV")
  )
head(combined_data)

corr_data <- combined_data %>%
  dplyr::select(
    plantheight,
    leaflength,
    leafbreadth,
    leafarea,
    nol,
    Damage,
    Larvae,
    Eggs,
    Pred
  )

cor_matrix <- cor(
  corr_data,
  use = "pairwise.complete.obs"
)

cor_matrix

library(corrplot)

# Rename the correlation matrix columns and rownames
colnames(cor_matrix) <- c(
  "Plant Height",
  "Leaf Length",
  "Leaf Breadth",
  "Leaf Area",
  "Number of Leaves",
  "Damage",
  "Larvae Count",
  "Egg Count",
  "Predators"
)
rownames(cor_matrix) <- colnames(cor_matrix)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 90
)