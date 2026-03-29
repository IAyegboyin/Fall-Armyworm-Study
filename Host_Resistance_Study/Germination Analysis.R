# Germination Analysis ----

germination <- read_sheet(
  "https://docs.google.com/spreadsheets/d/14KYjkx49VFTbC8AI7G0ev2uZNpbIKD3OwCbWAQxlWco/edit?gid=0#gid=0",
  sheet = "Germination_DAP10"
)

germi <- germination %>%
  rename(
    Emergence_14 = `Emergence)_14_10_2025_(0/2)`,
    Emergence_20 = `Emergence)_20_10_2025_(0/2)`
  ) %>%
  mutate(
    Variety = factor(Variety, levels = paste0("V", 1:11)),
    Plot = factor(Plot),
    Row = factor(Row),
    Column = as.numeric(Column),
    early_prop = Emergence_14 / 2,
    final_prop = Emergence_20 / 2
  ) %>%
  mutate(
    Variety = recode(as.character(Variety), !!!variety_map),
    Variety = factor(Variety, levels = ordered_names),
    Variety_Type = if_else(Variety %in% opv_varieties, "Open Pollinated", "Hybrid")
  )

variety_names <- c(
  "V1" = "Sammaz 15", "V2" = "Oba Super 4", "V3" = "Sammaz 52",
  "V4" = "Sammaz 60", "V5" = "SC 719", "V6" = "SC 649",
  "V7" = "Sammaz 66", "V8" = "Sammaz 59", "V9" = "Oba Super 6",
  "V10" = "Oba Super 15", "V11" = "Sammaz 51"
)
germi_final <- germi %>%
  mutate(Germination = final_prop)

run_germination_type_model <- function(data, type_label){
  dat <- data %>%
    filter(Variety_Type == type_label)
  # Summary statistics
  summary_df <- dat %>%
    group_by(Variety) %>%
    summarise(
      mean = mean(Germination, na.rm = TRUE),
      se = sd(Germination, na.rm = TRUE)/sqrt(n()),
      .groups = "drop"
    )
  # Mixed model
  mod <- glmer(
    Germination ~ Variety + (1|Plot),
    family = binomial,
    data = dat
  )
  # Estimated marginal means
  emm <- emmeans(mod, ~ Variety, type = "response")
  cld_res <- cld(
    emm,
    Letters = letters,
    adjust = "tukey"
  )
  cld_df <- as.data.frame(cld_res) %>%
    dplyr::select(Variety, .group) %>%
    mutate(.group = gsub(" ","",.group))
  plot_df <- left_join(summary_df, cld_df, by="Variety")
  
  # Plot
  plt <- ggplot(
    plot_df,
    aes(x = Variety, y = mean, fill = Variety)
  ) +
    geom_col(width = 0.30) +
    geom_errorbar(
      aes(ymin = mean - se,
          ymax = mean + se),
      width = 0.15
    ) +
    geom_text(
      aes(y = mean + se + 0.05,
          label = .group),
      size = 5
    ) +
    scale_fill_manual(values = variety_cols) +
    coord_cartesian(ylim=c(0,1)) +
    labs(
      title = paste("Germination Success of", type_label, "Maize Varieties"),
      y = "Germination proportion",
      x = "Maize Variety"
    ) +
    theme_classic(base_family="Times New Roman") +
    theme(
      axis.text.x = element_text(angle = 90, hjust=1),
      legend.position="none",
      plot.title = element_text(face="bold")
    )
  return(list(
    data = dat,
    summary = summary_df,
    model = mod,
    cld = cld_res,
    plot = plt
  ))
}

opv_germination <- run_germination_type_model(
  data = germi_final,
  type_label = "Open Pollinated"
)
opv_germination$plot
opv_germination$cld

hv_germination <- run_germination_type_model(
  data = germi_final,
  type_label = "Hybrid"
)
hv_germination$plot
hv_germination$cld

run_germination_variety_type_model <- function(data){
  dat <- data
  # Summary statistics
  summary_df <- dat %>%
    group_by(Variety_Type) %>%
    summarise(
      mean = mean(Germination, na.rm = TRUE),
      se = sd(Germination, na.rm = TRUE)/sqrt(n()),
      .groups = "drop"
    )
  # Correct binomial model
  mod <- glm(
    cbind(Emergence_20, 2 - Emergence_20) ~ Variety_Type,
    family = binomial,
    data = dat
  )
  # Estimated marginal means
  emm <- emmeans(mod, ~ Variety_Type, type = "response")
  cld_res <- cld(
    emm,
    Letters = letters
  )
  cld_df <- as.data.frame(cld_res) %>%
    dplyr::select(Variety_Type, .group) %>%
    mutate(.group = gsub(" ","",.group))
  plot_df <- left_join(summary_df, cld_df, by="Variety_Type")
  # Plot
  plt <- ggplot(
    plot_df,
    aes(x = Variety_Type, y = mean, fill = Variety_Type)
  ) +
    geom_col(width = 0.20) +
    geom_errorbar(
      aes(ymin = mean - se,
          ymax = mean + se),
      width = 0.08, linewidth = 0.6
    ) +
    geom_text(
      aes(y = mean + se + 0.05,
          label = .group),
      size = 5
    ) +
    scale_fill_manual(values = c(
      "Open Pollinated" = "#d95f02",
      "Hybrid"  = "#66a61e"
    )) +
    coord_cartesian(ylim = c(0,1)) +
    labs(
      title = "Germination Comparison of Maize Variety Types",
      y = "Mean Germination Proportion",
      x = "Variety Type"
    ) +
    theme_classic(base_family = "Times New Roman") +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold")
    )
  return(list(
    data = dat,
    summary = summary_df,
    model = mod,
    cld = cld_res,
    plot = plt
  ))
}

type_germination <- run_germination_variety_type_model(
  data = germi_final
  )
type_germination$plot
type_germination$cld