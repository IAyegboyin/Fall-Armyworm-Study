# Introduction----
# 05-09-2025 
# Ismail Ayomide Ayegboyin (ayegboyinismai@gmail.com)
# This gc-ead was sent to me by Dr Akinbuluma to check for different amplitude_mV using mcb as control, 
# and othe wavename for insects, diiferent ~ amplitude/component ~ wavename_dif 

# Libraries----
library(tidyverse)
library(readxl)
# Loading, Cleaning and Manipulation of data -----
setwd("/Users/Esmael/Desktop/Data Science Library/Data for play/Fall-Armyworm-Study/Sex_Pheromone_Study/Data")
gc_ead <- read_excel("GC-EAD data.xlsx")
# view(gc_ead) # view the raw data here and check for it structure using glimpse function

glimpse(gc_ead) # checking structure of the data and converting the necessary variables to their best format

gc_ead <- gc_ead %>%
  mutate(
  population = factor(population),
  component  = factor(component),
  wavename   = factor(ifelse(as.character(wavename) == "mcb", "MCB", "FAW"))
)

# exploratory data analysis starts here ----

# Summarize per group (average amplitude)
summary_data <- gc_ead %>%
  group_by(population, component, wavename) %>%
  summarise(mean_amp = mean(amplitude_mV, na.rm = TRUE),
            n = n(),
            .groups = "drop")

# Boxplot of amplitudes by country and component
ggplot(gc_ead, aes(x = component, y = amplitude_mV, fill = population)) +
  geom_boxplot() +
  facet_wrap(~ wavename, scales = "free_y") +
  theme_bw() +
  labs(# title = "GC-EAD responses per component by country",
       y = "Amplitude (mV)")

plot <- gc_ead %>%
  group_by(population, component, wavename) %>%
  summarise(
    mean_amp = mean(amplitude_mV, na.rm = TRUE),
    sem = sd(amplitude_mV, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# bar chart with error bars
ggplot(plot, aes(x = component, y = mean_amp, fill = population)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_amp - sem, ymax = mean_amp + sem),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  facet_wrap(~ wavename, scales = "free_y") +
  theme_bw(base_size = 13) +
  labs(
   # title = "GC-EAD responses of each component by country",
    x = "Component",
    y = "Mean amplitude (mV)",
    fill = "Country"
  )

ggplot(plot, aes(x = component, y = mean_amp, fill = wavename)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_amp - sem, ymax = mean_amp + sem),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  facet_wrap(~ population) +
  theme_bw(base_size = 13) +
  labs(
    # title = "GC-EAD responses per component across different countries",
    x = "Component",
    y = "Mean amplitude (mV)",
    fill = "Insect type"
  ) + theme_classic()


ggplot(plot, aes(x = wavename, y = mean_amp, fill = component)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_amp - sem, ymax = mean_amp + sem),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  facet_wrap(~ population) +
  theme_bw(base_size = 13) +
  labs(
    # title = "GC-EAD responses per component across populations",
    x = "Wavename",
    y = "Mean amplitude (mV)",
    fill = "Component"
  ) + theme_classic()

# grouped bars per country, facetted by component
ggplot(plot, aes(x = population, y = mean_amp, fill = wavename)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.5) +
  geom_errorbar(
    aes(ymin = mean_amp - sem, ymax = mean_amp + sem),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  facet_wrap(~ component, ncol = 2) +
  theme_bw(base_size = 13) +
  labs(
    # title = "GC-EAD blend responses per component",
    x = "Country",
    y = "Mean amplitude (mV)",
    fill = "Wavename"
  ) +  
theme_classic()


# Mean blend profile for mcb
mcb_profile <- gc_ead %>%
  filter(wavename == "mcb") %>%
  group_by(component, population) %>%
  summarise(mcb_mean = mean(amplitude_mV), .groups = "drop")
# Join to other insects
blend_comparison <- gc_ead %>%
  filter(wavename != "mcb") %>%
  group_by(component, population, wavename) %>%
  summarise(insect_mean = mean(amplitude_mV), .groups = "drop") %>%
  left_join(mcb_profile, by = c("component", "population")) %>%
  mutate(diff = insect_mean - mcb_mean)

blend_comparison



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