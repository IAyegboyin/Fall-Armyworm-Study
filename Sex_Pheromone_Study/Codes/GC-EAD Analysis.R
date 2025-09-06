# Introduction----
# 05-09-2025 
# Ismail Ayomide Ayegboyin (ayegboyinismai@gmail.com)
# This gc-ead was sent to me by Dr Akinbuluma to check for different amplitude_mV using mcb as control, 
# and othe wavename for insects, diiferent ~ amplitude/component ~ wavename_dif 

# Libraries----
library(tidyverse)
library(readxl)
library(factoextra)
library(FactoMineR)
# library(performance) # to check performance of the models
# library(MASS)
# library(emmeans)
# library(multcompView)
# library(multcomp) 
# library(lme4)

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

view(summary_data)

# Boxplot of amplitudes by country and component
ggplot(gc_ead, aes(x = component, y = amplitude_mV, fill = population)) +
  geom_boxplot() +
  facet_wrap(~ wavename, scales = "free_y") +
  theme_bw() +
  labs(title = "GC-EAD responses per component by country",
       y = "Amplitude (mV)")

plot_dat2 <- gc_ead %>%
  group_by(population, component, wavename) %>%
  summarise(
    mean_amp = mean(amplitude_mV, na.rm = TRUE),
    sem = sd(amplitude_mV, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# bar chart with error bars
ggplot(plot_dat2, aes(x = component, y = mean_amp, fill = population)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_amp - sem, ymax = mean_amp + sem),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  facet_wrap(~ wavename, scales = "free_y") +
  theme_bw(base_size = 13) +
  labs(
    title = "GC-EAD responses per component by country",
    x = "Component",
    y = "Mean amplitude (mV)",
    fill = "Country"
  )

####################################################
plot_dat <- gc_ead %>%
  group_by(population, component, wavename) %>%
  summarise(
    mean_amp = mean(amplitude_mV, na.rm = TRUE),
    sem = sd(amplitude_mV, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

ggplot(plot_dat, aes(x = component, y = mean_amp, fill = wavename)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_amp - sem, ymax = mean_amp + sem),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  facet_wrap(~ population) +
  theme_bw(base_size = 13) +
  labs(
    title = "GC-EAD responses per component across populations",
    x = "Component",
    y = "Mean amplitude (mV)",
    fill = "Insect type"
  )


ggplot(plot_dat, aes(x = wavename, y = mean_amp, fill = component)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_amp - sem, ymax = mean_amp + sem),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  facet_wrap(~ population) +
  theme_bw(base_size = 13) +
  labs(
    title = "GC-EAD responses per component across populations",
    x = "Wavename",
    y = "Mean amplitude (mV)",
    fill = "Component"
  )

# summarise means + SEM
plot_dat4 <- gc_ead %>%
  group_by(population, wavename, component) %>%
  summarise(
    mean_amp = mean(amplitude_mV, na.rm = TRUE),
    sem = sd(amplitude_mV, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# grouped bars per country, facetted by component
ggplot(plot_dat4, aes(x = population, y = mean_amp, fill = wavename)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.5) +
  geom_errorbar(
    aes(ymin = mean_amp - sem, ymax = mean_amp + sem),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  facet_wrap(~ component, ncol = 2) +
  theme_bw(base_size = 13) +
  labs(
    title = "GC-EAD blend responses per component",
    x = "Country",
    y = "Mean amplitude (mV)",
    fill = "Wavename"
  ) +  
theme(axis.text.x = element_text(angle = 45, hjust = 1))



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

print(blend_comparison)


gc_ead_pca <- gc_ead %>%
  mutate(wavename = ifelse(wavename == "mcb", "mcb", "faw")) %>%
  group_by(population, wavename, component) %>%
  summarise(mean_amp = mean(amplitude_mV, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = component, values_from = mean_amp, values_fill = 0)
meta <- gc_ead_pca %>% select(population, wavename)
gc_ead_pca <- gc_ead_pca %>% select(-population, -wavename)


pca_res <- PCA(gc_ead_pca, graph = FALSE)
ca_res <- CA(gc_ead_pca, graph = FALSE)

eig.val_gc_ead <- get_eigenvalue(pca_res)
eig.val_gc_ead

fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 50)) # scree plot

fviz_pca_var(pca_res, col.var = "black", repel = TRUE)

fviz_pca_var(pca_res, col.var = "cos2",
             repel = TRUE # Avoid text overlapping
)

fviz_pca_ind(pca_res, col.ind = "cos2", pointsize = "cos2",
             pointshape = 21, fill = "#E7B800", 
             repel = TRUE) # Avoid text overlapping (slow if many points)

fviz_pca_var(pca_res, col.var = "contrib",
             gradient.cols = c("red", "#E7B800", "darkgreen", "blue"),
             repel = TRUE # Avoid text overlapping
)

fviz_contrib(pca_res, choice = "var", axes = 1)
fviz_contrib(pca_res, choice = "var", axes = 2)












fviz_pca_ind(pca_res,
             geom.ind = "point",
             habillage = meta$wavename,
             addEllipses = TRUE,
             palette = c("#00AFBB", "#E7B800"),
             shape = meta$population,
             repel = TRUE)

fviz_pca_var(pca_res, repel = TRUE)


fviz_ca_ind(ca_res,
            habillage = gc_ea$wavename,
            addEllipses = TRUE,
            repel = TRUE)

fviz_ca_var(ca_res, repel = TRUE)
