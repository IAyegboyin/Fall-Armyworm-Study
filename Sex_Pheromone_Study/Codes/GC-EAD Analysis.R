# Introduction----
# 05-09-2025 
# Ismail Ayomide Ayegboyin (ayegboyinismai@gmail.com)
# This gc-ead was sent to me by Dr Akinbuluma to check for different amplitude_mV using mcb as control, 
# and othe wavename for insects, diiferent ~ amplitude/component ~ wavename_dif 

# Libraries----
library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
# Loading, Cleaning and Manipulation of data -----
setwd("/Users/Esmael/Desktop/Data Science Library/Data for play/Fall-Armyworm-Study/Sex_Pheromone_Study/Data")
gc_ead <- read_excel("GC-EAD data.xlsx")
# view(gc_ead) - # view the raw data here and check for it structure using glimpse function

glimpse(gc_ead) # checking structure of the data and converting the necessary variables to their best format

gc_ead <- gc_ead %>%
  mutate(
  population = factor(population),
  component  = factor(component),
  wavename = as.character(wavename)
) %>%
  mutate(
    component = recode(component,
                       "Z-9-14" = "Z9-14:OAC",
                       "E-Z-7-12" = "E/Z7-12:OAC",
                       "Z-9-12" = "Z9-12:OAC",
                       "Z-11-16" = "Z11-16:OAC"
    ),
    component = factor(component, 
                       levels = c("Z9-14:OAC", "E/Z7-12:OAC", "Z9-12:OAC", "Z11-16:OAC"))
  )


# exploratory data analysis starts here ----

# Summarize per group (average amplitude)
gc_ead.summary_data <- gc_ead %>%
  group_by(population, component, wavename) %>%
  summarise(mean_amp = mean(amplitude_mV, na.rm = TRUE),
            sem = sd(amplitude_mV, na.rm = TRUE) / sqrt(n()),
            n = n(),
            .groups = "drop")
view(gc_ead.summary_data)
write_csv(gc_ead.summary_data, 
          "/Users/Esmael/Desktop/Data Science Library/Data for play/Fall-Armyworm-Study/Sex_Pheromone_Study/Data/gc_ead.summary_data.csv")

# Small wrangling here 
gc_ead.s <- gc_ead.summary_data
gc_ead.faw <- gc_ead.s %>% filter(wavename == "FAW") %>% mutate(
  population = factor(population,
                      levels = c("Benin", "Kenya", "Nigeria"))
)
gc_ead.mcb <- gc_ead.s %>% filter(wavename == "MCB") %>% mutate(
  population = factor(population,
                      levels = c("Benin", "Kenya", "Nigeria")))


# GLMM starts here ----
gc_ead.model <- lmer(
  amplitude_mV ~ population * component + (1 | insect_no),
  data = gc_ead
) # unequal n handled with this model
summary(gc_ead.model)
AIC(gc_ead.model) 
# best model so far -- AIC (18.8321)
emmeans(gc_ead.model, ~ population | component)


# GLMM starts for FAW | Components here
faw_model <- gc_ead %>% filter(wavename == "FAW") %>% mutate (
  population = factor(population,
                      levels = c("Benin", "Kenya", "Nigeria")))
gc_ead.modelfaw <- lmer(
  amplitude_mV ~ population * component + (1 | insect_no),
  data = faw_model
) # unequal n handled with this model
summary(gc_ead.modelfaw)
AIC(gc_ead.modelfaw)
# best model so far -- AIC (2.911977)
emm_faw <- emmeans(gc_ead.modelfaw, ~ population | component)
faw_cld <- cld(
  emm_faw,
  by = "component",
  adjust = "tukey",
  Letters = letters
)
faw_cld

# GLMM starts for MCB | Components here
mcb_model <- gc_ead %>% filter(wavename == "MCB") %>% mutate (
  population = factor(population,
                      levels = c("Benin", "Kenya", "Nigeria")))
gc_ead.modelmcb <- lmer(
  amplitude_mV ~ population * component + (1 | insect_no),
  data = mcb_model
) # unequal n handled with this model
summary(gc_ead.modelmcb)
AIC(gc_ead.modelmcb)
# best model so far -- AIC (35.22165)
emm_mcb <- emmeans(gc_ead.modelmcb, ~ population | component)
emm_cld <- cld(
  emm_mcb,
  by = "component",
  adjust = "tukey",
  Letters = letters
)
emm_cld

# Preparing my clds for plots 
faw_letters <- as.data.frame(faw_cld) %>%
  dplyr::select(component, population, .group) %>%
  mutate(
    .group = gsub(" ", "", .group)  # remove spaces in letters
  )
mcb_letters <- as.data.frame(emm_cld) %>%
  dplyr::select(component, population, .group) %>%
  mutate(
    .group = gsub(" ", "", .group)
  )

# Updating the gc_ead data here with the cld letters 
gc_ead.faw <- gc_ead.faw %>%
  left_join(
    faw_letters,
    by = c("component", "population")
  )
gc_ead.mcb <- gc_ead.mcb %>%
  left_join(
    mcb_letters,
    by = c("component", "population")
  )
# head(gc_ead.mcb)
# visualization ----
# Define custom country palette (Benin = Yellow, Kenya = Brown/Red, Nigeria = Green)
palette <- c(
  "Benin"   = "#FCD116",  # Yellow (Benin flag)
  "Kenya"   = "#A52A2A",  # Brown-red (Kenya flag tone)
  "Nigeria" = "#008751"   # Green (Nigeria flag)
)

# 🟨 Plot 1: Gland Extracts
ggplot(gc_ead.faw, aes(x = population, y = mean_amp, fill = population)) +
  geom_errorbar(
    aes(ymin = mean_amp - sem, ymax = mean_amp + sem),
    width = 0.2
  ) +
  geom_col(width = 0.5, color = "black") +
  
  geom_text(
    aes(label = .group, y = mean_amp  + 0.15),
    size = 6,
    fontface = "bold"
  ) +
  
  facet_wrap(~ component, ncol = 2) +
  theme_classic(base_size = 13, base_family = "Times New Roman") +
  labs(
    title = "Gland Extracts",
    x = NULL,
    y = "Mean amplitude (mV)"
  ) +
  scale_fill_manual(values = palette) +
  scale_x_discrete(labels = c(
    "Benin" = "BENIN",
    "Kenya" = "KENYA",
    "Nigeria" = "NIGERIA"
  )) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 18),
    axis.text = element_text(face = "bold", size = 16), 
    axis.title = element_text(face = "bold", size = 16),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
  )

# 🟩 Plot 2: Multicomponent Blends
ggplot(gc_ead.mcb, aes(x = population, y = mean_amp, fill = population)) +
  geom_errorbar(
    aes(ymin = mean_amp - sem, ymax = mean_amp + sem),
    width = 0.2
  ) +
  geom_col(width = 0.5, color = "black") +
  
  geom_text(
    aes(label = .group, y = mean_amp + sem + 0.05),
    size = 6,
    fontface = "bold"
  ) +
  
  facet_wrap(~ component, ncol = 2) +
  theme_classic(base_size = 13, base_family = "Times New Roman") +
  labs(
    title = "MCB Extracts",
    x = NULL,
    y = "Mean amplitude (mV)"
  ) +
  scale_fill_manual(values = palette) +
  scale_x_discrete(labels = c(
    "Benin" = "BENIN",
    "Kenya" = "KENYA",
    "Nigeria" = "NIGERIA"
  )) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 18),
    axis.text = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 16),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
  )

# The end of the analysis and vizs, check other r files for dif viz -----