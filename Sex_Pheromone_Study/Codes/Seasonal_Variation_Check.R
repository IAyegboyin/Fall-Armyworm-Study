# Introduction----
# 03-09-2025 
# Ismail Ayomide Ayegboyin (ayegboyinismai@gmail.com)

# Libraries----
library(tidyverse)
library(readxl)
library(performance) # to check performance of the models
library(MASS)
library(emmeans)
library(multcompView)
library(multcomp) 
library(lme4)

# Loading, Cleaning and Manipulation of data -----
setwd("/Users/Esmael/Desktop/Data Science Library/Data for play/Fall-Armyworm-Study/Sex_Pheromone_Study/Data")

faw.season1 <- read_excel("Fall_Armyworm_Data.xlsx",
              sheet = "weekly_data")%>%
  mutate (
    Treatment = case_when(
      # Renaming treatments
      Treatment %in% c("B1R1", "B1R2") ~ "Blend1",
      Treatment %in% c("B2R1", "B2R2") ~ "Blend2",
      Treatment %in% c("B3R1", "B3R2") ~ "Blend3",
      Treatment == "B4" ~ "n-Hexane",
      TRUE ~ Treatment  # keeps original value if no match
    ), 
    season = "season 1"
  ) %>%
  pivot_longer(cols = starts_with("Week"), 
               names_to = "Week", 
               values_to = "Count")

# view(faw.season1)

faw.season2 <-  read_excel("Fall_Armyworm_Data_season_2.xlsx",
                           sheet = "season_2")%>%
  mutate (
    Treatment = case_when(
      # Renaming treatments
      Treatment %in% c("B1R1", "B1R2", "B1R3") ~ "Blend1",
      Treatment %in% c("B2R1", "B2R2", "B2R3") ~ "Blend2",
      Treatment %in% c("B3R1", "B3R2", "B3R3") ~ "Blend3",
      Treatment %in% c("B4R1", "B4R2", "B4R3") ~ "n-Hexane",
      TRUE ~ Treatment  # keeps original value if no match
    ), 
    season = "season 2"
  )%>%
  pivot_longer(cols = starts_with("Week"), 
               names_to = "Week", 
               values_to = "Count")
# view(faw.season2)

# Seasonality check starts here ----
faw.season <- bind_rows(faw.season1, faw.season2)
# view(faw.season)

# Using poison model to check for seasonal variation 
model.poison <- glm(Count ~ season * Treatment, data = faw.season, family = poisson)
summary(model.poison)
check_overdispersion(model.poison) #over dispersion check for poison model
check_homogeneity(model.poison) # homogeniety check for poison model 
check_zeroinflation(model.poison) 
model_performance(model.poison) # using this to check for the posion model's AIC 

#Using quasipoison model to check for seasonal variation

model.quasipoison <- glm(Count ~ season * Treatment, data = faw.season, 
                        family =  quasipoisson(link = "identity"))
summary(model.quasipoison)
check_overdispersion(model.quasipoison)
check_homogeneity(model.quasipoison)
check_zeroinflation(model.quasipoison)
model_performance(model.quasipoison) # using this to check for the quasipoison model's AIC


# Using negative binomial model to check for season variation
model.nb <- glm.nb(Count ~ season + Treatment, data = faw.season)
summary(model.nb)
check_overdispersion(model.nb)
check_homogeneity(model.nb)
check_zeroinflation(model.nb)
model_performance(model.nb) # using this to check for the negative binomial model AIC majorly


# Checking AIC for the models for comparison 
AIC(model.nb, model.poison)

# Using negative binomial model with mixed effect to check for season variation
# Fixed effect = season * Treatment, Random effect = Week

model.glmer <- glmer.nb(Count ~ season * Treatment + (1|Week),
                        data = faw.season)
summary(model.glmer)
check_overdispersion(model.glmer)
check_homogeneity(model.glmer)
check_zeroinflation(model.glmer)
model_performance(model.glmer)


# checking for the best model GLM or GLMM here 
AIC(model.glmer, model.nb)

# Since glmm negative binomial works best for the data, i.e. lower AIC
emmeans.model.glmer <- emmeans(model.glmer, pairwise ~ Treatment,
        adjust = "Tukey",
        type = "response")
emmeans.model.glmer
 
# Compact Letter Display
cld.model.glmer <- cld(emmeans.model.glmer$emmeans,
                 Letters = letters,
                 adjust = "tukey")
cld.model.glmer

emmeans.model.glmer.season <- emmeans(model.glmer, pairwise ~ season, 
                                      adjust = "tukey",
                                      type = "response")

# Checking for which season where the trap count is higher for all the blends
cld.model.glmer.season <- cld(emmeans.model.glmer.season$emmeans,
                       Letters = letters,
                       adjust = "tukey")
cld.model.glmer.season
