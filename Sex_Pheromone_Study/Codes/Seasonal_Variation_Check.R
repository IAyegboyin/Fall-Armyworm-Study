# Introduction----
# 03-09-2025 
# Ismail Ayomide Ayegboyin (ayegboyinismai@gmail.com)

# Libraries----
library(tidyverse)
library(readxl)


#library(MASS)
#library(emmeans)
#library(performance)
#ibrary(multcompView)
#library(multcomp) 
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