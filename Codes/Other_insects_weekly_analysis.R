# Introduction----
# 01-06-2025 
# Ismail Ayomide Ayegboyin (ayegboyinismai@gmail.com)
# This data was shared by Dr. M. D. Akinbuluma to check for some trends in the data and the work is still in progress .....
# Libraries----
library(tidyverse)
library(agricolae)
library(readxl)

# gs4_auth() # You need this to authenticate the google sheet

# Load Data----
other.daily <- read_excel("/Users/Esmael/Desktop/Data Science Library/Data for play/Fall-Armyworm-control/Data/Fall_Armyworm_Data.xlsx",
                        sheet = "other_insects")
view(other.daily)

other.weekly <- read_excel("/Users/Esmael/Desktop/Data Science Library/Data for play/Fall-Armyworm-control/Data/Fall_Armyworm_Data.xlsx",
                         sheet = "other_insects_weekly") 
view(other.weekly)


# Data Cleaning and Preparation ----
other.daily_clean <- other.daily %>%
  mutate (
    Treatment = case_when(
      # Renaming treatments
      Treatment %in% c("B1R1", "B1R2") ~ "Blend1",
      Treatment %in% c("B2R1", "B2R2") ~ "Blend2",
      Treatment %in% c("B3R1", "B3R2") ~ "Blend3",
      Treatment == "B4" ~ "Blend4",
      TRUE ~ Treatment  # keeps original value if no match
    )
  )
view(other.daily_clean)

other.weekly_clean <- other.weekly %>%
  mutate (
    Treatment = case_when(
      # Renaming treatments
      Treatment %in% c("B1R1", "B1R2") ~ "Blend1",
      Treatment %in% c("B2R1", "B2R2") ~ "Blend2",
      Treatment %in% c("B3R1", "B3R2") ~ "Blend3",
      Treatment == "B4" ~ "Blend4",
      TRUE ~ Treatment  # keeps original value if no match
    )
  )
view(other.weekly_clean)

# Descriptive Statistics starts here ----
other.weekly_data <- other.weekly_clean %>%
  pivot_longer(cols = starts_with("Week"), 
               names_to = "Week", 
               values_to = "Count")
other.summary.stats <- other.weekly_data %>%
  group_by(Treatment, Week) %>%
  summarise(Mean = mean(Count, na.rm = TRUE), 
            SD = sd(Count, na.rm = TRUE),
            SEM = sd(Count)/sqrt(n()),
            .groups = 'drop')
as.data.frame(other.summary.stats)
view(other.summary.stats)
write.csv(x = faw.summary.stats, file = "/Users/Esmael/Desktop/Data Science Library/Data for play/Fall-Armyworm-control/Data/Other_Insects_summary_stats.csv")
