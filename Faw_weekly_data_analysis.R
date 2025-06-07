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
faw.daily <- read_excel("/Users/user/Desktop/Data Science Library/Data for play/Fall-Armyworm-control--/Data/Fall_Armyworm_Data.xlsx",
                     sheet = "merged_data" )
view(faw.daily)

faw.weekly <- read_excel("/Users/user/Desktop/Data Science Library/Data for play/Fall-Armyworm-control--/Data/Fall_Armyworm_Data.xlsx",
                  sheet = "weekly_data" ) 
  view(faw.weekly)


# Data Cleaning and Preparation ----
faw.daily_clean <- faw.daily %>%
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
view(faw.daily_clean)

faw.weekly_clean <- faw.weekly %>%
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
view(faw.weekly_clean)

# Descriptive Statistics starts here ----
faw.summary.stats <- weekly_data %>%
  group_by(Treatment, Week) %>%
  summarise(Mean = mean(Count, na.rm = TRUE), 
            SD = sd(Count, na.rm = TRUE),
            SEM = sd(Count)/sqrt(n()),
            .groups = 'drop')
as.data.frame(faw.summary.stats)
view(faw.summary.stats)
write.csv(x = faw.summary.stats, file = "/Users/user/Desktop/Data Science Library/Data for play/Fall-Armyworm-control--/Data/Fall_Armyworm_summary_stats.csv")


faw.anova <- weekly_data %>%
  mutate(Blend = str_extract(Treatment, "R\\d") %>% replace_na("Control"))

# Convert Week columns to numeric type in case they're not
week_cols <- grep("Week", names(weekly_data), value = TRUE)
weekly_data[week_cols] <- lapply(weekly_data[week_cols], as.numeric)

# Create a total weekly count column
weekly_data <- weekly_data %>%
  mutate(Total = rowSums(across(all_of(week_cols)), na.rm = TRUE))

# One-way ANOVA to test effect of Blend on Total moth count
faw.anova.model <- aov(Total ~ Treatment, data = weekly_data)

# View summary
summary(faw.anova.model)

faw.hsd <- HSD.test(faw.anova.model, "Treatment", group = TRUE)

view(faw.hsd$groups)

# Test for normality of data 
qqnorm(farm_1$Count, main = "Normal Q-Q Plot for Farm 1")
qqline(farm_1$Count)  # Inference: Data is zero inflated 

qqnorm(farm_2$Count, main = "Normal Q-Q Plot for Farm 2")
qqline(farm_2$Count) # Inference: Data is zero inflated 

#shapiro-wilk test
shapiro.test(farm_1$Count)
shapiro.test(farm_2$Count)

#CONCLUSION: This data is not normally distributed ...



