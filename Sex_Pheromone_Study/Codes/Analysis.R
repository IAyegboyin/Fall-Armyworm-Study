# Introduction----
# 01-06-2025 
# Ismail Ayomide Ayegboyin (ayegboyinismai@gmail.com)
# This data was shared by Dr. M. D. Akinbuluma to check for some trends in the data and the work is still in progress .....
# Libraries----
library(tidyverse)
library(agricolae)
library(readxl)
library(MASS)
library(emmeans)
library(performance)
library(multcompView)
library(multcomp)
library(DHARMa)

# gs4_auth() # You need this to authenticate the google sheet

# faw - load data----
faw.daily <- read_excel("/Users/Esmael/Desktop/Data Science Library/Data for play/Fall-Armyworm-control/Data/Fall_Armyworm_Data.xlsx",
                        sheet = "merged_data" )
# view(faw.daily)

faw.weekly <- read_excel("/Users/Esmael/Desktop/Data Science Library/Data for play/Fall-Armyworm-control/Data/Fall_Armyworm_Data.xlsx",
                         sheet = "weekly_data" ) 
# view(faw.weekly)


# faw - data cleaning and preparation ----
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
# view(faw.daily_clean)

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
# view(faw.weekly_clean)

# faw - descriptive statistics starts here ----
weekly_data <- faw.weekly_clean %>%
  pivot_longer(cols = starts_with("Week"), 
               names_to = "Week", 
               values_to = "Count")
faw.summary.stats <- weekly_data %>%
  group_by(Treatment, Week) %>%
  summarise(Mean = mean(Count, na.rm = TRUE), 
            SD = sd(Count, na.rm = TRUE),
            SEM = sd(Count)/sqrt(n()),
            .groups = 'drop')
as.data.frame(faw.summary.stats)
# view(faw.summary.stats)
write.csv(x = faw.summary.stats, file = "/Users/Esmael/Desktop/Data Science Library/Data for play/Fall-Armyworm-control/Data/Fall_Armyworm_summary_stats.csv")

# faw kruskal - wallis test starts here ---- 
faw.kruskal <- faw.weekly_clean %>%
  pivot_longer(cols = starts_with("Week"), 
               names_to = "Week", 
               values_to = "Count") %>%
  mutate(Week = str_replace(Week, "Week_", "Week "),
         Week = factor(Week, levels = paste("Week", 1:6)))
# This was done because the data is not normally distributed (zero inflated data)
faw.kruskal.test <- kruskal(faw.kruskal$Count, faw.kruskal$Treatment, alpha = 0.05)
print(faw.kruskal.test) # This will check for the whole data 

# Function to get Kruskal-Wallis results for each week
get.kruskal.results <- function(week_data) {
  # Perform Kruskal-Wallis test
  kruskal <- with(week_data, kruskal(Count, Treatment, group = TRUE))
  
  # Extracting the means and groups
  results <- data.frame(
    Treatment = rownames(kruskal$means),
    mean = kruskal$means$Count,  # or kruskal$means[,1] if Count isn't the column name
    groups = kruskal$groups[rownames(kruskal$means), "groups"]
  )
  return(results)
}
# Applying the function to get the Kruskal-Wallis results
weekly.kruskal.results <- faw.kruskal %>%
  group_by(Week) %>%
  group_modify(~ get.kruskal.results(.x))

#view(weekly.kruskal.results)


# NON-TARGET INSECTS ANALYSIS STARTS HERE ----
# nt - load data----
other.daily <- read_excel("/Users/Esmael/Desktop/Data Science Library/Data for play/Fall-Armyworm-control/Data/Fall_Armyworm_Data.xlsx",
                          sheet = "other_insects")
# view(other.daily)

other.weekly <- read_excel("/Users/Esmael/Desktop/Data Science Library/Data for play/Fall-Armyworm-control/Data/Fall_Armyworm_Data.xlsx",
                           sheet = "other_insects_weekly") 
# view(other.weekly)


# nt - data cleaning and preparation ----
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
# view(other.daily_clean)

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
# view(other.weekly_clean)

# nt - descriptive statistics starts here ----
other.weekly_data <- other.weekly_clean %>%
  pivot_longer(cols = starts_with("Week"), 
               names_to = "Week", 
               values_to = "Count")
# view(other.weekly_data)
other.summary.stats <- other.weekly_data %>%
  group_by(Treatment, Week) %>%
  summarise(Mean = mean(Count, na.rm = TRUE), 
            SD = sd(Count, na.rm = TRUE),
            SEM = sd(Count)/sqrt(n()),
            .groups = 'drop')
as.data.frame(other.summary.stats)
# view(other.summary.stats)
write.csv(x = faw.summary.stats, file = "/Users/Esmael/Desktop/Data Science Library/Data for play/Fall-Armyworm-control/Data/Other_Insects_summary_stats.csv")

# nt - kruskal-wallis test ----
other.kruskal <- other.weekly_clean %>%
  pivot_longer(cols = starts_with("Week"), 
               names_to = "Week", 
               values_to = "Count") %>%
  mutate(Week = str_replace(Week, "Week_", "Week "),
         Week = factor(Week, levels = paste("Week", 1:6)))

non.target.kruskal <- kruskal(other.kruskal$Count, other.kruskal$Treatment, alpha = 0.05)
print(non.target.kruskal)

# Applying the function to get the Kruskal-Wallis results
non.target.weekly.kruskal.results <- other.kruskal %>%
  group_by(Week) %>%
  group_modify(~ get.kruskal.results(.x))

# view(non.target.weekly.kruskal.results)


# Negative Binomial Modelling ----
# negative binomial model 1 
faw.model <-weekly_data
# view(faw.model)

faw.nb.model.1 <- glm.nb(Count ~ Treatment+Week, data = faw.model) 
summary(faw.nb.model.1)

check_overdispersion(faw.nb.model.1)
emmeans.1 <- emmeans(faw.nb.model.1, pairwise ~ Treatment,
        adjust = "Tukey")
model_performance(faw.nb.model.1)

faw.nb.model.1 <- glm.nb(Count ~ Treatment + Week, data = faw.model)

# Model summary
summary(faw.nb.model.1)

# Check overdispersion
check_overdispersion(faw.nb.model.1)


# Negative binomial 1 
faw.nb.model.1 <- glm.nb(Count ~ Treatment + Week, data = faw.model)
summary(faw.nb.model.1)
check_overdispersion(faw.nb.model.1)
model_performance(faw.nb.model.1)

emmeans.1 <- emmeans(faw.nb.model.1, pairwise ~ Treatment, type = "response")
# compute letters here 
cld.1 <- cld(emmeans.1$emmeans,
             Letters = letters,
             adjust = "sidak")
# View with grouping letters
print(cld.1)


