# Introduction----
# 20-06-2025 
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

# gs4_auth() # You need this to authenticate the google sheet

# faw - load data----
faw.daily_2 <- read_excel("/Users/Esmael/Desktop/Data Science Library/Data for play/Fall-Armyworm-control/Data/Fall_Armyworm_Data_season_2.xlsx",
                        sheet = "faw_season_2")
view(faw.daily_2)

faw.weekly_2 <- read_excel("/Users/Esmael/Desktop/Data Science Library/Data for play/Fall-Armyworm-control/Data/Fall_Armyworm_Data_season_2.xlsx",
                         sheet = "faw_weekly_data_2") 
# view(faw.weekly_2)


# faw - data cleaning and preparation ----
faw.daily_clean_2 <- faw.daily_2 %>%
  mutate (
    Treatment = case_when(
      # Renaming treatments and it cut across this entire scripts as said in script 1
      Treatment %in% c("B1R1", "B1R2", "B1R3") ~ "Blend1",
      Treatment %in% c("B2R1", "B2R2", "B2R3") ~ "Blend2",
      Treatment %in% c("B3R1", "B3R2", "B3R3") ~ "Blend3",
      Treatment %in% c("B4R1", "B4R2", "B4R3") ~ "n-Hexane",
      TRUE ~ Treatment  # keeps original value if no match
    )
)
view(faw.daily_clean_2)

faw.weekly_clean_2 <- faw.weekly_2 %>%
  mutate (
    Treatment = case_when(
      Treatment %in% c("B1R1", "B1R2", "B1R3") ~ "Blend1",
      Treatment %in% c("B2R1", "B2R2", "B2R3") ~ "Blend2",
      Treatment %in% c("B3R1", "B3R2", "B3R3") ~ "Blend3",
      Treatment %in% c("B4R1", "B4R2", "B4R3") ~ "n-Hexane",
      Treatment %in% c("Female") ~ "Female",
      TRUE ~ Treatment  # keeps original value if no match
      )
    )
view(faw.weekly_clean_2)

# faw - descriptive statistics starts here ----
weekly_data_2 <- faw.weekly_clean_2 %>%
  pivot_longer(cols = starts_with("Week"), 
               names_to = "Week", 
               values_to = "Count")
faw.summary.stats_2 <- weekly_data_2 %>%
  group_by(Treatment, Week) %>%
  summarise(Mean = mean(Count, na.rm = TRUE), 
            SD = sd(Count, na.rm = TRUE),
            SEM = sd(Count)/sqrt(n()),
            .groups = 'drop')
as.data.frame(faw.summary.stats_2)
# view(faw.summary.stats_2)
write.csv(x = faw.summary.stats_2, file = "/Users/Esmael/Desktop/Data Science Library/Data for play/Fall-Armyworm-control/Data/Fall_Armyworm_summary_stats_2.csv")

# faw kruskal - wallis test starts here ---- 
faw.kruskal_2 <- faw.weekly_clean_2 %>%
  pivot_longer(cols = starts_with("Week"), 
               names_to = "Week", 
               values_to = "Count") %>%
  mutate(Week = str_replace(Week, "Week_", "Week "),
         Week = factor(Week, levels = paste("Week", 1:4)))


# This was done because the data is not normally distributed (zero inflated data) as the first season data 
faw.kruskal.test_2 <- kruskal(faw.kruskal_2$Count, faw.kruskal_2$Treatment, alpha = 0.05)
print(faw.kruskal.test_2) # This will check for the whole data 

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

get.kruskal.results <- function(week_data) {
  # Perform Kruskal-Wallis test on weekly basis
  kruskal_result <- with(week_data, kruskal(Count, Treatment, group = TRUE))
  
  # Check if grouping was successful
  if (is.null(kruskal_result$groups)) {
    warning("No significant groups found for week ", unique(week_data$Week))
    return(data.frame(Treatment = unique(week_data$Treatment), 
                      mean = NA, 
                      groups = NA))
  }
  
  # Extract means and groups of each treatment
  results <- data.frame(
    Treatment = rownames(kruskal_result$groups),
    mean = kruskal_result$means$Count,  # or kruskal_result$means[,1]
    groups = kruskal_result$groups$groups
  )
  
  return(results)
}

# NON-TARGET INSECTS ANALYSIS STARTS HERE ----
# nt - load data----
other.daily_2 <- read_excel("/Users/Esmael/Desktop/Data Science Library/Data for play/Fall-Armyworm-control/Data/Fall_Armyworm_Data_season_2.xlsx",
                          sheet = "other insect_season2")
# view(other.daily_2)

other.weekly_2 <- read_excel("/Users/Esmael/Desktop/Data Science Library/Data for play/Fall-Armyworm-control/Data/Fall_Armyworm_Data_season_2.xlsx",
                           sheet = "weekly_other_insect_season2") 
# view(other.weekly_2)


# nt - data cleaning and preparation ----
other.daily_clean_2 <- other.daily_2 %>%
  mutate (
    Treatment = case_when(
      Treatment %in% c("B1R1", "B1R2", "B1R3") ~ "Blend1",
      Treatment %in% c("B2R1", "B2R2", "B2R3") ~ "Blend2",
      Treatment %in% c("B3R1", "B3R2", "B3R3") ~ "Blend3",
      Treatment %in% c("B4R1", "B4R2", "B4R3") ~ "n-Hexane",
      TRUE ~ Treatment  # keeps original value if no match
    )
  )
# view(other.daily_clean_2)

other.weekly_clean_2 <- other.weekly_2 %>%
  mutate (
    Treatment = case_when(
      # Renaming treatments
      Treatment %in% c("B1R1", "B1R2", "B1R3") ~ "Blend1",
      Treatment %in% c("B2R1", "B2R2", "B2R3") ~ "Blend2",
      Treatment %in% c("B3R1", "B3R2", "B3R3") ~ "Blend3",
      Treatment %in% c("B4R1", "B4R2", "B4R3") ~ "n-Hexane",
      Treatment %in% c("Female") ~ "Female",
      TRUE ~ Treatment  # keeps original value if no match
    )
  )
# view(other.weekly_clean_2)

# nt - descriptive statistics starts here ----
other.weekly_data_2 <- other.weekly_clean_2 %>%
  pivot_longer(cols = starts_with("Week"), 
               names_to = "Week", 
               values_to = "Count")
other.summary.stats_2 <- other.weekly_data_2 %>%
  group_by(Treatment, Week) %>%
  summarise(Mean = mean(Count, na.rm = TRUE), 
            SD = sd(Count, na.rm = TRUE),
            SEM = sd(Count)/sqrt(n()),
            .groups = 'drop')
as.data.frame(other.summary.stats_2)
# view(other.summary.stats_2)
write.csv(x = other.summary.stats_2, file = "/Users/Esmael/Desktop/Data Science Library/Data for play/Fall-Armyworm-control/Data/Other_Insects_summary_stats_2.csv")

# nt - kruskal-wallis test ----
other.kruskal_2 <- other.weekly_clean_2 %>%
  pivot_longer(cols = starts_with("Week"), 
               names_to = "Week", 
               values_to = "Count") %>%
  mutate(Week = str_replace(Week, "Week_", "Week "),
         Week = factor(Week, levels = paste("Week", 1:4)))

non.target.kruskal_2 <- kruskal(other.kruskal_2$Count, other.kruskal_2$Treatment, alpha = 0.05)
print(non.target.kruskal_2)

# Applying the function to get the Kruskal-Wallis results
non.target.weekly.kruskal.results_2 <- other.kruskal_2 %>%
  group_by(Week) %>%
  group_modify(~ get.kruskal.results(.x))

view(non.target.weekly.kruskal.results_2)



# Negative Binomial Modelling ----
# negative binomial model 2

faw.model_2 <-  read_excel("/Users/Esmael/Desktop/Data Science Library/Data for play/Fall-Armyworm-control/Data/Fall_Armyworm_Data_season_2.xlsx",
                           sheet = "season_2")%>%
  mutate (
    Treatment = case_when(
      # Renaming treatments
      Treatment %in% c("B1R1", "B1R2", "B1R3") ~ "Blend1",
      Treatment %in% c("B2R1", "B2R2", "B2R3") ~ "Blend2",
      Treatment %in% c("B3R1", "B3R2", "B3R3") ~ "Blend3",
      Treatment %in% c("B4R1", "B4R2", "B4R3") ~ "n-Hexane",
      TRUE ~ Treatment  # keeps original value if no match
    )
  ) %>%
  pivot_longer(cols = starts_with("Week"), 
               names_to = "Week", 
               values_to = "Count")
# view(faw.model_2)

faw.nb.model.2 <- glm.nb(Count ~ Treatment+Week, data = faw.model_2) 
# Model summary
summary(faw.nb.model.2)

check_overdispersion(faw.nb.model.2)
emmeans.2 <- emmeans(faw.nb.model.2, pairwise ~ Treatment,
                     adjust = "Tukey",
                     type = "response")
model_performance(faw.nb.model.2)

# compute letters here 
cld.2 <- cld(emmeans.2$emmeans,
             Letters = letters,
             adjust = "sidak")
# View with grouping letters
print(cld.2)