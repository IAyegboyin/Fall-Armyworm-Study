# This script is for analysis of variance of the data both in overall and weekly manners 
# 15 - 06 - 2025
#Library required ----
library(tidyverse)
library(agricolae)

# Data preparation ---- 
other.anova <- other.weekly_data %>%
  mutate(Week = str_replace(Week, "Week_", "Week "),
         Week = factor(Week, levels = paste("Week", 1:6)))

# Anova and HSD.test is here ----
other.anova.m <- aov(Count ~ Treatment, data = other.anova)
summary(other.anova.m)
other.anova.m.hsd <- HSD.test(other.anova.m, "Treatment", group = TRUE, unbalanced = TRUE)
view(other.anova.m.hsd$groups)


# Function to get HSD results for each week
get.others.hsd.results <- function(week_data) {
  model <- aov(Count ~ Treatment, data = week_data)
  hsd <- HSD.test(model, "Treatment", group = TRUE)
  
  # Extracting the means and groups correctly for each week
  other.results <- data.frame(
    Treatment = rownames(hsd$means),
    mean = hsd$means$Count,  # or hsd$means[,1] if Count isn't the column name
    groups = hsd$groups[rownames(hsd$means), "groups"]
  )
  return(other.results)
}

# Applying the function here to get the anova
other.weekly.results <- other.anova %>%
  group_by(Week) %>%
  group_modify(~ get.hsd.results(.x))
view(other.weekly.results)

