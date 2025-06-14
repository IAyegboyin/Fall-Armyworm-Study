# This script is for analysis of variance of the data both in overall and weekly manners 
#Library required ----
library(tidyverse)
library(agricolae)

# Data preparation ---- 
faw.anova <- faw.weekly_clean %>%
  pivot_longer(cols = starts_with("Week"), 
               names_to = "Week", 
               values_to = "Count") %>%
  mutate(Week = str_replace(Week, "Week_", "Week "),
         Week = factor(Week, levels = paste("Week", 1:6)))

# Anova and HSD.test is here ----
# Function to get HSD results for each week
get.hsd.results <- function(week_data) {
  model <- aov(Count ~ Treatment, data = week_data)
  hsd <- HSD.test(model, "Treatment", group = TRUE)
  
  # Extracting the means and groups correctly for each week
  results <- data.frame(
    Treatment = rownames(hsd$means),
    mean = hsd$means$Count,  # or hsd$means[,1] if Count isn't the column name
    groups = hsd$groups[rownames(hsd$means), "groups"]
  )
  return(results)
}
# Applying the function here to get 
weekly.results <- faw.anova %>%
  group_by(Week) %>%
  group_modify(~ get.hsd.results(.x))
view(weekly.results)
# Create the anova and HSDtet plot here now
ggplot(weekly.results, aes(x = Week, y = mean, fill = Treatment)) +
  geom_col(position = position_dodge(0.9), width = 0.8) +
  geom_text(aes(label = groups, y = mean + max(mean)*0.05), 
            position = position_dodge(0.9), size = 4) +
  labs(x = "Week", y = "FAW Count", title = "WEEKLY ANOVA") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()
