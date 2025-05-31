# Introduction----
# 28-05-2025 
# Ismail Ayomide Ayegboyin (ayegboyinismai@gmail.com)
# This data was shared by Dr. M. D. Akinbuluma to check for some trends in the data and the work is still in progress .....

# Libraries----
library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(performance)
library(multcompView)

# gs4_auth() # You need this to authenticate the google sheet

# Load Data----
farm_1 <- read_excel("/Users/user/Desktop/Data Science Library/Data for play/Fall-Armyworm-control--/Data/Fall_Armyworm_Data.xlsx",
                      sheet = "AA" )    %>% 
  view(farm_1)

farm_2 <- read_excel("/Users/user/Desktop/Data Science Library/Data for play/Fall-Armyworm-control--/Data/Fall_Armyworm_Data.xlsx",
                     sheet = "BB" )    %>% 
  view(farm_2)

# Data Cleaning and Preparation ----
farm_1 <- farm_1 %>%
  pivot_longer(cols = starts_with("Day"), 
               names_to = "Day", 
               values_to = "Count") %>%
  mutate(Day = as.numeric(str_remove(Day, "Day ")))

farm_2 <- farm_2 %>%
  pivot_longer(cols = starts_with("Day"), 
               names_to = "Day", 
               values_to = "Count") %>%
  mutate(Day = as.numeric(str_remove(Day, "Day ")))


# Extract Blend (Treatment group) and Replicate (if any)
farm_1 <- farm_1 %>%
  mutate(
    Blend = str_extract(Treatment, "B\\d"),
    Replicate = str_extract(Treatment, "R\\d")
  )
farm_2 <- farm_2 %>%
  mutate(
    Blend = str_extract(Treatment, "B\\d"),
    Replicate = str_extract(Treatment, "R\\d")
  )

# Replace NA in Replicate with "R0" for control or single reps
farm_1$Replicate[is.na(farm_1$Replicate)] <- "R0"
farm_2$Replicate[is.na(farm_2$Replicate)] <- "R0"


# Exploratory Data Analysis Starts here ----

# Test for normality of data 
qqnorm(farm_1$Count, main = "Normal Q-Q Plot for Farm 1")
qqline(farm_1$Count)  # Inference: Data is zero inflated 

qqnorm(farm_2$Count, main = "Normal Q-Q Plot for Farm 2")
qqline(farm_2$Count) # Inference: Data is zero inflated 

#shapiro-wilk test
shapiro.test(farm_1$Count)
shapiro.test(farm_2$Count)

#CONCLUSION: This data is not normally distributed ....

# Descriptive Statistics 
farm_1.summary_stats <- farm_1 %>%
  group_by(Blend, Day) %>%
  summarise(Mean = mean(Count, na.rm = TRUE), 
            SD = sd(Count, na.rm = TRUE),
            .groups = 'drop')

print(farm_1.summary_stats)
view(farm_1.summary_stats)

farm_2.summary_stats <- farm_2 %>%
  group_by(Blend, Day) %>%
  summarise(Mean = mean(Count, na.rm = TRUE), 
            SD = sd(Count, na.rm = TRUE),
            .groups = 'drop')

print(farm_2.summary_stats)
view(farm_2.summary_stats)


# Modelling of the data (Farm 1 Starts here)----
# linear model ....
lm_farm_1 <- lm(Count ~ Blend, data = farm_1)
summary(lm_farm_1)
check_model(lm_farm_1)

lm.log_farm_1 <- lm(log1p(Count) ~ Blend, data = farm_1) # Best model so far AIC = 352.6905
summary(lm.log_farm_1)
check_model(lm.log_farm_1)


lm2.log_farm_1 <- lm(log1p(Count) ~ Blend * Day, data = farm_1) # Best model so far AIC = 271.1738
summary(lm2.log_farm_1)
check_model(lm2.log_farm_1)

AIC(lm2.log_farm_1, lm.log_farm_1, lm_farm_1) # Model comparison here 
AIC(lm2.log_farm_1, lm.log_farm_1)

emm_lm2.log_farm_1 <- emmeans(lm2.log_farm_1, ~ Blend)
summary(emm_lm2.log_farm_1)

emm_blends_farm_1 <- cld(emm_lm2.log_farm_1, adjust = "sidak")

view(emm_blends_farm_1)

# Modelling of the data (Farm 2 Starts here)----
# linear model ....
lm_farm_2 <- lm(Count ~ Blend, data = farm_2)
summary(lm_farm_2)
check_model(lm_farm_2)

lm.log_farm_2 <- lm(log1p(Count) ~ Blend, data = farm_2) # AIC = 352.6905
summary(lm.log_farm_2)
check_model(lm.log_farm_2)

lm2.log_farm_2 <- glm(log1p(Count) ~ Blend * Day, data = farm_2) # Best model so far AIC = 204.7054
summary(lm2.log_farm_2)
check_model(lm2.log_farm_2)


AIC(lm2.log_farm_2, lm.log_farm_2, lm_farm_2) # Model comparison here 
anova(lm2.log_farm_2, lm.log_farm_2)


emm_lm2.log_farm_2 <- emmeans(lm2.log_farm_2, ~ Blend)
summary(emm_lm2.log_farm_2)

emm_blends_farm_2 <- cld(emm_lm2.log_farm_2, adjust = "bonferroni")

view(emm_blends_farm_2)


# Other models are here just for check for farm one () ----

# general linear model 
model.1_farm_1 <- glm(Count ~ Blend, data = farm_1, 
                      family =  quasipoisson(link = "log"))
summary(model.1_farm_1)

check_model(model.1_farm_1)
check_overdispersion(model.1_farm_1)
model_performance(model.1_farm_1)

# negative binomial model 1 
model.nb_farm_1 <- glm.nb(Count ~ Blend+Day, data = farm_1) 
summary(model.nb_farm_1)

check_model(model.nb_farm_1)
check_overdispersion(model.nb_farm_1)
model_performance(model.nb_farm_1)

# negative binomial model 2
model.nb2_farm_1 <- glm.nb(Count ~ Blend*Day, data = farm_1)
summary(model.nb2_farm_1)

check_model(model.nb2_farm_1)
check_overdispersion(model.nb2_farm_1)
model_performance(model.nb2_farm_1)


# Linear Mixed Effects Model (Blend and Day as fixed, Replicate as random)
glmm_farm_1 <- lmer(Count ~ Blend + (1 | Replicate), data = farm_1)
summary(glmm_farm_1)
check_model(glmm_farm_1)
check_overdispersion(glmm_farm_1)
model_performance(glmm_farm_1)
