# Introductory part ----
# Date: September 1, 2025
# Author: Ismail A. AYEGBOYIN
# This is a real time analysis script of the host plant resistant trial
# subjected to different level of drought to check for impact of FAW infestation and fecundity

# Loading packages ----
require(tidyverse)
require(readxl)
library(lme4)
library(emmeans)
library(performance)
library(multcompView)
library(multcomp) 
# Loading, Cleaning and Manipulation of data -----
setwd("/Users/Esmael/Desktop/Data Science Library/Data for play/Fall-Armyworm-Study/Drought_Influence_Study/Data")

larva <- read_excel("drought data_10-9-25.xlsx",
                          sheet = "larva_expt")%>%
          dplyr::select(2,6,8,11,12,13,14,17,18,20,21,23)%>%
  rename(
    pot_type = pot,
    plant_height_cm = plant_heights_cm,
    pot_weight_g = `pot_weight to be maintained`,
    larval_weight_start = `larvalweight_g_30-7-2025`,
    leaf_damage_1 = `%leafdamage_31-7-25`,
    leaf_damage_2 = `%leafdamage_1-8-25`,
    leaf_damage_3 = `%leafdamage_4-8-25`,
    larval_weight_end = `larvalweight_5-8-25`,
    pupation = `pupation_10-825`,
    pupal_weight_g = `pupalweight_11-8-25`,
    adult_emergence = `adult_emergence_16-8_25`
  ) %>%
  # Convert all problematic columns at once, handling various text patterns
  mutate(
    across(c(larval_weight_start, larval_weight_end, pupal_weight_g), 
           ~as.numeric(case_when(
             . %in% c("died", "n/a", "x", "N/A") ~ NA_character_,
             TRUE ~ .
           ))),
    across(c(pupation), ~na_if(., "n/a")),
    pupation_success = if_else(pupation == "yes", 1, 0),
    pot_size = factor(str_sub(pot_type, 1, 1), levels = c("7", "9"), labels = c("Small", "Large")),
    treatment = factor(treatment, levels = c("no_drought", "drought")),
    avg_leaf_damage = (leaf_damage_1 + leaf_damage_2 + leaf_damage_3) / 3,
    larval_weight_gain = larval_weight_end - larval_weight_start # This should work now
  )

write_csv(larva, file = "/Users/Esmael/Desktop/Data Science Library/Data for play/Fall-Armyworm-Study/Drought_Influence_Study/Data/Larva.csv")

oviposition <- read_excel("drought data_10-9-25.xlsx",
                          sheet = "oviposition_expt") %>%
  rename(
    variety = variety,
    pot_type = pot,
    replicate = replicates,
    plant_id = plant_number,
    cage_id = cage_number,
    mating_pair_id = insect_mating_number,
    treatment = status...7,
    excel_date = date,
    eggmass_24h = eggmass_24h,
    other_eggs_24h = others...10,
    eggmass_48h = eggmass_48h,
    other_eggs_48h = others...12,
    notes = status...13
  ) %>%
  mutate(
    # Convert Excel numeric date
    date = as_date(excel_date, origin = "1899-12-30"),
    cage_id = factor(cage_id),
    total_eggmass = eggmass_24h + eggmass_48h,
    total_other_eggs = other_eggs_24h + other_eggs_48h,
    treatment = factor(treatment, levels = c("no_drought", "drought")),
    pot_size = factor(str_sub(pot_type, 1, 1),
                      levels = c("7", "9"),
                      labels = c("Small", "Large")),
    egg_location = case_when(
      is.na(notes) ~ "on_plant",
      str_detect(tolower(notes), "floor|cup") ~ "off_plant",
      TRUE ~ "on_plant"
    )
  ) %>%
  dplyr::select( 
    plant_id, cage_id, treatment,
    eggmass_24h, other_eggs_24h,
    eggmass_48h, other_eggs_48h,
    total_eggmass, total_other_eggs,
    egg_location, notes
  ) %>%
  group_by(cage_id, treatment)

# Data Analysis ----
# .............
# Oviposition data
# ..........................................................................
oviposition.model <- lmer(total_eggmass ~ treatment 
                          + (1|cage_id), 
                          data = oviposition)
summary(oviposition.model)
model_performance(oviposition.model)

emmeans.oviposition.model <- emmeans(
        oviposition.model, 
        pairwise ~ treatment, 
        type = "response")

# compute letters here 
cld.oviposition <- cld(emmeans.oviposition.model,
             Letters = letters,
             adjust = "sidak")
# View with grouping letters
print(cld.oviposition)




