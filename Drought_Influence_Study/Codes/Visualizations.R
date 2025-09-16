# .............
# Visualizations .......
# ..........................................................................

# summarise before pivoting
eggmass_total <- oviposition %>%
  group_by(cage_id, treatment) %>%
  summarise(total_eggmass = mean(total_eggmass, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = treatment, values_from = total_eggmass)

# do the same for other eggs
other_total <- oviposition %>%
  group_by(cage_id, treatment) %>%
  summarise(total_other_eggs = mean(total_other_eggs, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = treatment, values_from = total_other_eggs)

ggplot(eggmass_total, aes(x = 1, y = no_drought)) +
  geom_point(color="blue", size=3) +
  geom_point(aes(y = drought), color="red", size=3) +
  geom_segment(aes(x=1, xend=1.5, y=no_drought, yend=drought),
               color="gray") +
  scale_x_continuous(breaks=c(1,1.5), labels=c("No drought","Drought")) +
  labs(title="Paired eggmass counts per cage",
       y="Total eggmass") +
  theme_classic()

# .............
# ..........................................................................
ovi_long <- oviposition %>%
  dplyr::select(cage_id, treatment, eggmass_24h, eggmass_48h, total_eggmass,
                other_eggs_24h, other_eggs_48h, total_other_eggs) %>%
  pivot_longer(
    cols = -c(cage_id, treatment),
    names_to = "variable", values_to = "count"
  )

ggplot(ovi_long, aes(x = treatment, y = count, fill = treatment)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.7) +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "Egg-laying patterns across drought treatments",
       y = "Egg count")


ovi_summary <- ovi_long %>%
  group_by(treatment, variable) %>%
  summarise(
    mean = mean(count, na.rm = TRUE),
    sem = sd(count, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

ggplot(ovi_summary, aes(x = mean, y = variable, fill = treatment)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(
    aes(xmin = mean - sem, xmax = mean + sem),
    position = position_dodge(0.8),
    width = 0.3
  ) +
  theme_classic() +
  labs(
    # title = "Egg-laying patterns across drought treatments",
    x = "Mean ± SEM (egg count)",
    y = "Variable"
  ) +
  theme(
    legend.position = "top",
    text = element_text(size = 12)
  )

ggplot(oviposition, aes(x = treatment, y = total_eggmass, fill = treatment)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  theme_classic() +
  labs(title = "Distribution of Eggmass", y = "Total Eggmass", x = "Treatment")

# .............
# Larva data
# ..........................................................................
ggplot(larva, aes(x = pot_size, y = plant_height_cm, fill = pot_size)) +
  geom_boxplot(alpha = 0.6, width = 0.4) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  theme_classic() +
  labs(title = "Plant Height by Pot Size", x = "Pot Size", y = "Plant Height (cm)")

larva %>%
  group_by(treatment) %>%
  summarise(mean_damage = mean(avg_leaf_damage, na.rm = TRUE),
            sem_damage = sd(avg_leaf_damage, na.rm = TRUE) / sqrt(n())) %>%
  ggplot(aes(x = mean_damage, y = treatment, fill = treatment)) +
  geom_col(width = 0.5, alpha = 0.7) +
  geom_errorbarh(aes(xmin = mean_damage - sem_damage, xmax = mean_damage + sem_damage),
                 height = 0.2, color = "black") +
  theme_classic() +
  labs(title = "Average Leaf Damage by Treatment",
       x = "Average Leaf Damage (%)", y = "")


larva_long <- larva %>%
  pivot_longer(cols = starts_with("leaf_damage"),
               names_to = "timepoint",
               values_to = "damage") %>%
  mutate(timepoint = recode(timepoint,
                            leaf_damage_1 = "24h",
                            leaf_damage_2 = "48h",
                            leaf_damage_3 = "120h"),
         timepoint = factor(timepoint, levels = c("24h", "48h", "120h")))

ggplot(larva_long, aes(x = timepoint, y = damage, color = treatment, group = treatment)) +
  stat_summary(fun = mean, geom = "line", size = 0.8) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  theme_minimal() +
  labs(title = "Leaf Damage Progression Over Time",
       x = "Time after Infestation",
       y = "Leaf Damage (%)")

larva %>%
  group_by(treatment) %>%
  summarise(pupation_rate = mean(pupation == "yes", na.rm = TRUE),
            adult_emergence_rate = mean(adult_emergence %in% c("M","F"), na.rm = TRUE)) %>%
  pivot_longer(-treatment, names_to = "stage", values_to = "rate") %>%
  ggplot(aes(x = treatment, y = rate, fill = stage)) +
  geom_col(position = position_dodge(), width = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  labs(title = "Pupation & Adult Emergence Rates by Treatment", y = "Proportion", x = "Treatment")


sex_summary <- larva %>%
  filter(adult_emergence %in% c("M","F")) %>%
  group_by(treatment, adult_emergence) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(treatment) %>%
  mutate(prop = count / sum(count))

ggplot(sex_summary, aes(x = treatment, y = prop, fill = adult_emergence)) +
  geom_col(position = "fill", width = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  labs(title = "Sex Ratio of Adult Emergence by Treatment",
       x = "Treatment", y = "Proportion (%)", fill = "Sex")

ggplot(sex_summary, aes(x = treatment, y = count, fill = adult_emergence)) +
  geom_col(position = position_dodge(), width = 0.5) +
  theme_minimal() +
  labs(title = "Adult Emergence (Counts) by Treatment",
       x = "Treatment", y = "Number of Adults", fill = "Sex")