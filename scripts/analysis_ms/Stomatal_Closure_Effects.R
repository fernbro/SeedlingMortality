library(tidyverse)

closure <- read_csv("data/Experiment/Processed/Stomata_Closure_Day.csv")


# soil water limitation?
stress_days <- read_csv("data/Experiment/Processed/Water_Limitation_Days.csv")

stom_stress <- inner_join(closure, stress_days)

stom_sum <- stom_stress %>% 
  filter(water == "drought") %>% 
  group_by(spp) %>% 
  summarise(mean_clo = mean(day_closed),
            sd_clo = sd(day_closed),
            n = n())

cor.test(stom_stress$day_closed, stom_stress$cpt)
summary(lm(day_closed ~ spp + cpt, data = stom_stress))

# emmeans(lm(day_closed ~ cpt : spp, data = stom_stress), specs = c("cpt", "spp"))

ggplot(stom_stress, aes(x = cpt, y = day_closed))+
  geom_point()+
  facet_wrap(~spp)+
  geom_abline(slope = 1, intercept = 0)+
  geom_smooth(method = "lm", se = F)+
  theme_minimal(base_size = 20)+
  labs(x = "Day of soil water limitation", y = "Day of permanent stomatal closure")

ggplot(stom_stress, aes(x = spp, y = day_closed))+
  geom_boxplot(aes(fill = spp))+
  theme_minimal(base_size = 20)+
  labs(x = "Species", y = "Day of permanent stomatal closure")

# mortality?
death_days_fl <- read_csv("data/Experiment/Processed/Death_Day_Fl-Based.csv") %>% 
  filter(treatment == "drought") %>% 
  mutate(day = death) %>% 
  dplyr::select(TreeID, day)
death_days_col <- read_csv("data/Experiment/Processed/Death_Day_Color-Based.csv")

# set to one of the above:
death_days <- death_days_col # more accurate

stom_death <- inner_join(closure, death_days) %>% 
  inner_join(stress_days)

ggplot(stom_death, aes(x = day_closed, y = day))+
  geom_point(aes(color = spp))+
  geom_abline(slope = 1, intercept = 0)+
  geom_smooth(method = "lm", se = T)+
  theme_minimal(base_size = 20)+
  labs(x = "Day of permanent stomatal closure", y = "Day of mortality")

ggplot(stom_death, aes(x = day_closed, y = day))+
  geom_point(alpha = 0.2)+
  facet_wrap(~spp, scales = "free_y")+
  geom_abline(slope = 1, intercept = 0)+
  geom_smooth(method = "lm", se = F)+
  theme_minimal(base_size = 20)+
  labs(x = "Day of permanent stomatal closure", y = "Day of mortality")

# ID <- x

# combine con data with the closure dates to add as vertical lines



summary(lm(day ~ day_closed, data = filter(stom_death, spp == "PIFL")))



