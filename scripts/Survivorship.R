library(tidyverse)
# install.packages('survival')
library(survival)

start_exp <- yday(as.POSIXct("2025-07-21"))
fl <- read_csv("data/Experiment/Processed/Fluorescence.csv") %>% 
  mutate(date = date(date))
con <- read_csv("data/Experiment/Processed/Conductance.csv") %>% 
  mutate(date = date(date))
brown <- read_csv("data/Experiment/Processed/Ocular_Color.csv") %>% 
  mutate(date = date(date))


# ummm what is going on here bruh
ggplot(brown, aes(x = day, y = brown))+
  geom_line(aes(group = TreeID, color = spp), alpha = 0.6)+
  geom_point(aes(color = spp))+
  # geom_smooth(aes(group = spp))+
  theme_minimal(base_size = 20)+
  facet_wrap(~water+temp)+
  labs(x = "Day", y = "% of foliage brown", color = "Species")

# Time of death from fl:

fl_death <- (filter(fl, Fv_Fm_dark < 0.1)) %>% 
  group_by(spp, TreeID) %>%
  summarise(tod = min(day))

st_closure <- (filter(con, con < 90)) %>% 
  group_by(spp, TreeID) %>%
  summarise(tod = min(day))
  
fl_con <- inner_join(fl_death, st_closure, join_by(TreeID, spp)) %>% 
  group_by(spp, TreeID) %>% 
  transmute(fl_d = tod.x, con_d = tod.y) %>% 
  mutate(diff = fl_d - con_d) %>% 
  mutate(status = case_when(diff < 0 ~ "Closure after death", 
                            diff == 0 ~ "Closure at death",
                            diff > 0 ~ "Closure before death"))

ggplot(fl_con, aes(x = fl_d, y = con_d))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0)

ggplot(fl_con, aes(x = status))+
  geom_histogram(stat = "count")

ggplot(fl_con, aes(y = spp, x = diff))+
  geom_boxplot(fill = "lightpink")+
  theme_minimal(base_size = 20)+
  labs(y = "Species", x = "Day of loss of photosynthesis - day of stomatal closure")

ggplot(fl_con, aes(x = diff))+
  geom_density(aes(fill = spp), alpha = 0.3)+
  theme_minimal(base_size = 20)+
  labs(y = "Species", x = "Day of loss of photosynthesis - day of stomatal closure")

TukeyHSD(aov(diff ~ spp, fl_con))

ggplot(fl_con, aes(x = diff))+
  geom_density()





# let's create a T/F or 1/0 dataframe for each date and tree if 
# its dead based on certain criteria

dead_trees <- (filter(fl, Fv_Fm_dark < 0.1)) %>% 
  group_by(TreeID) %>%
  unique() %>% 
  mutate(life = 1) %>% 
  select(TreeID, spp, date, life, temp, water)

alive_trees <- (filter(fl, Fv_Fm_dark >= 0.1)) %>% 
  group_by(TreeID) %>%
  unique() %>% 
  mutate(life = 0) %>% 
  select(TreeID, spp, date, life, temp, water)

tree_fl <- rbind(dead_trees, alive_trees) %>% 
  mutate(time = yday(date)-202)

ggplot(tree_fl, aes(x = time, y = life))+
  geom_point()+
  geom_smooth(aes(fill = spp))

library(ggfortify)
# from Allie's code:
km <- with(tree_fl, Surv(time, life))
km_fit <- survfit(Surv(time, life) ~ spp, data=filter(tree_fl, water == "drought"))
plot(km_fit)
autoplot(km_fit)+
  theme_minimal(base_size = 20)+
  labs(x = "Day", y = "% Survival")

km_fit1 <- survfit(Surv(time, life) ~ spp, data=filter(tree_fl, water == "water" & temp == "ambient"))
plot(km_fit1)
autoplot(km_fit1)+
  theme_minimal(base_size = 20)+
  labs(x = "Day", y = "% Survival")+
  ylim(c(0, 1))

km_hw <- survfit(Surv(time, life) ~ spp, data=filter(tree_fl, water == "drought" & temp == "heatwave"))
plot(km_hw)
autoplot(km_hw)+
  theme_minimal(base_size = 20)+
  labs(x = "Day", y = "% Survival")

