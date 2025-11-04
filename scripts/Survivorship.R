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
ggplot(brown, aes(x = date, y = brown))+
  geom_line(aes(group = TreeID, color = spp))


# let's create a T/F or 1/0 dataframe for each date and tree if 
# its dead based on certain criteria

dead_trees <- (filter(fl, Fv_Fm_dark < 0.1)) %>% 
  group_by(TreeID) %>%
  unique() %>% 
  mutate(life = 0) %>% 
  select(TreeID, spp, date, life)

alive_trees <- (filter(fl, Fv_Fm_dark >= 0.1)) %>% 
  group_by(TreeID) %>%
  unique() %>% 
  mutate(life = 1) %>% 
  select(TreeID, spp, date, life)

tree_fl <- rbind(dead_trees, alive_trees)

ggplot(tree_fl, aes(x = date, y = life))+
  geom_point()+
  geom_smooth(aes(fill = spp))

# from Allie's code:
km_species_fit <- survfit(Surv(yday(date)-202, event = life, type = "right") ~ spp, data=tree_fl)
plot(km_species_fit) # i mean, it doesnt look good.


