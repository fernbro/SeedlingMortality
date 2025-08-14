library(tidyverse)

sd <- read_csv("data/Experiment/Raw/SD_example.csv") %>% 
  group_by(TreeID) %>% 
  summarise(sd = mean(StomatalDensity))

hist(sd$sd)
