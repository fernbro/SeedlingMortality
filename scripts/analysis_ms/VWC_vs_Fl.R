library(tidyverse)

wks <- read_csv("data/Experiment/Dates.csv") %>% 
  mutate(date = as.Date(date, format = "%m/%d/%y"))
soil <- read_csv("data/Experiment/Processed/VWC.csv") %>% 
  mutate(date = as.Date(date)) %>% 
  inner_join(wks) %>% 
  dplyr::select(-day, -date)
fl <- read_csv("data/Experiment/Processed/Fluorescence.csv") %>% 
  mutate(date = as.Date(date)) %>% 
  inner_join(wks) %>% 
  dplyr::select(-day, -date)

data <- full_join(soil, fl) %>% 
  filter(!is.na(Fv_Fm_dark))

ggplot(data, aes(x = VWC_perc, y = Fv_Fm_dark))+
  geom_point(aes(color = spp))+
  geom_path(aes(group = TreeID))

vwc0 <- data %>% 
  group_by(TreeID, spp) %>% 
  arrange(week) %>% 
  filter(VWC_perc < 0.1) %>% 
  summarise(vwc0_wk = min(week))
fl0 <- data %>% 
  group_by(TreeID, spp) %>% 
  arrange(week) %>% 
  filter(Fv_Fm_dark < 0.1) %>% 
  summarise(fl0_wk = min(week))

wk0 <- full_join(vwc0, fl0)  

ggplot(wk0, aes(x = fl0_wk, y = vwc0_wk))+
  geom_point(aes(color = spp))+
  geom_smooth(method = "lm", se = F, aes(color = spp))+
  labs(x = "Week of Fv/Fm < 0.1", y = "Week of VWC < 0.1%")+
  theme_minimal(base_size = 20)


