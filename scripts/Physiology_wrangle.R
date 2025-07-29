library(tidyverse)

water <- read_csv("data/Experiment/Raw/Watered_Plants.csv")$TreeID

pipo_072125 <- read_csv("data/Experiment/Raw/PIPO_Physiology_07212025.csv") %>% 
  transmute(tree = TreeID, con = Conductance_mmol_m2s, week = 1
            # , fl = Fv_Fm_light/1000
            )
psme_072125 <- read_csv("data/Experiment/Raw/PSME_Physiology_07212025.csv") %>% 
  transmute(tree = TreeID, con = Conductance_mmol_m2s, week = 1
            # , fl = Fv_Fm_light/1000
            )
pipo_072825 <- read_csv("data/Experiment/Raw/PIPO_Physiology_07282025.csv") %>% 
  transmute(tree = TreeID, con = Conductance_mmol_m2s, week = 2)
psme_072825 <- read_csv("data/Experiment/Raw/PSME_Physiology_07282025.csv") %>% 
  transmute(tree = TreeID, con = Conductance_mmol_m2s, week = 2)


phys <- rbind(pipo_072125, psme_072125) %>% 
  rbind(pipo_072825) %>% 
  rbind(psme_072825) %>% 
  mutate(spp = str_sub(tree, 1, 4),
         id = str_sub(tree, 5, 6)) %>% 
  mutate(temp = case_when(id <= 30 ~ "ambient",
                          .default = "heatwave"),
         water = case_when(tree %in% water ~ "water",
                           .default = "drought"))

ggplot(data = phys, aes(x = as.factor(week), y = con, group = interaction(spp, water)))+
  # geom_line(aes(color = water))+
  # geom_point(alpha = 0.4, aes(color = water))+
  geom_smooth(method = "lm", aes(color = water, fill = water))+
  facet_wrap(~spp)

ggplot(data = phys, aes(x = week, y = con, group = interaction(week, water, spp)))+
  geom_boxplot(aes(fill = water))+
  facet_wrap(~spp)+
  theme_light()







TukeyHSD(aov(fl ~ spp*temp, phys))
