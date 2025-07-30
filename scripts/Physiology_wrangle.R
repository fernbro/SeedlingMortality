library(tidyverse)

water <- read_csv("data/Experiment/Raw/Watered_Plants.csv")$TreeID

pipo_072125 <- read_csv("data/Experiment/Raw/PIPO_Physiology_07212025.csv") %>% 
  transmute(tree = TreeID, con = Conductance_mmol_m2s, week = 1
            # , fl = Fv_Fm_light/1000
            )
pipo_072825 <- read_csv("data/Experiment/Raw/PIPO_Physiology_07282025.csv") %>% 
  transmute(tree = TreeID, con = Conductance_mmol_m2s, week = 2)
psme_072125 <- read_csv("data/Experiment/Raw/PSME_Physiology_07212025.csv") %>% 
  transmute(tree = TreeID, con = Conductance_mmol_m2s, week = 1
            # , fl = Fv_Fm_light/1000
            )
psme_072825 <- read_csv("data/Experiment/Raw/PSME_Physiology_07282025.csv") %>% 
  transmute(tree = TreeID, con = Conductance_mmol_m2s, week = 2)
pifl_072225 <- read_csv("data/Experiment/Raw/PIFL_Physiology_07222025.csv") %>% 
  transmute(tree = TreeID, con = Conductance_mmol_m2s, week = 1)
pifl_072925 <- read_csv("data/Experiment/Raw/PIFL_Physiology_07292025.csv") %>% 
  transmute(tree = TreeID, con = Conductance_mmol_m2s, week = 2)
pien_072225 <- read_csv("data/Experiment/Raw/PIEN_Physiology_07222025.csv") %>% 
  transmute(tree = TreeID, con = Conductance_mmol_m2s, week = 1)
pien_072925 <- read_csv("data/Experiment/Raw/PIEN_Physiology_07292025.csv") %>% 
  transmute(tree = TreeID, con = Conductance_mmol_m2s, week = 2)


phys <- rbind(pipo_072125, pipo_072825, psme_072125, psme_072825,
              pifl_072225, pifl_072925, pien_072225, pien_072925) %>% 
  mutate(spp = str_sub(tree, 1, 4),
         id = str_sub(tree, 5, 6)) %>% 
  mutate(temp = case_when(id <= 30 ~ "ambient",
                          .default = "heatwave"),
         water = case_when(tree %in% water ~ "water",
                           .default = "drought"))

phys_wide <- phys %>% 
  pivot_wider(names_from = week, values_from = con) %>% 
  mutate(con_1 = `1`, con_2 = `2`) %>% 
  mutate(delta_con = con_2 - con_1)

ggplot(data = phys, aes(x = as.factor(week), y = con, group = interaction(spp, water)))+
  geom_line(aes(group = tree, color = water))+
  geom_point(alpha = 0.4, aes(color = water))+
  # geom_smooth(method = "lm", aes(color = water, fill = water))+
  facet_wrap(~interaction(spp, water), nrow = 4)

ggplot(data = phys, aes(x = week, y = con, group = interaction(week, water, spp)))+
  geom_boxplot(aes(fill = water))+
  facet_wrap(~spp)+
  theme_light()

ggplot(data = phys_wide, aes(x = spp, y = delta_con, fill = water))+
  geom_boxplot()+
  geom_point(alpha = 0.4)+
  labs(x = "Species", y = "Week 2 conductance - Week 1 conductance")



TukeyHSD(aov(fl ~ spp*temp, phys))
