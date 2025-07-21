library(tidyverse)

pipo_072125 <- read_csv("data/Experiment/Raw/PIPO_Phys_07212025.csv") %>% 
  transmute(tree = TreeID, con = Conductance_mmol_m2s, fl = Fv_Fm_light/1000)
psme_072125 <- read_csv("data/Experiment/Raw/PSME_Phys_07212025.csv") %>% 
  transmute(tree = TreeID, con = Conductance_mmol_m2s, fl = Fv_Fm_light/1000)


phys <- rbind(pipo_072125, psme_072125) %>% 
  mutate(spp = str_sub(tree, 1, 4),
         id = str_sub(tree, 5, 6)) %>% 
  mutate(temp = case_when(id <= 30 ~ "ambient",
                          .default = "heatwave"))

ggplot(data = phys, aes(x = spp, y = fl))+
  geom_boxplot(aes(fill = temp))+
  geom_point(alpha = 0.4, aes(color = temp))

ggplot(data = phys, aes(x = spp, y = fl))+
  geom_boxplot()+
  geom_point(alpha = 0.4)

summary(aov(fl ~ spp + temp, phys))
