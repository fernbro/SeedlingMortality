library(tidyverse)

pipo_morph_1 <- read_csv("data/Experiment/Raw/PIPO_TheWorks_07232025.csv")

psme_morph_1 <- read_csv("data/Experiment/Raw/PSME_TheWorks_07232025.csv")

pien_morph_1 <- read_csv("data/Experiment/Raw/PIEN_TheWorks_07242025.csv")

pifl_morph_1 <- read_csv("data/Experiment/Raw/PIFL_TheWorks_07242025.csv")


morph_1 <- rbind(pipo_morph_1, psme_morph_1) %>% 
  rbind(pien_morph_1) %>% 
  rbind(pifl_morph_1) %>% 
  mutate(spp = str_sub(TreeID, 1, 4))

ggplot(morph_1, aes(x = Pot_weight_g))+
  geom_density(aes(fill = spp), alpha = 0.4)

ggplot(morph_1, aes(x = Diameter_mm))+
  geom_density(aes(fill = spp), alpha = 0.4)

# ggplot(morph_1, aes(x = Diameter_mm, y = Pot_weight_g, color = spp))+
#   geom_point()+
#   geom_smooth(method = "lm", se = F)+
#   facet_wrap(~spp)
