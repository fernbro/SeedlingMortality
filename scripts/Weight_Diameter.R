library(tidyverse)

water <- read_csv("data/Experiment/Raw/Watered_Plants.csv")$TreeID

morph_files <- list.files("data/Experiment/Raw/TheWorks", full.names = T)

morph_dat <- lapply(morph_files, read_csv)

names(morph_dat) <- str_sub(morph_files, start = 30, end = 42)

for(i in 1:length(morph_dat)){
  morph_dat[[i]]$textdate <- str_sub(morph_files[i], start = 44, end = 51)
}

morph <- bind_rows(morph_dat) %>% 
  mutate(date = as.POSIXct(textdate, tryFormats = "%m%d%Y"),
         spp = str_sub(TreeID, start = 1, end = 4),
         id = as.numeric(str_sub(TreeID, start = 5, end = 6)),
         Brown_perc = as.numeric(str_sub(Perc_brown, start = 1, end = 2)),
         Diam_mm = round(Diameter_mm, 1)) %>%  # rounded diameter
  select(-textdate, -Perc_brown, -Diameter_mm) %>% 
  mutate(temp = case_when(id < 31 ~ "ambient",
                          id >= 31 ~ "heatwave"),
         water = case_when(TreeID %in% water ~ "water",
                           .default = "drought"))

ggplot(morph, aes(x = date, y = Pot_weight_g))+
  geom_line(alpha = 0.4, aes(group = TreeID))+
  geom_point(aes(shape = spp))+
  geom_smooth(method = "lm")+
  facet_wrap(~interaction(water, spp))+
  theme_light()+
  labs(x = "Date", y = "Weight (g) ", shape = "Species")+
  geom_text(aes(label = id))

summary(lm(Pot_weight_g ~ date, data = filter(morph, spp == "PIPO" & water == "drought")))

soil <- read_csv("data/Experiment/Processed/VWC.csv")

soil_comp <- inner_join(soil, morph, by = join_by(date, TreeID, spp, water, temp, id)) %>% 
  mutate(el_group = case_when(spp %in% c("PSME", "PIPO") ~ "low elevation",
                              .default = "high elevation"))


# relativize weight and VWC

soil_wide <- soil_comp %>% 
  group_by(TreeID) %>%
  arrange(date, .by_group = T)



##################

cor.test(soil_comp$VWC_perc, soil_comp$Pot_weight_g)

ggplot(soil_comp, aes(x = Pot_weight_g, y = VWC_perc))+
  geom_point(aes(color = spp))+
  geom_path(aes(group = TreeID), lineend = "square")+
  geom_smooth(method = "lm")+
  facet_wrap(~interaction(temp, el_group))

