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
  labs(x = "Date", y = "Weight (g) ", shape = "Species")

summary(lm(Pot_weight_g ~ date, data = filter(morph, spp == "PIPO" & water == "drought")))


morph_stats <- morph %>% 
  group_by(TreeID) %>% 
  summarise(max_weight = max(Pot_weight_g))

morph <- morph %>% 
  full_join(morph_stats) %>% 
  mutate(weight_frac = Pot_weight_g/max_weight)

# write_csv()

soil <- read_csv("data/Experiment/Processed/VWC.csv")

soil_comp <- inner_join(soil, morph, by = join_by(date, TreeID, spp, water, temp, id)) %>% 
  mutate(el_group = case_when(spp %in% c("PSME", "PIPO") ~ "low elevation",
                              .default = "high elevation"))


# relativize weight and VWC

comp_stats <- soil_comp %>% 
  group_by(TreeID) %>%
  summarise(max_weight = max(Pot_weight_g),
            max_vwc = max(VWC_perc))


soil_comp2 <- soil_comp %>% 
  full_join(comp_stats) %>% 
  mutate(weight_frac = Pot_weight_g/max_weight,
         vwc_frac = VWC_perc/max_vwc)
  
cor.test(soil_comp2$weight_frac, soil_comp2$vwc_frac)
  
  # arrange(date, .by_group = T)

# by fraction:
ggplot(filter(soil_comp2, el_group == "low elevation" & water == "drought"), 
       aes(x = weight_frac, y = vwc_frac))+
  geom_point(aes(color = spp))+
  # geom_path(aes(group = TreeID), lineend = "square")+
  geom_smooth(method = "lm", se = F, aes(linetype = temp))+
  # facet_wrap(~interaction(temp, el_group))+
  labs(x = "% of max weight", y = "% of max VWC")+
  theme_light(base_size = 24)
# by VWC and mass:
ggplot(filter(soil_comp2, el_group == "low elevation" & water == "drought"), 
       aes(x = Pot_weight_g, y = VWC_perc))+
  geom_point(aes(color = spp))+
  # geom_path(aes(group = TreeID), lineend = "square")+
  geom_smooth(method = "lm", se = F, aes(linetype = temp))+
  # facet_wrap(~interaction(temp, el_group))+
  labs(x = "Pot weight (g)", y = "Soil moisture (%)", linetype = "chamber")+
  theme_light(base_size = 24)
ggsave("figures/VWC_v_weight.png", last_plot(), width = 8, height = 6)

ggplot(filter(soil_comp2, date == "2025-07-30" & temp == "heatwave" & water == "drought"), 
       aes(x = water, y = vwc_frac, fill = spp))+
  geom_point()+
  facet_wrap(~spp)+
  labs(x = "Chamber", y = "% of maximum VWC on 7/30/2025", title = "Soil moisture in heatwave chamber")+
  theme_light(base_size = 26)
ggsave("figures/VWC_frac_07302025.png", last_plot(), width = 9, height = 6)

ggplot(filter(morph, date == "2025-07-30" & temp == "heatwave" & water == "drought"), 
       aes(x = water, y = weight_frac, fill = spp))+
  geom_boxplot(alpha = 0.4)+
  geom_point()+
  facet_wrap(~spp)+
  labs(x = "Chamber", y = "% of max pot weight on 7/30/2025", title = "Soil moisture in heatwave chamber")+
  theme_light(base_size = 24)
ggsave("figures/Weight_frac_07302025.png", last_plot(), width = 9, height = 6)




ggplot(filter(soil_comp2, date == "2025-07-30"), aes(x = temp, y = weight_frac, fill = spp))+
  geom_boxplot()


##################

cor.test(soil_comp$VWC_perc, soil_comp$Pot_weight_g)

ggplot(soil_comp, aes(x = Pot_weight_g, y = VWC_perc))+
  geom_point(aes(color = spp))+
 # geom_path(aes(group = TreeID), lineend = "square")+
  geom_smooth(method = "lm")+
  facet_wrap(~interaction(temp, el_group))

