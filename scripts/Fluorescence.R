library(tidyverse)
library(dplyr)

start_exp <- yday(as.POSIXct("2025-07-21"))
water <- read_csv("data/Experiment/Raw/Watered_Plants.csv")$TreeID
dates <- read_csv("data/Experiment/Dates.csv") %>% 
  mutate(date = as.POSIXct(date, tryFormats = "%m/%e/%y"))
hw_colors <- c("blue", "red")
hw_days <- c(19,25)

fl_files <- list.files("data/Experiment/Raw/Fluorescence", full.names = T)

fl_dat <- lapply(fl_files, read_csv)

for(i in 1:length(fl_dat)){
  fl_dat[[i]]$textdate <- str_sub(fl_files[i], start = 52, end = 59)
}

fluor <- bind_rows(fl_dat) %>% 
  select(TreeID, Fv_Fm_dark, textdate) %>% 
  mutate(date = date(as.POSIXct(textdate, tryFormats = "%m%d%Y")),
         spp = str_sub(TreeID, start = 1, end = 4),
         id = as.numeric(str_sub(TreeID, start = 5, end = 6))) %>% 
  select(-textdate) %>%
  mutate(temp = case_when(id < 31 ~ "ambient",
                          id >= 31 ~ "heatwave"),
         water = case_when(TreeID %in% water ~ "water",
                           .default = "drought")) %>% 
  inner_join(dates) %>% 
  mutate(day = yday(date)-202)
  # group_by(Fv_Fm_dark, spp, water) %>% 
  # mutate(n_obs = n())

write_csv(fluor, "data/Experiment/Processed/Fluorescence.csv")

dead_trees <- (filter(fluor, Fv_Fm_dark < 0.1, water == "drought")) %>%  
  group_by(TreeID) %>% 
  mutate(weeks_dead = n()) %>% 
  filter(weeks_dead >= 2) %>%
  select(TreeID) %>% 
  arrange(TreeID) %>% 
  unique()
# PLUS psme22 and pipo56 (removed before 2 weeks of fl<0.1 bc of mold)
# all droughted FLUORESCED of PIEN, PIPO, and PSME are dead as of 11/30/2025

fluor$spp <- factor(fluor$spp, levels = c("PSME", "PIPO", "PIEN", "PIFL"))


ggplot(fluor, aes(x = day, y = Fv_Fm_dark, group = spp))+
  geom_hline(yintercept = 0.1, color = "red", linewidth = 1.5, alpha = 0.1)+
  geom_line(alpha = 0.3, aes(group = TreeID, linetype = temp))+
  geom_point(aes(group = interaction(date, water), fill = water, color = water,
                 shape = temp), 
             size = 2, alpha = 0.6)+
  facet_wrap(~spp, ncol = 1)+
  # annotate("rect", alpha = 0.3, xmin = hw_days[1], xmax = hw_days[2], ymin = 0, ymax = 0.85,
  #          fill = "orange")+
  # scale_fill_manual(values = hw_colors)+
  scale_shape_manual(values=c(21,22))+
  theme_minimal(base_size = 15)+
  # geom_smooth(aes(group = interaction(water, temp), fill = temp, linetype = water))+
  # theme(strip.background = element_rect(color = "black", fill = "white"))+
  # theme(strip.text = element_text(colour = 'black'))+
  labs(x = "Day", y = "Fv/Fm", shape = "Temp", linetype = "Temp", color = "Water", fill = "Water")
# ggsave("figures/FvFm090225.png", width = 10, height = 8, units = "in")


# new aesthetics:
ggplot(fluor, aes(x = day, y = Fv_Fm_dark, group = spp))+
  geom_hline(yintercept = 0.1, color = "red", linewidth = 1.5, alpha = 0.1)+
  geom_line(alpha = 0.3, aes(group = TreeID, linetype = temp, color = temp))+
  geom_point(aes(shape = water,
                 fill = temp,
                 color = temp), 
             # color = "black",
             alpha = 0.7)+
  facet_wrap(~spp+water, ncol = 2)+
  scale_fill_manual(values = hw_colors, guide = T)+
  scale_color_manual(values = hw_colors)+
  scale_shape_manual(values=c(22, 1))+
  theme_minimal(base_size = 20)+
  guides(fill = guide_legend(override.aes = list(shape = c(22,21))))+
  labs(x = "Day", y = "Fv/Fm", 
       shape = "Water", 
       linetype = "Temp",
       fill = "Temp")
