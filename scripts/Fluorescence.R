library(tidyverse)
library(dplyr)

start_exp <- yday(as.POSIXct("2025-07-21"))
water <- read_csv("data/Experiment/Raw/Watered_Plants.csv")$TreeID
dates <- read_csv("data/Experiment/Dates.csv") %>% 
  mutate(date = as.POSIXct(date, tryFormats = "%m/%e/%y"))
hw_colors <- c("blue", "red")
w_colors <- c("red", "blue")
hw_days <- c(19,25)

fl_files <- list.files("data/Experiment/Raw/Fluorescence", full.names = T)

fl_dat <- lapply(fl_files, read_csv)

for(i in 1:length(fl_dat)){
  fl_dat[[i]]$textdate <- str_sub(fl_files[i], start = 52, end = 59)
}

fluor <- bind_rows(fl_dat) %>% 
  dplyr::select(TreeID, Fv_Fm_dark, textdate) %>% 
  mutate(date = date(as.POSIXct(textdate, tryFormats = "%m%d%Y")),
         spp = str_sub(TreeID, start = 1, end = 4),
         id = as.numeric(str_sub(TreeID, start = 5, end = 6))) %>% 
  dplyr::select(-textdate) %>%
  mutate(temp = case_when(id < 31 ~ "ambient",
                          id >= 31 ~ "heatwave"),
         water = case_when(TreeID %in% water ~ "water",
                           .default = "drought")) %>% 
  full_join(dates) %>% 
  mutate(day = case_when(year(date) == 2025 ~ yday(date)-202,
                         year(date) == 2026 ~ 365 - 202 + yday(date))) %>% 
  filter(!is.na(Fv_Fm_dark))
  # group_by(Fv_Fm_dark, spp, water) %>% 
  # mutate(n_obs = n())

write_csv(fluor, "data/Experiment/Processed/Fluorescence.csv")

max_fl <- fluor %>% 
  group_by(TreeID) %>% 
  summarise(max = max(Fv_Fm_dark)) %>% 
  mutate(mort_perc_loss = (max-0.1)/max,
         pl95 = 0.05*max)

dead_trees <- (filter(fluor, Fv_Fm_dark < 0.1, water == "drought")) %>%  
  filter(spp=="PIFL") %>% 
  group_by(TreeID) %>% 
  mutate(weeks_dead = n()) %>% 
  filter(weeks_dead >= 2) %>%
  dplyr::select(TreeID) %>% 
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
             size = 2, alpha = 0.5)+
  facet_wrap(~spp, ncol = 1)+
  annotate("rect", alpha = 0.3, xmin = hw_days[1], xmax = hw_days[2], ymin = 0, ymax = 0.85,
           fill = "orange")+
  scale_fill_manual(values = w_colors)+
  scale_color_manual(values = w_colors)+
  scale_shape_manual(values=c(21,22))+
  theme_minimal(base_size = 15)+
  # geom_smooth(aes(group = interaction(water, temp), fill = temp, linetype = water))+
  # theme(strip.background = element_rect(color = "black", fill = "white"))+
  # theme(strip.text = element_text(colour = 'black'))+
  labs(x = "Day", y = "Fv/Fm", shape = "Temp", linetype = "Temp", color = "Water", fill = "Water")+
  theme(panel.background = element_rect(fill = 'white'), plot.background = element_rect(fill = 'white'))
# ggsave("figures/FvFm01122026.png", width = 10, height = 10, units = "in")


# new aesthetics:
ggplot(filter(fluor, water == "drought"), aes(x = day, y = Fv_Fm_dark, group = spp))+
  geom_hline(yintercept = 0.1, color = "red", linewidth = 1.5, alpha = 0.1)+
  geom_line(alpha = 0.3, aes(group = TreeID, linetype = temp, color = temp))+
  geom_point(aes(shape = temp,
                 color = temp), 
             # color = "black",
             alpha = 0.7)+
  facet_wrap(~spp, ncol = 1)+
  # scale_fill_manual(values = hw_colors, guide = T)+
  scale_color_manual(values = hw_colors)+
  scale_shape_manual(values=c(16, 15))+
  theme_minimal(base_size = 20)+
  guides(fill = guide_legend(override.aes = list(shape = c(22,21))))+
  labs(x = "Day", y = "Fv/Fm", 
       shape = "Temp", 
       linetype = "Temp",
       color = "Temp")+
  theme(panel.background = element_rect(fill = 'white'), plot.background = element_rect(fill = 'white'))
# ggsave("figures/FvFm01122026_WaterTreatments.png", width = 10, height = 10, units = "in")
