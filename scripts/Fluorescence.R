library(tidyverse)

water <- read_csv("data/Experiment/Raw/Watered_Plants.csv")$TreeID
dates <- read_csv("data/Experiment/Dates.csv")

fl_files <- list.files("data/Experiment/Raw/Fluorescence", full.names = T)

fl_dat <- lapply(fl_files, read_csv)

for(i in 1:length(fl_dat)){
  fl_dat[[i]]$textdate <- str_sub(fl_files[i], start = 52, end = 59)
}

fluor <- bind_rows(fl_dat) %>% 
  mutate(date = date(as.POSIXct(textdate, tryFormats = "%m%d%Y")),
         spp = str_sub(TreeID, start = 1, end = 4),
         id = as.numeric(str_sub(TreeID, start = 5, end = 6))) %>% 
  select(-textdate) %>% 
  mutate(temp = case_when(id < 31 ~ "ambient",
                          id >= 31 ~ "heatwave"),
         water = case_when(TreeID %in% water ~ "water",
                           .default = "drought")) %>% 
  inner_join(dates)
  # group_by(Fv_Fm_dark, spp, water) %>% 
  # mutate(n_obs = n())

write_csv(fluor, "data/Experiment/Processed/Fluorescence.csv")

ggplot(fluor, aes(x = factor(week), y = Fv_Fm_dark, group = spp))+
  geom_line(alpha = 0.2, aes(group = TreeID, linetype = temp))+
  # geom_point(alpha = 0.4, size = 3)+
  geom_point(aes(group = interaction(date, water), color = water, shape = temp), alpha = 0.8)+
  geom_hline(yintercept = 0.75)+
  # geom_hline(yintercept = 0.85)+
  facet_wrap(~interaction(spp))+
  theme_light()+
  labs(x = "Week", y = "Fv/Fm")

ggplot(fluor, aes(x = date, y = Fv_Fm_dark, group = spp))+
  # geom_line(alpha = 0.4, aes(group = TreeID))+
  geom_point(alpha = 0.4, size = 3)+
  geom_smooth(aes(group = interaction(water, temp), linetype = water, color = temp), se = F)+
  #geom_boxplot(aes(group = interaction(date, spp), fill = spp))+
  #geom_hline(yintercept = 0.75)+
 # geom_hline(yintercept = 0.85)+
  facet_wrap(~spp)+
  theme_light()+
  labs(x = "Species", y = "Fv/Fm")


ggplot(fluor, aes(x = water, y = Fv_Fm_dark))+
  # geom_line(alpha = 0.4)+
  geom_boxplot()+
  # geom_point(size = 4)+
  geom_hline(yintercept = 0.75)+
  geom_hline(yintercept = 0.85)+
  facet_wrap(~spp)+
  theme_light(base_size = 26)+
  labs(x = "Species", y = "Fv/Fm", shape = "Number of obs")
# ggsave("figures/FvFm080425.png", last_plot(), width = 10, height = 6)
