library(tidyverse)

water <- read_csv("data/Experiment/Raw/Watered_Plants.csv")$TreeID

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
  group_by(Fv_Fm_dark, spp, water) %>% 
  mutate(n_obs = n())

# write_csv(fluor, "data/Experiment/Processed/Fluorescence.csv")

ggplot(fluor, aes(x = spp, y = Fv_Fm_dark, group = TreeID, shape = factor(n_obs)))+
  # geom_line(alpha = 0.4)+
  geom_point(alpha = 0.4, size = 3)+
  geom_hline(yintercept = 0.75)+
  geom_hline(yintercept = 0.85)+
  facet_wrap(~water)+
  theme_light()+
  labs(x = "Species", y = "Fv/Fm", shape = "Number of obs")


ggplot(fluor, aes(x = spp, y = Fv_Fm_dark))+
  # geom_line(alpha = 0.4)+
  # geom_boxplot()+
  geom_point(alpha = 0.3)+
  geom_hline(yintercept = 0.75)+
  geom_hline(yintercept = 0.85)+
  facet_wrap(~interaction(water, temp))+
  theme_light(base_size = 26)+
  labs(x = "Species", y = "Fv/Fm")
# ggsave("figures/FvFm080425.png", last_plot(), width = 10, height = 6)
