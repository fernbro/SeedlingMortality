library(tidyverse)

water <- read_csv("data/Experiment/Raw/Watered_Plants.csv")$TreeID
dates <- read_csv("data/Experiment/Dates.csv")

con_files <- list.files("data/Experiment/Raw/Conductance", full.names = T)

con_dat <- lapply(con_files, read_csv)

for(i in 1:length(con_dat)){
  con_dat[[i]]$textdate <- str_sub(con_files[i], start = 49, end = 56)
}

con <- bind_rows(con_dat) %>% 
  mutate(date = as.POSIXct(textdate, tryFormats = "%m%d%Y"),
         spp = str_sub(TreeID, start = 1, end = 4),
         id = as.numeric(str_sub(TreeID, start = 5, end = 6)),
         con = Conductance_mmol_m2s) %>%  # rounded diameter
  select(-textdate, -Fv_Fm_light, -VWC_perc) %>% 
  mutate(temp = case_when(id < 31 ~ "ambient",
                          id >= 31 ~ "heatwave"),
         water = case_when(TreeID %in% water ~ "water",
                           .default = "drought")) %>%
  inner_join(dates)

# determine effective 0 conductance:
# mean(c(84.4, 82, 80.1, 78.1)) # mean of values in mmol/m2s on Whatman Paper = 81.15 mmol/m2/s


ggplot(data = con, aes(x = date, y = con, group = interaction(spp, temp, water)))+
  # geom_boxplot(aes(group = interaction(date, water), fill = water))+
  # geom_line(aes(group = TreeID), alpha = 0.2)+
  geom_point(alpha = 0.7, aes(color = water))+
  geom_hline(yintercept = 81.15)+
  geom_smooth(method = "lm", aes(color = water, fill = water), se = T)+
  facet_wrap(~interaction(spp, temp), nrow = 4)+
  theme_light(base_size = 20)+
  labs(x = "Date", y = "Foliar conductance (mmol/m2s)")

ggplot(data = con, aes(x = week, y = log(con), group = interaction(spp, temp, water)))+
  # geom_boxplot(aes(group = interaction(date, water), fill = water))+
  # geom_line(aes(group = TreeID), alpha = 0.2)+
  geom_boxplot(alpha = 0.7, aes(group = interaction(date, water), fill = water))+
  # geom_hline(yintercept = 81.15)+
  # geom_smooth(method = "lm", aes(color = water, fill = water), se = T)+
  facet_wrap(~interaction(spp, temp), nrow = 4, scales = "free_y")+
  theme_light(base_size = 20)+
  labs(x = "Date", y = "Foliar conductance (mmol/m2s)")

