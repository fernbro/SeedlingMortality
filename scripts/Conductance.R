library(tidyverse)

water <- read_csv("data/Experiment/Raw/Watered_Plants.csv")$TreeID

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
                           .default = "drought"))

# determine effective 0 conductance:
# mean(c(84.4, 82, 80.1, 78.1)) # mean of values in mmol/m2s on Whatman Paper = 81.15 mmol/m2/s


ggplot(data = con, aes(x = date, y = con, group = interaction(spp, water)))+
  geom_point(alpha = 0.4, aes(color = water))+
  geom_boxplot(aes(group = interaction(date, water), fill = water))+
  # geom_line(aes(group = TreeID), alpha = 0.4)+
  geom_hline(yintercept = 81.15)+
  #geom_smooth(method = "lm", aes(color = water, fill = water))+
  facet_wrap(~interaction(spp), nrow = 4)

