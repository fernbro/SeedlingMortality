library(tidyverse)

water <- read_csv("data/Experiment/Raw/Watered_Plants.csv")$TreeID

vwc_files <- list.files("data/Experiment/Raw/VWC", full.names = T)

vwc_dat <- lapply(vwc_files, read_csv)

# names(vwc_dat) <- str_sub(vwc_files, start = 25, end = 41)

for(i in 1:length(vwc_dat)){
  vwc_dat[[i]]$textdate <- str_sub(vwc_files[i], start = 34, end = 41)
  }

vwc <- bind_rows(vwc_dat) %>% 
  mutate(date = date(as.POSIXct(textdate, tryFormats = "%m%d%Y")),
         spp = str_sub(TreeID, start = 1, end = 4),
         id = as.numeric(str_sub(TreeID, start = 5, end = 6))) %>% 
  select(-textdate) %>% 
  mutate(temp = case_when(id < 31 ~ "ambient",
                          id >= 31 ~ "heatwave"),
         water = case_when(TreeID %in% water ~ "water",
                           .default = "drought"))

# write_csv(vwc, "data/Experiment/Processed/VWC.csv")


ggplot(vwc, aes(x = yday(date), y = VWC_perc))+
  # geom_line(alpha = 0.4, aes(group = TreeID))+
  geom_smooth(aes(linetype = spp, fill = spp), method = "lm")+
  geom_point(aes(shape = spp))+
  facet_wrap(~water)+
  theme_light(base_size = 26)+
  labs(x = "Julian day", y = "Soil moisture (%)", shape = "Species", linetype = "Species", fill = "Species")
# ggsave("figures/VWC_v_time.jpg", last_plot(), width = 8, height = 5)
