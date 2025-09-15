library(tidyverse)
library(dplyr)

water <- read_csv("data/Experiment/Raw/Watered_Plants.csv")$TreeID
hw_colors <- c("blue", "red")

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
                           .default = "drought")) %>% 
  data.frame()

vwc_sum <- vwc %>% 
  dplyr::group_by(spp, water, temp, date) %>% 
  dplyr::summarise(vwc_mean = mean(VWC_perc, na.rm = T), 
            vwc_sd = sd(VWC_perc, na.rm = T))

write_csv(vwc, "data/Experiment/Processed/VWC.csv")

ggplot(vwc_sum, aes(x = date, y = vwc_mean))+
  facet_wrap(~interaction(water, spp), ncol = 2)+
  geom_point(aes(color = temp))+
  geom_hline(yintercept = 0)+
  # geom_smooth(se = F, aes(group = temp))+
  geom_line(aes(color = temp))+
  geom_errorbar(aes(color = temp, ymin = vwc_mean - vwc_sd, ymax = vwc_mean + vwc_sd))

ggplot(filter(vwc, water == "water"), aes(x = yday(date), y = VWC_perc))+
  # geom_line(alpha = 0.4, aes(group = TreeID))+
  # geom_smooth(aes(linetype = spp, fill = spp), method = "lm")+
  # geom_point(aes(shape = spp))+
  geom_boxplot(aes(group = date))+
  facet_wrap(~water)+
  theme_light(base_size = 26)+
  labs(x = "Julian day", y = "Soil moisture (%)", shape = "Species", linetype = "Species", fill = "Species")
# ggsave("figures/VWC_v_time.jpg", last_plot(), width = 8, height = 5)

ggplot(filter(vwc, water == "drought"), aes(x = yday(date), y = VWC_perc))+
  # geom_line(alpha = 0.4, aes(group = TreeID))+
  # geom_smooth(aes(linetype = spp, fill = spp), method = "lm")+
  geom_point(pch = 1, alpha = 0.2)+ 
  geom_boxplot(aes(group = interaction(yday(date), temp), fill = temp))+
  scale_fill_manual(values = hw_colors)+
  facet_wrap(~spp)+
  theme_light(base_size = 20)+
  labs(x = "Julian day", y = "Soil moisture (%)", linetype = "Species", fill = "Species")
#ggsave("figures/VWC_v_time_preHW.jpg", last_plot(), width = 8, height = 5)


ggplot(filter(vwc, water == "drought"), aes(x = yday(date), y = VWC_perc))+
  geom_line(alpha = 0.4, aes(linetype = temp, color = temp, group = TreeID))+
  # geom_smooth(method = "lm", aes(fill = temp, group = TreeID), se = F)+
  # geom_boxplot(aes(group = yday(date)))+
  geom_point(pch = 1, alpha = 1, aes(color = temp))+
  scale_color_manual(values = hw_colors)+
  facet_wrap(~spp)+
  theme_light(base_size = 20)+
  labs(x = "Julian day", y = "Soil moisture (%)", linetype = "Species", fill = "Species")
#ggsave("figures/VWC_Box_preHW.jpg", last_plot(), width = 8, height = 5)

ggplot(filter(vwc, water == "drought"), aes(x = yday(date), y = VWC_perc))+
  geom_line(alpha = 0.4, aes(linetype = temp, color = temp, group = TreeID))+
  # geom_smooth( aes(fill = temp))+
  # geom_boxplot(aes(group = yday(date)))+
  geom_point(pch = 1, alpha = 1, aes(color = temp))+
  scale_color_manual(values = hw_colors)+
  scale_fill_manual(values = hw_colors)+
  facet_wrap(~interaction(spp))+
  theme_light(base_size = 20)+
  labs(x = "Julian day", y = "Soil moisture (%)", linetype = "Species", fill = "Species")

vwc_max <- vwc %>% 
  group_by(TreeID) %>%
  summarise(max_vwc = max(VWC_perc))

vwc_comp <- full_join(vwc, vwc_max) %>% 
  mutate(vwc_frac = VWC_perc/max_vwc)

ggplot(filter(vwc_comp, water == "drought"), aes(x = yday(date), y = vwc_frac))+
  geom_boxplot(aes(group = yday(date)))+
  geom_point(pch = 1, alpha = 0.2)+
  facet_wrap(~spp)+
  theme_light(base_size = 20)+
  labs(x = "Julian day", y = "Fraction of max soil moisture", linetype = "Species", fill = "Species")
#ggsave("figures/VWCfrac_Box_preHW.jpg", last_plot(), width = 8, height = 5)


soil_avgs <- filter(vwc_comp, 
                    water == "drought") %>%  
  # update with more recent date after this week
  group_by(spp) %>% 
  summarise(mean_vwc_frac = mean(vwc_frac))





