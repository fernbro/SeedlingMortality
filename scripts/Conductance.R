library(tidyverse)

start_exp <- yday(as.POSIXct("2025-07-21"))
water <- read_csv("data/Experiment/Raw/Watered_Plants.csv")$TreeID
dates <- read_csv("data/Experiment/Dates.csv") %>% 
  mutate(date = as.POSIXct(date, tryFormats = "%m/%e/%y"))
hw_colors <- c("blue", "red")
hw_days <- c(19,25)

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
  full_join(dates) %>% 
  filter(con > 10, con < 500, !is.na(con)) %>% 
  mutate(day = case_when(year(date) == 2025 ~ yday(date)-202,
                         year(date) == 2026 ~ 365 - 202 + yday(date))) %>% 
  mutate(sensor = case_when(day < 90 ~ 1, day >= 90 ~ 2))


# Create a label for when the sensor changed and then can group the regressions/points by sensor

write_csv(con, "data/Experiment/Processed/Conductance.csv")

# determine effective 0 conductance:
# mean(c(84.4, 82, 80.1, 78.1)) # mean of values in mmol/m2s on Whatman Paper = 81.15 mmol/m2/s


ggplot(data = con, aes(x = yday(date), y = con, group = interaction(spp, temp, water)))+
  geom_point(alpha = 0.7, aes(color = water))+
  geom_hline(yintercept = 81.15)+
  geom_smooth(method = "lm", aes(group = interaction(sensor, water), color = water, fill = water), se = T, alpha = 0.3)+
  facet_wrap(~interaction(temp, spp), nrow = 4)+
  theme_light(base_size = 20)+
  labs(x = "Julian Day", y = "Stomatal conductance (mmol/m2s)")


ggplot(filter(con, temp == "heatwave"), aes(x = yday(date), y = con, 
                                            group = interaction(spp, temp, water)))+
  annotate("rect", alpha = 0.5, xmin = 220, xmax = 227, ymin = 0, ymax = 500,
           fill = "orange")+
  geom_point(alpha = 0.7, aes(color = water))+
  geom_hline(yintercept = 81.15)+
  geom_smooth(aes(group = interaction(sensor, water), color = water, fill = water), se = T, alpha = 0.3,
              span = 0.5)+
  facet_wrap(~interaction(temp, spp), nrow = 4)+
  theme_light(base_size = 20)+
  labs(x = "Julian Day", y = "Stomatal conductance (mmol/m2s)")

ggplot(con, aes(x = yday(date), y = log(con), group = interaction(spp, temp, water)))+
  # annotate("rect", alpha = 0.5, xmin = 220, xmax = 227, ymin = 0, ymax = 500,
  #          fill = "orange")+
  # geom_point(alpha = 0.7, aes(color = water))+
  # geom_hline(yintercept = 81.15)+
  geom_smooth(aes(color = water, linetype = temp, fill = water), se = T, alpha = 0.3,
              span = 0.5)+
  facet_wrap(~interaction(spp), nrow = 2)+
  theme_light(base_size = 20)+
  labs(x = "Julian Day", y = "Stomatal conductance (mmol/m2s)")

TukeyHSD(aov(con ~ temp:spp:water, con))

######

ggplot(data = filter(con), aes(x = week, y = (con), group = interaction(spp, temp, water)))+
  geom_boxplot(alpha = 0.7, aes(group = interaction(date, water), fill = water))+
  facet_wrap(~interaction(spp, temp), nrow = 4, scales = "free_y")+
  theme_light(base_size = 20)+
  labs(x = "Week", y = "Foliar conductance (mmol/m2s)")

ggplot(data = con, aes(x = day, y = (con), group = interaction(spp, temp, water)))+
  annotate("rect", alpha = 0.15, xmin = 3.5, xmax = 4.5, ymin = 0, ymax = 450,
            fill = "red")+ 
  # geom_boxplot(alpha = 0.7, aes(group = interaction(date, temp), fill = temp))+
  geom_point(alpha = 0.7, aes(color = water))+
  geom_hline(yintercept = 81)+
  # geom_smooth(method = "lm", aes(fill = temp))+
  geom_line(aes(group = TreeID), alpha = 0.2)+
  # scale_color_manual(values = hw_colors)+
  facet_wrap(~temp+spp, nrow = 2)+
  theme_minimal(base_size = 20)+
  labs(x = "Day", y = "Conductance (mmol/m2s)")

# avgs by spp and treatments over time
con_avg <- con %>% 
  dplyr::group_by(spp, water, temp, week) %>% 
  dplyr::summarise(c_mean = mean(con, na.rm = T), c_sd = sd(con, na.rm = T))

ggplot(filter(con_avg), aes(x = factor(week), y = c_mean, color = water))+
  annotate("rect", alpha = 0.5, xmin = 3.5, xmax = 4.5, ymin = 0, ymax = 500,
           fill = "orange")+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = c_mean - c_sd, ymax = c_mean + c_sd),
                width = 0.1, alpha = 0.7)+
  facet_wrap(~interaction(temp, spp), ncol = 2)+
  theme_light(base_size = 20)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))+
  labs(x = "Week", y = "Conductance (mmol/m2s)")

ggplot(filter(con), aes(x = day, y = con, color = water))+
  annotate("rect", alpha = 0.5, xmin = hw_days[1], xmax = hw_days[2], ymin = 0, ymax = 500,
           fill = "orange")+
  geom_point(size = 2, alpha = 0.5)+
  geom_vline(xintercept = 90, color = "gray50")+
  facet_wrap(~temp + spp, ncol = 4)+
  geom_smooth(alpha = 0.4, aes(group = interaction(sensor, water), fill = water))+
  theme_light(base_size = 20)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))+
  labs(x = "Day", y = "Conductance (mmol/m2s)", fill = "Water", color = "Water")

ggplot(filter(con), aes(x = day, y = con, color = water))+
  annotate("rect", alpha = 0.5, xmin = hw_days[1], xmax = hw_days[2], ymin = 0, ymax = 500,
           fill = "orange")+
  geom_point(size = 2, alpha = 0.5)+
  geom_vline(xintercept = 90, color = "gray50")+
  facet_wrap(~temp + spp, ncol = 4)+
  geom_smooth(alpha = 0.4, aes(group = interaction(water, sensor), fill = water))+
  theme_minimal(base_size = 20)+
  labs(x = "Day", y = "Conductance (mmol/m2s)", fill = "Water", color = "Water")



