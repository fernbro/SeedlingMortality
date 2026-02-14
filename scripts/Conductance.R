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
  dplyr::select(-textdate, -Fv_Fm_light, -VWC_perc) %>% 
  mutate(temp = case_when(id < 31 ~ "ambient",
                          id >= 31 ~ "heatwave"),
         water = case_when(TreeID %in% water ~ "water",
                           .default = "drought")) %>%
  full_join(dates) %>% 
  filter(con > 10, con < 500, !is.na(con)) %>% 
  mutate(day = case_when(year(date) == 2025 ~ yday(date)-202,
                         year(date) == 2026 ~ 365 - 202 + yday(date))) %>% 
  mutate(sensor = case_when(day < 90 ~ 1, day >= 90 ~ 2)) # Create a label for when the sensor changed and then can group the regressions/points by sensor

write_csv(con, "data/Experiment/Processed/Conductance.csv")

# determine effective 0 conductance:
# mean(c(84.4, 82, 80.1, 78.1)) # mean of values in mmol/m2s on Whatman Paper = 81.15 mmol/m2/s

# run analysis: when did each conductance time series consistently drop below 90?

# use rle function (base)

ind <- unique(filter(con, water == "drought")$TreeID)

closure <- data.frame(matrix(ncol = 0, nrow = 0))

for(i in 1:length(ind)){
  ID <- ind[i]
  
  con_id <- con %>% 
    filter(TreeID == ID) %>% 
    dplyr::select(con, day) %>% 
    arrange(day) %>% 
    filter(!is.na(con), !is.na(day))

  closed <- con_id$con <= 90  # create a logical vector to analyse runs (sequential strings of values) above or below this thresh
  
  runs <- rle(closed) # outputs each run length and whether it fit the threshold criteria
  
  run_starts <- cumsum(c(1, runs$lengths[-length(runs$lengths)]))
  # find the start of each run by cumulatively summing the lengths of the runs with the exception of the last run
  
  long_runs <- runs$lengths >= 2 & runs$values == TRUE # identifies where the runs were both at least 2 sequential values long and met the threshold crit
  run <- which(long_runs) # pulls the desired run by specifying if it fits the long_run criteria
  
  if(length(run) > 0) {
    # Get the very last run that qualified
    last_run_index <- tail(run, 1)
    
    # Extract the start position and the actual time value
    start_pos <- run_starts[last_run_index] # extract the start index of the last run
    crossing_time <- con_id$day[start_pos] # indexes the day from the original (subsetted) conductance df
    
  } else {
    crossing_time <- NA # if there are no qualifying runs, set NA; but this shouldn't apply..
  }
  
  crossing <- data.frame(crossing_time, ID)
  colnames(crossing) <- c("day_closed", "TreeID")
  
  closure <- rbind(crossing, closure)
}


# write_csv(closure, "data/Experiment/Processed/Stomata_Closure_Day.csv")

con_clo <- full_join(con, closure)

ID <- "PIFL43";ggplot(filter(con_clo, TreeID == ID), aes(x = day, y = con))+
  geom_line(color = "red")+
  geom_point()+
  # geom_smooth()+
  geom_hline(yintercept = 90, linetype = 3)+
  geom_vline(aes(xintercept = day_closed), linetype = 4)+
  theme_minimal()




# names(closure) <- "day_closed"
# closure$TreeID <- ind

# we now have a dataframe called "closure" that has a day of permanent stomatal closure for each droughted plant!



###### 
con_mod <- lm(log(con) ~ spp*day + temp*day + water*day, data = con)
anova(con_mod)
summary(con_mod)


library(emmeans)

emmip(con_mod, spp ~ day | water,
      at = list(day = c(25, 50, 75, 100, 125, 150)),
      type = "response", CIs = T)+
  facet_wrap(~water, scales = "free_x")+
  geom_hline(yintercept = 90, linetype = 2, alpha = 0.6)+
  labs(y = "Conductance (mmol/m2/s)", x = "Day")+
  theme_minimal()


# plots:

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

anova(lm(con ~ spp + day + temp + water, data = con)) # no significant effects of temperature for whole time..
anova(lm(con ~ spp + temp + water, 
         data = filter(con, day >= hw_days[1] & day <= hw_days[2]))) # or for just during the heatwave

######

ggplot(data = filter(con), aes(x = week, y = (con), group = interaction(spp, temp, water)))+
  geom_boxplot(alpha = 0.7, aes(group = interaction(date, water), fill = water))+
  facet_wrap(~interaction(spp, temp), nrow = 4, scales = "free_y")+
  theme_light(base_size = 20)+
  labs(x = "Week", y = "Foliar conductance (mmol/m2s)")

ggplot(data = filter(con, temp == "heatwave"), aes(x = day, y = (con), group = interaction(spp, temp, water)))+
  geom_hline(yintercept = 90)+
  xlim(c(14,46))+
  # geom_boxplot(alpha = 0.7, aes(group = interaction(date, temp), fill = temp))+
  geom_point(alpha = 0.7, size = 3,
             aes(color = water))+
  # geom_smooth(method = "lm", aes(fill = temp))+
  geom_line(aes(group = TreeID, color = water), alpha = 0.4)+
  # scale_color_manual(values = hw_colors)+
  facet_wrap(~spp, nrow = 2)+
  theme_minimal(base_size = 20)+
  labs(x = "Day", y = "Conductance (mmol/m2s)")

# avgs by spp and treatments over time
con_avg <- con %>% 
  dplyr::group_by(spp, water, temp, day, week) %>% 
  dplyr::summarise(c_mean = mean(con, na.rm = T), c_sd = sd(con, na.rm = T))

ggplot(filter(con_avg, day > 10, day < 30), aes(x = (week), y = c_mean, color = water))+
  # annotate("rect", alpha = 0.5, xmin = 3.5, xmax = 4.5,
  #          ymin = 0, ymax = 500,
  #          fill = "orange")+
  geom_point(size = 3, aes(shape = temp), position = position_dodge(width = 0.1))+
  geom_line(aes(group = interaction(temp, water), linetype = temp), position = position_dodge(width = 0.1))+
  geom_errorbar(aes(ymin = c_mean - 2*c_sd, ymax = c_mean + 2*c_sd),
                width = 0.1, alpha = 0.7,
                position = position_dodge(width = 0.1))+
  facet_wrap(~spp, ncol = 2)+
  theme_minimal(base_size = 20)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))+
  labs(x = "Week", y = "Conductance (mmol/m2s)")





# faceted by species x temp:
ggplot(filter(con), aes(x = day, y = con, color = water))+
  annotate("rect", alpha = 0.5, xmin = hw_days[1], xmax = hw_days[2], ymin = 0, ymax = 500,
           fill = "orange")+
  geom_point(size = 2, alpha = 0.5)+
  geom_vline(xintercept = 90, color = "gray50")+
  facet_wrap(~temp + spp, ncol = 4, scales = "free_x")+
  geom_smooth(alpha = 0.4, aes(group = interaction(sensor, water), fill = water), method = "gam",
              formula = y ~ s(x, bs = "cs", k = 4))+ # set k (# of knots) parameter so you dont get an error for small samples
              # in this format, the number of knots has to be the same for each group i think
  theme_light(base_size = 20)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))+
  labs(x = "Day", y = "Conductance (mmol/m2s)", fill = "Water", color = "Water")


# only droughted plants faceted by spp:
ggplot(filter(con, water == "drought"), aes(x = day, y = con, color = temp))+
  annotate("rect", alpha = 0.5, xmin = hw_days[1], xmax = hw_days[2], ymin = 0, ymax = 500,
           fill = "orange")+
  geom_point(size = 2, alpha = 0.5)+
  geom_vline(xintercept = 90, color = "gray50")+
  facet_wrap(~spp, ncol = 4, scales = "free_x")+
  geom_smooth(alpha = 0.4, aes(group = interaction(sensor, temp), fill = temp), method = "gam",
              formula = y ~ s(x, bs = "cs", k = 4))+ # set k (# of knots) parameter so you dont get an error for small samples
  # in this format, the number of knots has to be the same for each group i think
  theme_light(base_size = 20)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))+
  labs(x = "Day", y = "Conductance (mmol/m2s)", fill = "Temp", color = "Temp")




ggplot(filter(con), aes(x = day, y = con, color = water))+
  annotate("rect", alpha = 0.5, xmin = hw_days[1], xmax = hw_days[2], ymin = 0, ymax = 500,
           fill = "orange")+
  geom_point(size = 2, alpha = 0.5)+
  geom_vline(xintercept = 90, color = "gray50")+
  facet_wrap(~temp + spp, ncol = 4)+
  geom_smooth(alpha = 0.4, aes(group = interaction(sensor, water, temp), fill = water), method = "gam",
              formula = y ~ s(x, bs = "cs", k = 4))+
  theme_minimal(base_size = 20)+
  labs(x = "Day", y = "Conductance (mmol/m2s)", fill = "Water", color = "Water")



