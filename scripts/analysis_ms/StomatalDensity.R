library(tidyverse)

sd <- read_csv("data/Experiment/Raw/PIPO_SD.csv")
names(sd) <- c("TreeID", "Area_mm2", "Stomata_n", "SD")

stress_days <- read_csv("data/Experiment/Processed/Water_Limitation_Days.csv")


# death:
    death_days_fl <- read_csv("data/Experiment/Processed/Death_Day_Fl-Based.csv") %>% 
      filter(treatment == "drought") %>% 
      mutate(day = death) %>% 
      dplyr::select(TreeID, day)
    death_days_col <- read_csv("data/Experiment/Processed/Death_Day_Color-Based.csv")

# set to one of the above:
death_days <- death_days_col

  
sd <- sd %>% 
  group_by(TreeID) %>% 
  summarise(stom = mean(SD, na.rm = T))

hist(sd$stom, breaks = 10)

sd_mort <- inner_join(sd, death_days)

plot(sd_mort$stom, sd_mort$day)

sd_lim <- inner_join(sd, stress_days)

plot(sd_lim$stom, sd_lim$cpt)
