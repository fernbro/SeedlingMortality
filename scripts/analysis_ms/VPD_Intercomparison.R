library(tidyverse)

# soil water stress days:

stress <- read_csv("data/Experiment/Processed/Water_Limitation_Days.csv")

stress_stats <- stress %>% 
  group_by(spp, temp) %>%  # basically spp x chamber
  summarise(mean_lim = mean(cpt), med_lim = median(cpt)) %>% 
  mutate(hw = case_when(temp == "ambient" ~ F,
                        temp == "heatwave" ~ T)) %>% 
  dplyr::select(-temp)

# chamber data (aggregated to daily: vpd mean, max, and min in kPa):

chamber <- read_csv("data/Experiment/Processed/Kestrel_Dailys.csv") %>% 
  mutate(hw = case_when(chamber %in% c(1, 3) ~ F,
                        chamber %in% c(2, 4) ~ T))

chamber_pipo <- filter(chamber, set == "low elevation") %>% 
  mutate(spp = "PIPO")
chamber_psme <- filter(chamber, set == "low elevation") %>% 
  mutate(spp = "PSME")
chamber_pifl <- filter(chamber, set == "high elevation") %>% 
  mutate(spp = "PIFL")
chamber_pien <- filter(chamber, set == "high elevation") %>% 
  mutate(spp = "PIEN")

chamber <- rbind(chamber_pipo, chamber_psme, chamber_pifl, chamber_pien) %>% 
  mutate(vpd_sd = sd(vpd_m))

# provenance data (daily mean (1 half of vpdmin + vpdmax), min, and max in kPa):

prov <- read_csv("data/Provenance/Daily_Provenance_Climate.csv") %>% 
  dplyr::select(date, month, doy, vpdmin, vpdmax, vpd, spp)

# now we just want to pull the kestrel data from after the soil water stress times

c_s <- full_join(chamber, stress_stats, relationship = "many-to-many")

# post-water limitation:

(c_s_lim <- filter(c_s, day > mean_lim) %>% 
  group_by(spp) %>% 
  summarise(ch_vpd_avg = mean(vpd_m),
            ch_vpd_sd = sd(vpd_m)))

(prov_stats <- prov %>% 
  group_by(spp) %>% 
  filter(month %in% c(6)) %>% 
  summarise(irl_vpd_avg = mean(vpd),
            irl_vpd_sd = sd(vpd),
            vpd_cv = irl_vpd_sd/irl_vpd_avg))

vpd_comp <- full_join(c_s_lim, prov_stats) %>% 
  mutate(ch_irl_ratio = ch_vpd_avg/irl_vpd_avg)

ggplot(vpd_comp, aes(x = ch_vpd_avg, y = irl_vpd_avg))+
  geom_abline(slope = 1, intercept = 0, linetype = 1)+
  geom_abline(slope = 2, intercept = 0, linetype = 2)+
  geom_errorbar(aes(ymin = irl_vpd_avg - irl_vpd_sd, ymax = irl_vpd_avg + irl_vpd_sd))+
  geom_errorbar(aes(xmin = ch_vpd_avg - ch_vpd_sd, xmax = ch_vpd_avg + ch_vpd_sd))+
  geom_point(aes(fill = spp), pch = 21, size = 3)+
  labs(x = "Chamber mean daily VPD post-drydown (kPa)",
       y = "Provenance mean daily June VPD (kPa), 1984-2024")+
  theme_minimal(base_size = 20)
