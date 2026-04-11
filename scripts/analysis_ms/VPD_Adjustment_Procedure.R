library(tidyverse)

# temperature relationship:

temp <- seq(0, 8, 0.5)
delta_mort <- -5.5*temp

temp_mort <- data.frame(temp, delta_mort)

ggplot(temp_mort, aes(x = temp, y = delta_mort))+
  geom_line(color = "red", linewidth = 2)+
  theme_minimal(base_size = 20)+
  labs(x = "Temp above ambient (ºC)",
       y = "% change in time to mortality",
       title = "PIED & PIPO relationship",
       subtitle = "Adams et al. 2017 ERL")

# read in climate data:

insitu <- read_csv("data/Provenance/Daily_Prov_Climate.csv")

ggplot(insitu, aes(x = tmean, y = vpd))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~spp)

# SVP equation:

calc_svp <- function(air_temp){
  svp <- (0.611 * exp(17.27*air_temp/(237.3 + air_temp)))
  return(svp)
} # looks good! comparable with NOAA online calculator

# now calculate SVP for the entire PRISM dataset

insitu <- insitu %>% 
  mutate(svp_mean = calc_svp(tmean)) %>% 
  mutate(avp_mean = svp_mean - vpd) %>% 
  filter(avp_mean > 0)

hist(insitu$avp_mean)

ggplot(insitu, aes(x = tmean))+
  geom_point(aes(y = avp_mean), pch = 1, alpha = 0.4)+
  geom_line(aes(y = svp_mean), color = "red")+
  facet_wrap(~spp)+
  theme_minimal(base_size = 20)+
  labs(x = "Daily mean temp (ºC)",
       y = "Vapor pressure")

ggplot(insitu, aes(x = tmean))+
  geom_bin2d(aes(y = avp_mean), binwidth = c(0.5, 0.1))+
  geom_line(aes(y = svp_mean), color = "red")+
  facet_wrap(~spp)+
  theme_minimal(base_size = 20)+
  scale_fill_continuous(type = "viridis")+
  labs(x = "Daily mean temp (ºC)",
       y = "Vapor pressure")

# we need to know: on average, how much does a change in T change VPD? in June?

ggplot(filter(insitu, month(date)==6), aes(x = tmean, y = vpd))+
  geom_point(aes(color = spp), pch = 1)+
  # facet_wrap(~spp)+
  geom_smooth(method = "lm", se = T)+
  geom_smooth(se = T, color = "red")+
  theme_minimal(base_size = 20)+
  labs(x = "Mean daily temperature (ºC)", color = "Species",
       y = "Mean daily VPD (kPa)",
       title = "June VPD v. temp, 1984-2024")

library(mgcv)
library(gratia)

vpd_gam <- (gam(vpd ~ s(tmean),
  data = filter(insitu, month(date)==6)))
gratia::basis(vpd_gam)

summary(lm(vpd ~ tmean, filter(insitu, month(date)==6)))
# for june, a one-unit change in temperature led to an average VPD increase of
# OVERALL: 0.125 (SE: 0.0011)
# PIPO: 0.139
# PSME: 0.135
# PIFL: 0.124
# PIEN: 0.111
# ... kilopascals

# what was the vpd in our experiment?

# read in experimental "climate" (chamber) data:
chamber <- read_csv("data/Experiment/Processed/Kestrel_Dailys.csv") %>% 
  mutate(hw = case_when(chamber %in% c(1, 3) ~ F,
                        chamber %in% c(2, 4) ~ T))

# spp x el groups:
spp_el <- data.frame(spp = c("PIPO", "PSME", "PIFL", "PIEN"),
                     set = c("low elevation", "low elevation",
                             "high elevation", "high elevation"))

chamber_stats <- chamber %>% 
  group_by(set) %>% 
  filter(day >= 0) %>% 
  summarise(vpd_cham = mean(vpd_m)) %>% 
  inner_join(spp_el) %>% 
  dplyr::select(vpd_cham, spp)

# figure out target VPDs:
insitu_stats <- insitu %>% 
  group_by(spp) %>% 
  filter(month(date) == 6) %>% 
  summarise(vpd_situ = mean(vpd))

cham_situ <- full_join(chamber_stats, insitu_stats)

# let's just go with 0.125 as the overall VPD increase per T increase
# we could increase VPD by increasing temps, and each 1 ºC increase should
# increase VPD by 0.125 kPa in the field
# so, how many º C do we have to add to achieve the appropriate increase in VPD?

cham_situ <- cham_situ %>% 
  mutate(temp_to_increase = (vpd_situ - vpd_cham)/0.125) # ∆VPD divided by rate of VPD increase with T

# now apply the -5.5% decrease in mortality per temp C raise:
cham_situ <- cham_situ %>%
  mutate(change_to_mort = -5.5*temp_to_increase) # multiply the temp increase by the ∆time to mortality per ºC rate

mean(cham_situ$change_to_mort) # average decrease in time to mortality of ~33%
select(cham_situ, spp, change_to_mort)



# alternative method to try:

# calculate actual vapor pressure in the chambers
# calculate what the temperature would have to be at that vapor pressure to achieve a field-accurate VPD
# adjust the temperatures this way to achieve the reduction in mortality.



