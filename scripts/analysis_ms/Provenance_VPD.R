library(tidyverse)
options(digits = 10)
library(RcppRoll)
# install.packages("climatrends")
library(climatrends)

# DAILY data acquired from PRISM explorer (https://prism.oregonstate.edu/explorer/) on Thursday, June 19th, 2025
# see acquisition parameters in original CSVs

#Ponderosa Pine (PIPO)
pipo <- read_csv("data/Provenance/PIPO_Daily.csv", skip = 10)
pipo_ppt <- read_csv("data/Provenance/PIPO_Daily_PPT.csv", skip = 10)[,"ppt (mm)"]
pipo <- cbind(pipo, pipo_ppt)
#names(pipo) <- pipo[1,1:4]
#pipo <- pipo[-1,]
pipo$spp <- "PIPO"

#Limber Pine (PIFL)
pifl <- read_csv("data/Provenance/PIFL_Daily.csv", skip = 10)
pifl_ppt <- read_csv("data/Provenance/PIFL_Daily_PPT.csv", skip = 10)[,"ppt (mm)"]
pifl <- cbind(pifl, pifl_ppt)
# # names(pifl) <- pifl[1,1:4]
# pifl <- pifl[-1,]
pifl$spp <- "PIFL"

#Douglas Fir (PSME)
psme <- read_csv("data/Provenance/PSME_Daily.csv", skip = 10)
psme_ppt <- read_csv("data/Provenance/PSME_Daily_PPT.csv", skip = 10)[,"ppt (mm)"]
psme <- cbind(psme, psme_ppt)
# names(psme) <- psme[1,1:4]
# psme <- psme[-1,]
psme$spp <- "PSME"

#Engelmann Spruce (PIEN)
pien <- read_csv("data/Provenance/PIEN_Daily.csv", skip = 10)
pien_ppt <- read_csv("data/Provenance/PIEN_Daily_PPT.csv", skip = 10)[,"ppt (mm)"]
pien <- cbind(pien, pien_ppt)
# names(pien) <- pien[1,1:4]
# pien <- pien[-1,]
pien$spp <- "PIEN"

prov_clim <- rbind(pipo, pifl, psme, pien) %>% 
  mutate(doy = yday(Date),
    year = as.numeric(substr(Date, 1, 4)),
    month = month(Date)) %>% 
  filter(year != 2024) # incomplete data
names(prov_clim) <- c("date", "tmin", "tmean", "tmax", "vpdmin", "vpdmax", "ppt",
                      "spp", "doy", "year", "month")


prov_clim <- prov_clim %>% 
  mutate(vpdmax = vpdmax/10, vpdmin = vpdmin/10) %>% 
  mutate(vpd = (vpdmax+vpdmin)/2)
# prov_clim <- prov_clim %>% 
#   mutate(tmean = as.numeric(tmean))

# how to check for days < 5mm?

rainfall(filter(prov_clim, spp == "PIPO")$ppt)
rainfall(filter(prov_clim, spp == "PSME")$ppt)
rainfall(filter(prov_clim, spp == "PIEN")$ppt)
rainfall(filter(prov_clim, spp == "PIFL")$ppt)

# lets get the day of peak VPD and day of first (daily) PPT >= 1

prov_jjas_vpd <- prov_clim %>% 
  filter(month %in% c(6, 7, 8, 9)) %>%
  # filter(month == 6) %>%# june, july, august, september for a broad window of monsoon arrival
  group_by(year, spp) %>% 
  arrange(doy) %>% 
  reframe(peak_vpd = max(vpd),
          peak_vpd_day = doy[which(vpd == peak_vpd)])

peak_of_vpd <- prov_clim %>% 
  # filter(month >= 6) %>%
  group_by(year, spp) %>% 
  arrange(doy) %>% 
  reframe(peak_vpd = max(vpd),
          peak_vpd_day = doy[which(vpd == peak_vpd)])

# Higgins et al. 1997: "date when The onset is defined as 
# the date when accumulated precipitation above 0.5 mm persists for 3 days after 1 June"
# let's compute the average daily for our new climatological period?

daily_avg <- prov_clim %>% 
  group_by(spp) %>% 
  # dplyr::select(ppt) %>% 
  summarise(mean_daily = mean(ppt))
# so now our daily "threshold" will be 1.56 mm
# can also assign species-specific thresholds?

monsoon_start <- prov_clim %>% 
  filter(doy < 250 & month >= 6) %>% 
  dplyr::select(spp, year, doy, ppt) %>% 
  arrange(doy) %>% 
  mutate(ppt_thresh = case_when(ppt >= 1.56 ~ 1, # insert the threshold here
                                ppt < 1.56 ~ 0)) %>% 
  group_by(year, spp) %>% 
  mutate(wet_3days = roll_sum(x = ppt_thresh, n = 3,
                               align = "left", 
                               fill = NA)) %>% 
  filter(wet_3days == 3) %>% 
  filter(doy == min(doy)) %>% 
  ungroup() %>% 
  dplyr::select(spp, year, doy)
names(monsoon_start) <- c("spp", "year", "start_day")

ggplot(monsoon_start, aes(x = year, y = start_day))+
  geom_point(aes(color = spp))+
  geom_smooth()

start_vs_peak <- full_join(peak_of_vpd, monsoon_start) %>% 
  mutate(diff = start_day - peak_vpd_day)

diffs <- start_vs_peak %>% 
  group_by(spp) %>% 
  summarise(mean_diff = mean(diff),
            med_diff = median(diff))

yrly_ppt <- prov_clim %>% 
  group_by(spp, year) %>% 
  summarise(ppt = sum(ppt))
  

# positive diff: the start day was later than the peak VPD day
# negative diff: the start day was before the peak VPD day

ggplot(start_vs_peak, aes(x = peak_vpd_day, y = start_day))+
  geom_point(aes(color = spp))+
  geom_abline(slope = 1, intercept = 0)

start_vs_peak <- full_join(start_vs_peak, yrly_ppt)

starts_stats <- start_vs_peak %>% 
  group_by(spp) %>% 
  summarise(mean_peak_day = mean(peak_vpd_day),
            mean_start_day = mean(start_day),
            med_peak_day = median(peak_vpd_day),
            med_start_day = median(start_day))

ggplot(start_vs_peak, aes(x = year, y = diff))+
  geom_point(aes())+
  geom_hline(yintercept = 0)+
  geom_smooth()+
  # geom_smooth(method = "lm")+
  facet_wrap(~spp, scales = "free")+
  theme_minimal(base_size = 20)+
  labs(x = "Year", y = "Days post-peak VPD before monsoon onset")
  

  # filter(ppt >= 0.5)

      # defining rainy season onset?
      # from 10.1002/joc.6264:
      # The MR14 method defines onset as: (a) the first wet day
      # (≥ 1 mm) of (b) the first 5-day period with average rainfall
      # equal or larger than the climatological 5-day wet spell for
      # April–October and (c) without a 10-day dry spell with precipitation 
      # below 5 mm during the following 30 days.
      
      # This definition includes five parameters: the
      # amount of rainfall of the first and last wet spell, its duration,
      # the duration and intensity of the post-onset and pre-demise
      # dry spells, and the length of the period for which these dry
      # spells are searched
      
      # steps:
      # identify 5-day periods where the running cumulative sum of precipitation 
      # is greater than or equal to the mean 5-day accumulation from april-october
      # select the first day of the earliest 5-day period, then make sure there are 
      # no 10 consecutive days where ppt doesn't sum to at least 5mm within the next 30 days.
      # iterate thru the qualifying 5-day periods until you have the earliest one with no subsequent dry spells
      
# years <- unique(prov_clim$year)
# spp <- unique(prov_clim$spp)
# days <- seq(1, 366, by = 1)
# 
# # compute "wet spell" climatologies
# wet_sums <- prov_clim %>% 
#   filter(month >= 4 & month <= 10) %>% 
#   dplyr::select(spp, doy, ppt, year) %>% 
#   group_by(spp, year) %>% 
#   arrange(doy) %>% 
#   mutate(ppt_5day = roll_sum(ppt, n = 5, align = "right", fill = NA)) %>% 
#   filter(!is.na(ppt_5day)) %>% 
#   ungroup()
# 
# climatologies <- wet_sums %>% 
#   group_by(spp) %>% 
#   summarise(wet_avg = mean(ppt_5day))
# 
# for(j in 1:length(spp)){
#   for(i in 1:length(years)){
#   
#   prov_sub <- filter(prov_clim, 
#                      year == years[i] & spp == spp[j]) %>% 
#     filter(month >= 4 & month <= 10) %>% 
#     dplyr::select(spp, doy, ppt) %>% 
#     arrange(doy) %>% 
#     mutate(ppt_5day = roll_sum(ppt, n = 5, align = "right", fill = NA), # preceding 5 days (incl. day-of)
#            ppt_10day = roll_sum(ppt, n = 10, align = "left", fill = NA)) # succeeding 10 days (incl. day-of)
#   
#   # provide T or F for if days have the 5-day wet period threshold
#   # provide T or F for if 10-day sums are at least 5mm
#   qual_period <- prov_sub %>% 
#     mutate(wet_pd = case_when(ppt_5day >= filter(climatologies, spp == spp[j])$wet_avg ~ T,
#                               ppt_5day < filter(climatologies, spp == spp[j])$wet_avg ~ F,),
#            at_least_5 = case_when(ppt_10day >= 5 ~ 1,
#                                   ppt_10day < 5 ~ 0))
#   
#   # select days where the 5-day wet period is met and there are no falses in the following 30 days
#   quals <- qual_period %>% 
#     arrange(doy) %>% 
#     mutate(wet_score = roll_sum(at_least_5, n = 10, align = "left", fill = NA, na.rm = T))
#   
#   }
# }

#rollapply(.$at_least_5, width = 10, FUN = isTRUE, align = "left", fill = NA)


# first day where ppt > 5 mm:
prov_jjas_ppt <- prov_clim %>% 
  filter(month >= 6) %>%
  dplyr::select(spp, year, doy, ppt) %>% 
  filter(ppt >= 5) %>% 
  group_by(year, spp) %>% 
  reframe(first_ppt_day = min(doy))

ppt_vpd <- full_join(prov_jjas_vpd, prov_jjas_ppt) %>%
  mutate(diff = first_ppt_day - peak_vpd_day)

ggplot(ppt_vpd, aes(x = peak_vpd_day, y = first_ppt_day))+
  geom_point(aes(color = year))+
  geom_abline(slope = 1, intercept = 0)+
  facet_wrap(~spp)

ggplot(ppt_vpd, aes(x = peak_vpd, y = peak_vpd_day))+
  geom_point(aes(color = year))+
  geom_smooth(method = "lm")+
  facet_wrap(~spp)

ggplot(ppt_vpd, aes(x = year, y = diff))+
  geom_point(aes(color = spp))+
  geom_hline(yintercept = 0)+
  geom_smooth(method = "lm")+
  facet_wrap(~spp)

ggplot(ppt_vpd, aes(x = year, y = diff))+
  geom_point(aes(color = spp))+
  geom_hline(yintercept = 0)+
  geom_smooth()

ggplot(prov_jja, aes(x = year, y = peak_vpd_day))+
  geom_point(aes(color = spp))+
  geom_smooth()

# avg_clim <- prov_clim %>% 
#   group_by(year) %>% 
#   summarise(temp = mean(tmean))

# let's get some z-scores

ggplot(filter(prov_clim), aes(x = month, y = ppt))+
  geom_boxplot(aes(group = month))+
  facet_wrap(~spp)

pien6 <- filter(prov_clim, month == 6 & spp == "PIEN")
pifl6 <- filter(prov_clim, month == 6 & spp == "PIFL")
pipo6 <- filter(prov_clim, month == 6 & spp == "PIPO")
psme6 <- filter(prov_clim, month == 6 & spp == "PSME")

# mat <- prov_clim %>% 
#   group_by(spp) %>% 
#   summarise(mat = mean(tmean, na.rm = T))

loel6 <- rbind(pipo6, psme6)
hiel6 <- rbind(pien6, pifl6)

#Max:
ggplot(filter(prov_clim, month == 6))+
  # \geom_boxplot(aes(x = spp, y = tmin, fill = spp), alpha = 0.5)+
  geom_boxplot(aes(x = spp, y = tmax, fill = spp))+
  geom_hline(aes(yintercept = mean(loel6$tmax, na.rm = T)), color = "red")+
  geom_hline(aes(yintercept = mean(hiel6$tmax, na.rm = T)), color = "blue")+
  geom_hline(aes(yintercept = median(loel6$tmax, na.rm = T)), color = "red", linetype = 2)+
  geom_hline(aes(yintercept = median(hiel6$tmax, na.rm = T)), color = "blue", linetype = 2)+
  theme_light(base_size = 23)+
  labs(x = "Species",
       y = "June daily maximum temperatures (ºC), 1984 - 2024", fill = "Species")


ggplot(filter(prov_clim, month == 6))+
  # \geom_boxplot(aes(x = spp, y = tmin, fill = spp), alpha = 0.5)+
  geom_boxplot(aes(x = spp, y = vpdmax, fill = spp))+
  theme_light(base_size = 23)+
  labs(x = "Species",
       y = "June max VPD (kPa), 1984 - 2024", fill = "Species")

ggplot(filter(prov_clim, month == 6))+
  # \geom_boxplot(aes(x = spp, y = tmin, fill = spp), alpha = 0.5)+
  geom_boxplot(aes(x = spp, y = vpd, fill = spp))+
  theme_light(base_size = 23)+
  labs(x = "Species",
       y = "June mean daily VPD (kPa), 1984 - 2024", fill = "Species")

ggplot(filter(prov_clim, month == 6))+
  geom_density(aes(x = vpd, fill = spp), alpha = 0.3)+
  theme_light(base_size = 23)+
  labs(x = "VPD (kPa)",
       y = "June mean daily VPD (kPa), 1984 - 2024", fill = "Species")
ggplot(filter(prov_clim, month == 7))+
  geom_density(aes(x = vpd, fill = spp), alpha = 0.3)+
  theme_light(base_size = 23)+
  labs(x = "July mean daily VPD (kPa), 1984 - 2024", fill = "Species")

# create a range of monthly VPD curves
# remember: spp corresponds to location

prov_vpd_jun <- prov_clim %>% 
  filter(month == 6) %>% 
  group_by(spp, doy) %>% 
  summarise(vpdmax_mean = mean(vpdmax, na.rm = T), 
            vpdmin_mean = mean(vpdmin, na.rm = T))

prov_vpd_jul <- prov_clim %>% 
  # filter(month == 7) %>% 
  group_by(spp, doy) %>% 
  summarise(vpdmax_mean = mean(vpdmax, na.rm = T), 
            vpdmin_mean = mean(vpdmin, na.rm = T),
            vpdmax_sd = sd(vpdmax, na.rm = T), 
            vpdmin_sd = sd(vpdmin, na.rm = T),
            ppt_mean = mean(ppt, na.rm = T),
            ppt_sd = sd(ppt, na.rm = T)) %>%
  mutate(vpd_mean = vpdmax_mean - vpdmin_mean,
         vpd_sd = sd(vpd_mean))

prov_mo <- prov_clim %>% 
  # filter(month == 7) %>% 
  group_by(spp, month) %>% 
  summarise(vpdmax_mean = mean(vpdmax, na.rm = T), 
            vpdmin_mean = mean(vpdmin, na.rm = T),
            vpdmax_sd = sd(vpdmax, na.rm = T), 
            vpdmin_sd = sd(vpdmin, na.rm = T),
            ppt_mean = mean(ppt, na.rm = T),
            ppt_sd = sd(ppt, na.rm = T)) %>%
  mutate(vpd_mean = vpdmax_mean - vpdmin_mean,
         vpd_sd = sd(vpd_mean))

ggplot(prov_vpd_jul, aes(x = doy, y = vpdmax_mean))+
  geom_point(aes(color = spp))+
  facet_wrap(~spp)+
  geom_errorbar(aes(color = spp,
                    ymax = vpdmax_mean + vpdmax_sd, 
                    ymin = vpdmax_mean - vpdmax_sd))+
  geom_vline(xintercept = 166)

ggplot(prov_vpd_jul)+
  geom_line(aes(x = doy, y = vpd_mean, color = spp))+
  # geom_col(aes(x = doy, y = ppt_mean), position = "identity")+
  geom_line(aes(x = doy, y = ppt_mean))+
  facet_wrap(~spp)+
  # geom_vline(xintercept = 166)+
  geom_ribbon(alpha = 0.4,
    aes(fill = spp, x = doy, y = vpd_mean,
                    ymax = vpdmax_mean + vpdmax_sd, 
                    ymin = vpdmax_mean - vpdmax_sd))+
geom_ribbon(alpha = 0.4,
            aes(fill = spp, x = doy, y = ppt_mean,
                ymax = ppt_mean + ppt_sd, 
                ymin = 0))

ggplot(prov_mo, aes(x = month, y = vpd_mean))+
  geom_line(aes(color = spp))

# compare mean date of Peak VPD with mean date of first ppt != 0?
# what is the window..?



