library(tidyverse)
options(digits = 10)


# DAILY data acquired from PRISM explorer (https://prism.oregonstate.edu/explorer/) on Thursday, June 19th, 2025
# see acquisition parameters in original CSVs

#Ponderosa Pine (PIPO)
pipo <- read_csv("data/Provenance/PIPO_Daily.csv", skip = 10)
#names(pipo) <- pipo[1,1:4]
#pipo <- pipo[-1,]
pipo$spp <- "PIPO"

#Limber Pine (PIFL)
pifl <- read_csv("data/Provenance/PIFL_Daily.csv", skip = 10)
# # names(pifl) <- pifl[1,1:4]
# pifl <- pifl[-1,]
pifl$spp <- "PIFL"

#Douglas Fir (PSME)
psme <- read_csv("data/Provenance/PSME_Daily.csv", skip = 10)
# names(psme) <- psme[1,1:4]
# psme <- psme[-1,]
psme$spp <- "PSME"

#Engelmann Spruce (PIEN)
pien <- read_csv("data/Provenance/PIEN_Daily.csv", skip = 10)
# names(pien) <- pien[1,1:4]
# pien <- pien[-1,]
pien$spp <- "PIEN"

prov_clim <- rbind(pipo, pifl, psme, pien) %>% 
  mutate(doy = yday(Date),
    year = as.numeric(substr(Date, 1, 4)),
    month = month(Date))
names(prov_clim) <- c("date", "tmin", "tmean", "tmax", "vpdmin", "vpdmax",
                      "spp", "doy", "year", "month")
# prov_clim <- prov_clim %>% 
#   mutate(tmean = as.numeric(tmean))

# avg_clim <- prov_clim %>% 
#   group_by(year) %>% 
#   summarise(temp = mean(tmean))

# let's get some z-scores

ggplot(filter(prov_clim, month == 6), aes(x = spp, y = tmax))+
  geom_boxplot()

pien6 <- filter(prov_clim, month == 6 & spp == "PIEN")
pifl6 <- filter(prov_clim, month == 6 & spp == "PIFL")
pipo6 <- filter(prov_clim, month == 6 & spp == "PIPO")
psme6 <- filter(prov_clim, month == 6 & spp == "PSME")

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
# send this to don & dave

#Min:
ggplot(filter(prov_clim, month == 6))+
  # \geom_boxplot(aes(x = spp, y = tmin, fill = spp), alpha = 0.5)+
  geom_boxplot(aes(x = spp, y = tmin, fill = spp))+
  geom_hline(aes(yintercept = mean(loel6$tmin, na.rm = T)), color = "red")+
  geom_hline(aes(yintercept = mean(hiel6$tmin, na.rm = T)), color = "blue")+
  geom_hline(aes(yintercept = median(loel6$tmin, na.rm = T)), color = "red", linetype = 2)+
  geom_hline(aes(yintercept = median(hiel6$tmin, na.rm = T)), color = "blue", linetype = 2)+
  theme_light(base_size = 23)+
  labs(x = "Species",
    y = "June daily minimum temperatures (ºC), 1984 - 2024", fill = "Species")


# PIPO & PSME
mean(loel6$tmax, na.rm = T) # 25.39 (25)
median(loel6$tmax, na.rm = T) # 25.39 (25)
mean(loel6$tmin, na.rm = T) # 9.15 (9)
median(loel6$tmin, na.rm = T) # 9.15 (9)

# PIEN & PIFL
mean(hiel6$tmax, na.rm = T) # 22.03 (32)
median(hiel6$tmax, na.rm = T) # 23 (25)
mean(hiel6$tmin, na.rm = T) # 6.79 (17)
median(hiel6$tmin, na.rm = T) # 9.15 (9)






june_min_mean <- prov_clim %>% 
  filter(month == 6) %>% 
  group_by(spp) %>% 
  summarise(tmin_mean = mean(tmin))
june_min_median <- prov_clim %>% 
  filter(month == 6) %>% 
  group_by(spp) %>% 
  summarise(tmin_med = median(tmin))

# ggplot()+
#   geom_point(data = june_min_mean, aes(x = spp, y = tmin_mean))+
#   geom_point(data = june_min_median, aes(x = spp, y = tmin_med))


prov_z_june <- prov_clim %>% 
  group_by(spp) %>% 
  filter(month == 6) %>% 
  summarise(tmax_mean = mean(tmax, na.rm = T), tmax_sd = sd(tmax, na.rm = T),
            n = n()) %>% 
  ungroup() %>% 
  mutate(tmax_lo = tmax_mean - 2*tmax_sd,
         tmax_hi = tmax_mean + 2*tmax_sd,
         tmax_se = tmax_sd/sqrt(n))

ggplot(prov_z_june, aes(x = spp, y = tmax_mean))+
  geom_point()+
  geom_errorbar(aes(ymin = tmax_mean - tmax_sd, ymax = tmax_mean + tmax_sd), width = 0.2)+
  geom_errorbar(aes(ymin = tmax_mean - 2*tmax_se, ymax = tmax_mean + 2*tmax_se), width = 0.2, color = "orange")+
  geom_hline(yintercept = mean(prov_z_june$tmax_mean), color = "blue")+
  labs(x = "Species", y = "Mean max daily temp in June (ºC) ± SD (± 2 SE)")+
  theme_light(base_size = 26)

mean(prov_z_june$tmax_mean)

TukeyHSD(aov(tmax ~ spp, data = filter(prov_clim, month == 6)), conf.level = 0.95, na.rm = T)


ggplot(prov_clim, aes(x = doy, y = tmax))+
  geom_point(aes(group = year, color = spp), alpha = 0.2)+
  geom_smooth(aes(group = spp, color = spp))+
  geom_vline(xintercept = 126)
ggplot(prov_clim, aes(x = doy, y = vpdmax))+
  geom_point(aes(group = year, color = spp), alpha = 0.2)+
  geom_smooth(aes(group = spp, color = spp))

prov_clim_z <- prov_clim %>% 
  filter(month == 6) %>% 
  full_join(prov_z_june, by = join_by(spp)) %>% 
  mutate(tmax_z = (tmax - tmax_mean)/tmax_sd) %>% 
  select(spp, date, year, doy, tmax_z)

ggplot(prov_clim_z, aes(x = doy, y = tmax_z, group = interaction(spp, year)))+
  geom_line(aes(color = spp), alpha = 0.1)


prov_doy_stats <- prov_clim %>% 
  group_by(spp, doy) %>% 
  summarise(tmax_mean = mean(tmax, na.rm = T), tmax_sd = sd(tmax, na.rm = T))

ggplot(na.omit(prov_doy_stats), aes(x = doy, y = tmax_mean, color = spp))+
  geom_ribbon(aes(ymin = tmax_mean - tmax_sd, ymax = tmax_mean + tmax_sd, fill = spp),
              alpha = 0.3)+
  geom_line(linewidth = 1.5)

# ggplot(prov_clim, aes(x = spp, y = tmax))+
#   geom_boxplot(aes(fill = spp))+
#   geom_hline(yintercept = mean(prov_clim$tmax), color = "red", linetype = 2)+
#   labs(x = "Species", y = "Max June Temp (ºC): 1980 - 2024",
#        fill = "Species")+
#   theme_light(base_size = 20)

ggplot(prov_clim, aes(x = spp, y = tmax))+
  geom_boxplot(aes(fill = spp))+
  geom_hline(yintercept = mean(prov_clim$tmax), color = "red", linetype = 2)+
  labs(x = "Species", y = "Max Daily Temp (ºC): 1984 - 2024",
       fill = "Species")+
  theme_light(base_size = 20)

ggplot(filter(prov_clim, month(date) == 6), aes(x = spp, y = tmax))+
  geom_boxplot(aes(fill = spp))+
  geom_hline(yintercept = mean(filter(prov_clim, month(date) == 6)$tmax), color = "red", linetype = 2)+
  labs(x = "Species", y = "Max Daily Temp in June (ºC): 1984 - 2024",
       fill = "Species")+
  theme_light(base_size = 20)

ggplot(filter(prov_clim, month %in% c(6)), aes(x = tmax))+
  geom_density(aes(color = spp), alpha = 0.4)+
  geom_vline(xintercept = median(filter(prov_clim, month == 6 & spp == "PIEN")$tmax), color = "red", linetype = 2)+
  geom_vline(xintercept = median(filter(prov_clim, month == 6 & spp == "PIFL")$tmax), color = "green4", linetype = 2)+
  geom_vline(xintercept = median(filter(prov_clim, month == 6 & spp == "PIPO")$tmax), color = "turquoise2", linetype = 2)+
  geom_vline(xintercept = median(filter(prov_clim, month == 6 & spp == "PSME")$tmax), color = "purple", linetype = 2)+
  labs(x = "Max Daily Temp (ºC): 1984 - 2024",
       color = "Species")+
  theme_light(base_size = 20)

ggplot(prov_clim, aes(x = tmax))+
  geom_histogram(aes(fill = spp), alpha = 0.4, binwidth = 0.5)+
  #geom_vline(yintercept = mean(prov_clim$tmax), color = "red", linetype = 2)+
  labs(x = "Max Daily Temp (ºC): 1984 - 2024",
       fill = "Species")+
  theme_light(base_size = 20)

mean(prov_clim$tmax)


ggplot(avg_clim, aes(x = year, y = temp))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm")


