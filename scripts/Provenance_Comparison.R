library(tidyverse)

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
    year = as.numeric(substr(Date, 1, 4)))
names(prov_clim) <- c("date", "tmin", "tmean", "tmax", "vpdmin", "vpdmax",
                      "spp", "doy", "year")
# prov_clim <- prov_clim %>% 
#   mutate(tmean = as.numeric(tmean))

# avg_clim <- prov_clim %>% 
#   group_by(year) %>% 
#   summarise(temp = mean(tmean))


ggplot(prov_clim, aes(x = doy, y = tmax))+
  geom_point(aes(group = year, color = spp), alpha = 0.2)+
  geom_smooth(aes(group = spp, color = spp))

prov_clim <- prov_clim %>% 
  filter(!is.na(tmax)) %>% 
  mutate(tmax = as.numeric(tmax))

prov_doy_stats <- prov_clim %>% 
  group_by(spp, doy) %>% 
  summarise(tmax = max(tmax))

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





ggplot(prov_clim, aes(x = tmax))+
  geom_density(aes(color = spp), alpha = 0.4)+
  geom_vline(xintercept = mean(filter(prov_clim, spp == "PIEN")$tmax), color = "red", linetype = 2)+
  geom_vline(xintercept = mean(filter(prov_clim, spp == "PIFL")$tmax), color = "darkgreen", linetype = 2)+
  geom_vline(xintercept = mean(filter(prov_clim, spp == "PIPO")$tmax), color = "turquoise", linetype = 2)+
  geom_vline(xintercept = mean(filter(prov_clim, spp == "PSME")$tmax), color = "purple", linetype = 2)+
  labs(x = "Max Daily Temp (ºC): 1984 - 2024",
       color = "Species")+
  theme_light(base_size = 20)

ggplot(prov_clim, aes(y = tmax))+
  geom_histogram(aes(fill = spp), alpha = 0.4, binwidth = 0.5)+
  #geom_vline(yintercept = mean(prov_clim$tmax), color = "red", linetype = 2)+
  labs(y = "Max Daily Temp (ºC): 1984 - 2024",
       fill = "Species")+
  theme_light(base_size = 20)

mean(prov_clim$tmax)

options(digits = 6)
TukeyHSD(aov(tmax ~ spp, prov_clim))


ggplot(avg_clim, aes(x = year, y = temp))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm")


