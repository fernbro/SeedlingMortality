library(tidyverse)

# Data acquired from PRISM explorer (https://prism.oregonstate.edu/explorer/) on Friday, June 13th, 2025
# see acquisition parameters in original CSVs

#Ponderosa Pine (PIPO)
pipo <- read_csv("data/Provenance/PIPO_June_Monthly.csv")[10:55,]
names(pipo) <- pipo[1,1:4]
pipo <- pipo[-1,]
pipo$spp <- "PIPO"

#Limber Pine (PIFL)
pifl <- read_csv("data/Provenance/PIFL_June_Monthly.csv")[10:55,]
names(pifl) <- pifl[1,1:4]
pifl <- pifl[-1,]
pifl$spp <- "PIFL"

#Douglas Fir (PSME)
psme <- read_csv("data/Provenance/PSME_June_Monthly.csv")[10:55,]
names(psme) <- psme[1,1:4]
psme <- psme[-1,]
psme$spp <- "PSME"

#Engelmann Spruce (PIEN)
pien <- read_csv("data/Provenance/PIEN_June_Monthly.csv")[10:55,]
names(pien) <- pien[1,1:4]
pien <- pien[-1,]
pien$spp <- "PIEN"

prov_clim <- rbind(pipo, pifl, psme, pien) %>% 
  mutate(year = as.numeric(substr(Date, 1, 4))) %>% 
  select(-Date)
names(prov_clim) <- c("tmin", "tmean", "tmax", "spp", "year")
prov_clim <- prov_clim %>% 
  mutate(tmean = as.numeric(tmean))

avg_clim <- prov_clim %>% 
  group_by(year) %>% 
  summarise(temp = mean(tmean))


ggplot(prov_clim, aes(x = year, y = tmean))+
  geom_point(aes(group = spp, color = spp))+
  geom_line(aes(group = spp, color = spp))+
  geom_smooth(method = "lm", aes(group = spp, color = spp),
              alpha = 0.2)

prov_clim <- prov_clim %>% 
  filter(!is.na(tmax)) %>% 
  mutate(tmax = as.numeric(tmax))

ggplot(prov_clim, aes(x = spp, y = tmax))+
  geom_boxplot(aes(fill = spp))+
  geom_hline(yintercept = mean(prov_clim$tmax), color = "red", linetype = 2)+
  labs(x = "Species", y = "Max June Temp (ºC): 1980 - 2024",
       fill = "Species")+
  theme_light(base_size = 20)

mean(prov_clim$tmax)

TukeyHSD(aov(tmean ~ spp, prov_clim))


ggplot(avg_clim, aes(x = year, y = temp))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm")


