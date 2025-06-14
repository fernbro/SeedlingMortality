library(tidyverse)

# Data acquired from PRISM explorer (https://prism.oregonstate.edu/explorer/) on Friday, June 13th, 2025
# see acquisition parameters in original CSVs

#Ponderosa Pine (PIPO)
pipo <- read_csv("data/Provenance_Climate/PIPO_PRISM.csv")[10:55,]
names(pipo) <- pipo[1,1:4]
pipo <- pipo[-1,]
pipo$spp <- "PIPO"

#Limber Pine (PIFL)
pifl <- read_csv("data/Provenance_Climate/PIFL_PRISM.csv")[10:55,]
names(pifl) <- pifl[1,1:4]
pifl <- pifl[-1,]
pifl$spp <- "PIFL"

#Douglas Fir (PSME)
psme <- read_csv("data/Provenance_Climate/PSME_PRISM.csv")[10:55,]
names(psme) <- psme[1,1:4]
psme <- psme[-1,]
psme$spp <- "PSME"

#Engelmann Spruce (PIEN)
pien <- read_csv("data/Provenance_Climate/PIEN_PRISM.csv")[10:55,]
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

ggplot(prov_clim, aes(x = spp, y = tmean))+
  geom_boxplot(aes(fill = spp))+
  geom_hline(yintercept = 15.73667, color = "red", linetype = 2)+
  labs(x = "Species", y = "Mean June Temp (Deg. C): 1980 - 2024",
       fill = "Species")+
  theme_light(base_size = 26)

TukeyHSD(aov(tmean ~ spp, prov_clim))


ggplot(avg_clim, aes(x = year, y = temp))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm")


