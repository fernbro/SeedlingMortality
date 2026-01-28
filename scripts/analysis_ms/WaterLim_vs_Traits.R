library(tidyverse)
library(AICcmodavg)

stress_days <- read_csv("data/Experiment/Processed/Water_Limitation_Days.csv")

# morphological data:

height <- read_csv("data/Experiment/TreeHeight_ForAnalysis.csv")[,1:2] # height from soil base to top of foliage in mm
names(height) <- c("TreeID", "height")
diam <- read_csv("data/Experiment/Processed/Weight_Diam_Color.csv") %>% 
  mutate(date = date(date)) %>% 
  dplyr::select(TreeID, day, diam_mm) %>% 
  group_by(TreeID) %>% 
  filter(day < 25) %>% 
  summarise(diam = mean(diam_mm)) # avg diameter over the first 3-ish weeks of the experiment?


analysis <- inner_join(stress_days, height) %>% 
  inner_join(diam)

hist(analysis$height)
hist(analysis$diam)
hist(analysis$cpt, breaks = 50)
hist(log(analysis$cpt), breaks = 40)
hist(sqrt(analysis$cpt), breaks = 40)

cor.test(analysis$height, analysis$diam) # they are positively correlated tho... # diam may not end up mattering much

# should pick a model selection process:
# first do height, diam, and temp individually
# then start combining/interacting variables

model_pien1 <- lm(log(cpt) ~ height+temp, data = filter(analysis, spp == "PIEN"))
model_pien <- lm(log(cpt) ~ height*temp, data = filter(analysis, spp == "PIEN"))

model_pipo1 <- lm(log(cpt) ~ diam, data = filter(analysis, spp == "PIPO"));summary(model_pipo1)
model_pipo <- lm(log(cpt) ~ height*temp, data = filter(analysis, spp == "PIPO"))


model_psme1 <- lm(log(cpt) ~ height, data = filter(analysis, spp == "PSME"));summary(model_psme1)


model_pifl1 <- lm(log(cpt) ~ height*temp*diam, data = filter(analysis, spp == "PIFL"));summary(model_pifl1)

#pifl:
# AICc(lm(log(cpt) ~ height*temp*diam, data = filter(analysis, spp == "PIFL")))
# [1] 36.37064


# ;summary(model_pien)

plot(model)

plot((fitted(model)), log(analysis$cpt))



