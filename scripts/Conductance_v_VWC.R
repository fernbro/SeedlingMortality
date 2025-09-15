library(tidyverse)
library(lme4)
library(MuMIn)

con <- read_csv("data/Experiment/Processed/Conductance.csv") %>% 
  mutate(date = date(date))
vwc <- read_csv("data/Experiment/Processed/VWC.csv") %>% 
  mutate(date = date(date))

con_vwc <- inner_join(con, vwc) %>% 
  mutate(vwc = case_when(VWC_perc == 0 ~ 0.1,
                         .default = VWC_perc))

ggplot(con_vwc, aes(x = vwc, y = con))+
  geom_point(aes(color = spp, shape = water))+
  geom_smooth(aes(fill = spp), method = "lm")
  # geom_line(aes(y = predict(mixedmod), color = spp))


# model 1: fitting mixed model with random effect of species
            mixedmod <- lmer(con ~ 1 + VWC_perc + (1 + VWC_perc | (spp)), con_vwc)
            summary(mixedmod)
            plot(mixedmod)
            
# model 2: and a mixed model with fixed effect of spp, mixed effect of treeID
            mod2 <- lmer(con ~ 1 + spp + VWC_perc + (1 | TreeID), con_vwc)
            summary(mod2)
            plot(mod2)
            r.squaredGLMM(mixedmod);AIC(mixedmod)
            r.squaredGLMM(mod2);AIC(mod2)
            
            # better R2 (c and m), lower AIC than model 1
            
            
# model(s) 3: individual models for each spp with mixed effect of TreeID
            # a different type of analysis...
            # how might i differently interpret the mixed model?
            
            lmer(con ~ 1 + VWC_perc + (1 + VWC_perc | TreeID), con_vwc)
             # failed to convg
            
            pipo <- lmer(con ~ 1 + VWC_perc + (1 | TreeID), filter(con_vwc, spp == "PIPO"))
            r.squaredGLMM(pipo)
            
            psme <- lmer(con ~ 1 + (VWC_perc) + (1 | TreeID), filter(con_vwc, spp == "PSME"))
            r.squaredGLMM(psme)
            plot(filter(con_vwc, spp == "PSME")$VWC_perc, filter(con_vwc, spp == "PSME")$con)
            plot(filter(con_vwc, spp == "PSME")$VWC_perc, predict(psme))
            plot(psme)
            
            
            pifl <- lm(con ~ 1 + VWC_perc, filter(con_vwc, spp == "PIFL"))
            r.squaredGLMM(pifl)
            plot(filter(con_vwc, spp == "PIFL")$VWC_perc, filter(con_vwc, spp == "PIFL")$con)
            
            pien <- lmer(con ~ 1 + VWC_perc + (1 | TreeID), filter(con_vwc, spp == "PIEN"))
            r.squaredGLMM(pien)

# 


