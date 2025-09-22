library(tidyverse)
library(lme4)
library(MuMIn)

con <- read_csv("data/Experiment/Processed/Conductance.csv") %>% 
  mutate(date = date(date))
vwc <- read_csv("data/Experiment/Processed/VWC.csv") %>% 
  mutate(date = date(date))

con_vwc <- inner_join(con, vwc) %>% 
  mutate(vwc = case_when(VWC_perc == 0 ~ 0.1,
                         .default = VWC_perc)) # change the VWC = 0 to detection limit (0.1%)

ggplot(con_vwc, aes(x = vwc, y = con))+
  geom_point(aes(color = spp, shape = water))+
  geom_smooth(aes(fill = spp, color = spp), method = "lm", alpha = 0.2)+
  theme_light(base_size = 20)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))+
  facet_wrap(~temp)+
  labs(x = "Volumetric water content (%)", y = "Conductance (mmol/m2/s)")


# model 1: fitting mixed model with random effect of species
            mixedmod <- lmer(con ~ 1 + VWC_perc + (1 + VWC_perc | (spp)), con_vwc, REML = F)
            # did not converge w ML 
            summary(mixedmod)
            plot(mixedmod)
            
# model 2: and a mixed model with fixed effect of spp, mixed effect of treeID
            mod2 <- lmer(con ~ 1 + spp + VWC_perc + (1 | TreeID), con_vwc, REML = F)
            summary(mod2)
            plot(mod2)
            r.squaredGLMM(mixedmod);AIC(mixedmod)
            r.squaredGLMM(mod2);AIC(mod2) # lower AIC! better R2! residuals look ok!
            
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


