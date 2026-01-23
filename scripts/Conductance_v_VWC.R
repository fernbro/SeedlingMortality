library(tidyverse)
library(lme4)
library(MuMIn)

con <- read_csv("data/Experiment/Processed/Conductance.csv") %>% 
  mutate(date = date(date))
vwc <- read_csv("data/Experiment/Processed/VWC.csv") %>% 
  mutate(date = date(date))
kest <- read_csv("data/Experiment/Processed/Kestrel_Dailys_ChamberAvg.csv") %>% 
  mutate(tair = temp) %>% 
  dplyr::select(-temp)


con_vwc <- full_join(con, vwc) %>% 
  mutate(vwc = case_when(VWC_perc == 0 ~ 0.1,
                         .default = VWC_perc)) %>% # change the VWC = 0 to detection limit (0.1%)
  filter(!is.na(vwc), !is.na(con)) %>% 
  mutate(chamber = case_when(spp %in% c("PIPO", "PSME") & id < 31 ~ 1,
                             spp %in% c("PIPO", "PSME") & id > 30 ~ 2,
                             spp %in% c("PIFL", "PIEN") & id < 31 ~ 3,
                             spp %in% c("PIFL", "PIEN") & id > 30 ~ 4)) %>% 
  inner_join(kest)

ggplot(con_vwc, aes(x = vwc, y = con))+
  geom_point(aes(color = spp, shape = water))+
  geom_smooth(
    # aes(fill = spp, color = spp), 
    alpha = 0.2)+
  theme_light(base_size = 20)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))+
  facet_wrap(~temp)+
  labs(x = "Volumetric water content (%)", y = "Conductance (mmol/m2/s)")

ggplot(filter(con_vwc, water == "drought"), aes(x = vwc, y = con))+
  geom_point(aes(color = spp, shape = water))+
  geom_smooth(
    # aes(fill = spp, color = spp), 
    alpha = 0.2)+
  theme_light(base_size = 20)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))+
  facet_wrap(~temp)+
  labs(x = "Volumetric water content (%)", y = "Conductance (mmol/m2/s)")

vwc2 <- (lm(con ~ poly(vwc, 2) * vpd * spp, data = con_vwc));summary(vwc2)
# vwc2 <- (lm(con ~ vwc * spp, data = con_vwc));summary(vwc2)
anova(vwc2)

vwc2_p <- data.frame(predict.lm(vwc2, interval = "confidence")) %>% 
  bind_cols(con_vwc)

ggplot(vwc2_p, aes(x = vwc, y = con))+
  geom_point(aes(color = spp, shape = water))+
  geom_line(aes(y = fit, color = spp))+
  geom_ribbon(aes(ymax = upr, ymin = lwr, fill = spp), alpha = 0.3)+
  theme_minimal(base_size = 20)+
  # theme(strip.background = element_rect(color = "black", fill = "white"))+
  # theme(strip.text = element_text(colour = 'black'))+
  # facet_wrap(~temp)+
  labs(x = "Volumetric water content (%)", y = "Conductance (mmol/m2/s)",
       fill = "Species", shape = "Water", color = "Species")

ggplot(vwc2_p, aes(x = con, y = fit))+
  geom_point(aes(color = spp))+
  geom_abline(slope = 1, intercept = 0)

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


