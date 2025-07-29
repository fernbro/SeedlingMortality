library(tidyverse)

vwc_init <- read_csv("data/Experiment/Raw/VWC_07182025.csv")[,1:2] %>% 
  transmute(tree = TreeID, vwc1 = VWC_perc)

vwc_pipo <- read_csv("data/Experiment/Raw/PIPO_VWC_07232025.csv")
vwc_psme <- read_csv("data/Experiment/Raw/PSME_VWC_07232025.csv")
vwc_pifl <- read_csv("data/Experiment/Raw/PIFL_VWC_07242025.csv")
vwc_pien <- read_csv("data/Experiment/Raw/PIEN_VWC_07242025.csv")

vwc_2 <- rbind(vwc_pipo, vwc_psme) %>% 
  rbind(vwc_pifl) %>% 
  rbind(vwc_pien) %>% 
  transmute(tree = TreeID, vwc2 = VWC_perc)

delta_vwc <- full_join(vwc_init, vwc_2) %>% 
  filter(!is.na(vwc2)) %>% 
  mutate(spp = str_sub(tree, 1, 4), delta = vwc2-vwc1) %>% 
  mutate(set = case_when(spp %in% c("PIPO", "PSME") ~ "lo el",
                         .default = "hi el"),
         half_init = vwc1/2) # half_init would be the (theoretical) target for
                             # starting the heatwave!


# the "high elevation" plants dried out a bit more- probably because  

ggplot(delta_vwc, aes(x = vwc1, y = vwc2, color = spp))+
  geom_point(aes(shape = spp))+
  geom_smooth(method = "lm", se = F)+
  geom_abline(slope = 1, intercept = 0)+
  labs(x = "Week 1 VWC (%)", y = "Week 2 VWC (%)")

mean(filter(delta_vwc, set == "hi el")$delta, na.rm = T) # pien, pifl; -2.0225
mean(filter(delta_vwc, set == "lo el")$delta) # pipo, psme; -1.99

summary(lm(vwc2 ~ vwc1, data = delta_vwc)) # R2 0.2567

# vwc2 (in %, pph) on average was 2.6 plus 60% of vwc1
# mean decline was 2% (absolute value)

ggplot(delta_vwc, aes(x = delta))+
  geom_density(aes(fill = set))
