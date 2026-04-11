library(tidyverse)
library(lme4)

morph <- read_csv("data/Experiment/Processed/Weight_Diam_Color.csv") %>% 
  mutate(date = date(date)) %>% 
  dplyr::select(TreeID, date, spp, id, water, temp, weight) %>% 
  mutate(chamber = case_when(spp %in% c("PIPO", "PSME") & id <= 30 ~ 1, 
                             spp %in% c("PIPO", "PSME") & id > 30 ~ 2, 
                             spp %in% c("PIFL", "PIEN") & id <= 30 ~ 3, 
                             spp %in% c("PIFL", "PIEN") & id > 30 ~ 4)) %>% 
  mutate(chamber = as.factor(chamber))

meta <- morph %>% 
  dplyr::select(TreeID, chamber, water, temp)

# tree ID + spp vector?
tree_spp <- morph %>% 
  dplyr::select(TreeID, spp) %>% 
  unique()

# remove watered plants from calculation???

morph <- morph %>% 
  filter(water == "drought") %>%  # only keep droughted plants
  select(TreeID, date, weight) %>% 
  filter(month(date) %in% seq(6, 11, 1))

ggplot(morph, aes(x = date, y = weight))+
  geom_point(aes(group = TreeID))+
  geom_line(aes(group = TreeID))

# 10/60 (1/6) of the plants in each chamber were watered
# 5/6 of the area was accounted for by "unwatered" plants

# fit splines to remaining data:

ind <- unique(morph$TreeID)
full_df <- data.frame(matrix(ncol = 0, nrow = 0))

for(i in 1:length(ind)){
  ID <- ind[i] # select the chamber to fit a curve for
  
  weight_id <- morph %>%  # filter the data to be only for that chamber
    filter(TreeID == ID) %>% 
    arrange(date) %>% 
    mutate(day = yday(date)) %>% 
    dplyr::select(weight, day) %>% 
    filter(!is.na(weight), !is.na(day))
  
  smoother <- smooth.spline(x = weight_id$day,
                            y = weight_id$weight) # create a smooth spline for that data
  
  predict_d1 <- data.frame(et = predict(smoother, deriv = 1,
                                x = seq(from = min(weight_id$day), 
                                to = max(weight_id$day), 
                                by = 1))$y,
                          day = predict(smoother, deriv = 1,
                                   x = seq(from = min(weight_id$day), 
                                           to = max(weight_id$day), 
                                           by = 1))$x,
                          TreeID = ID) %>% 
    ungroup()
  
  full_df <- rbind(predict_d1, full_df)
}

# ET rates are in grams per day

# convert grams to volume, then divide by area of chamber to get ET depth
# 1 g = 1 mL
# 1 mL = 10^3 cubic mm
# chamber footprint = 761805 sq mm 
# individual pot footprint: 2.7 inch diameter at top
  # 2.7 in d = 1.35 in r; radius = 34.29 mm; area = pi*r^2 3693.89752262 mm^2
# (not all of the area is evaporating but it defines the "control volume")
# just the footprint of the 60 pots ?
# divide chamber volume ET by chamber area

tree_et <- full_df %>% 
  filter(et < 0) %>% 
  mutate(et_mm = (et*1000/3693.8975),
         day = day - 202) %>% 
  inner_join(meta)

tree_et$chamber <- as.factor(tree_et$chamber)

ggplot(tree_et, aes(x = day, y = -et_mm))+
  geom_line(aes(color = as.factor(TreeID)))+
  labs(x = "Day of year", y = "ET (mm/day)", color = "Chamber")+
  theme_minimal(base_size = 20)+
  theme(legend.position = "none")


# regress individual ET rate against chamber VPD?

kest <- read_csv("data/Experiment/Processed/Kestrel_Dailys.csv") %>% 
  mutate(ta = temp) %>% 
  dplyr::select(-temp)
kest$chamber <- as.factor(kest$chamber)

et_vpd <- full_join(tree_et, kest) %>% 
  mutate(et_mm = -et_mm) %>% 
  mutate(hw = case_when(
    # chamber %in% c(2, 4) & 
      day %in% seq(19,25, by = 1)~ "hw",
    .default = "amb"
    
  )) %>% 
  inner_join(tree_spp)

# add a time "flag" to indicate if points were during the heatwave..?

ggplot(filter(et_vpd, hw == "hw"), 
       aes(x = vpd_m, y = et_mm))+
  geom_point(aes(color = chamber), alpha = 0.4)+
  geom_smooth(method = "lm", aes(group = TreeID))+
  labs(x = "VPD (kPa)", y = "ET (mm/day)", color = "")+
  theme_minimal(base_size = 20)

TukeyHSD(aov(et_mm ~ chamber, data = filter(et_vpd, hw == "hw")))

summary(lmer(et_mm ~ vpd_m + (1 | TreeID), data = filter(et_vpd)))

ggplot(et_vpd, aes(x = vpd_m, y = et_mm))+
  geom_bin2d(binwidth = c(0.05, 0.1))+
  scale_fill_continuous(type = "viridis")+
  # geom_smooth(aes(group = chamber), se = F)+
  geom_smooth(method = "lm", se = F)+
  labs(x = "VPD (kPa)", y = "ET (mm/day)", color = "Chamber")+
  theme_minimal(base_size = 20)+
  theme(legend.position = "none")

summary(lm(et_mm ~ vpd_m + hw + spp, et_vpd))

# i want to include VWC in my model with an interaction
# to see if the effect of VPD on ET depended on VWC

