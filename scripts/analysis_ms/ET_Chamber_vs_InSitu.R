library(tidyverse)

morph <- read_csv("data/Experiment/Processed/Weight_Diam_Color.csv") %>% 
  mutate(date = date(date)) %>% 
  select(TreeID, date, spp, id, water, weight) %>% 
  mutate(chamber = case_when(spp %in% c("PIPO", "PSME") & id <= 30 ~ 1, 
                             spp %in% c("PIPO", "PSME") & id > 30 ~ 2, 
                             spp %in% c("PIFL", "PIEN") & id <= 30 ~ 3, 
                             spp %in% c("PIFL", "PIEN") & id > 30 ~ 4)) %>% 
  mutate(chamber = as.factor(chamber))

# remove watered plants from calculation???

morph_sums <- morph %>% 
  group_by(chamber, date) %>% 
  summarise(total_weight = sum(weight)) %>%
  filter(month(date) %in% seq(6, 11, 1))

ggplot(morph_sums, aes(x = date, y = total_weight))+
  geom_point(aes(color = as.factor(chamber)))+
  geom_line(aes(group = chamber))

# 10/60 (1/6) of the plants in each chamber were watered
# 5/6 of the area was accounted for by "unwatered" plants

# fit splines to remaining data:

chambers <- unique(morph_sums$chamber)
full_df <- data.frame(matrix(ncol = 0, nrow = 0))

for(i in 1:length(chambers)){
  cham <- chambers[i] # select the chamber to fit a curve for
  
  weight_cham <- morph_sums %>%  # filter the data to be only for that chamber
    filter(chamber == cham) %>% 
    arrange(date) %>% 
    mutate(day = yday(date)) %>% 
    dplyr::select(total_weight, day) %>% 
    filter(!is.na(total_weight), !is.na(day))
  
  smoother <- smooth.spline(x = weight_cham$day,
                            y = weight_cham$total_weight) # create a smooth spline for that data
  
  predict_d1 <- data.frame(et = predict(smoother, deriv = 1,
                                x = seq(from = min(weight_cham$day), 
                                to = max(weight_cham$day), 
                                by = 5))$y,
                          day = predict(smoother, deriv = 1,
                                   x = seq(from = min(weight_cham$day), 
                                           to = max(weight_cham$day), 
                                           by = 5))$x,
                          chamber = cham) %>% 
    ungroup()
  
  full_df <- rbind(predict_d1, full_df)
}

# ET rates are in grams per day

# convert grams to volume, then divide by area of chamber to get ET depth
# 1 g = 1 mL
# 1 mL = 10^3 cubic mm
# chamber footprint = 761805 sq mm 
# (not all of the area is evaporating but it defines the "control volume")
# just the footprint of the 60 pots ?
# divide chamber volume ET by chamber area

chamber_et <- full_df %>% 
  filter(et < 0) %>% 
  mutate(et_mm = (et*1000/761805))


ggplot(chamber_et, aes(x = day, y = -et_mm))+
  geom_line(aes(color = as.factor(chamber)), linewidth = 1.5)+
  labs(x = "Day of year", y = "Chamber ET (mm/day)", color = "Chamber")+
  theme_minimal(base_size = 20)





