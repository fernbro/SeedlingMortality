library(tidyverse)
library(changepoint)

hw_colors <- c("blue", "red")

# using changepoint analysis (AMOC) to detect water limitation and the "profile-controlled stage"
# of soil drying for our seedling mortality experiment

morph <- read_csv("data/Experiment/Processed/Weight_Diam_Color.csv") %>% 
  mutate(date = date(date))

# examp <- stress_df %>% 
#   filter(TreeID == "PIEN4") %>% 
#   dplyr::select(weight)
# 
# x <- cpt.mean(examp$weight, method = "AMOC", class = F)[[1]] 

# this just looks at changes in the MEAN WEIGHT, *not the rate of change*
# could simply differentiate and it might be ok?
# or also look into cpop R package: "Detection of Multiple Changes in Slope in Univariate Time-Series

stress_df <- morph %>% 
  filter(water == "drought")

# create a vector of the individual trees:
ind <- unique(stress_df$TreeID)

# create empty df to fill with "stress points"
stress_day <- data.frame(matrix(ncol = 0, nrow = 0))

# goal:
# for each tree, identify a changepoint in the first derivative using cpt.mean
# extract a date using the cpt output time index for each TreeID

for(i in 1:length(ind)){
  ID <- ind[i] # select the individual to fit a curve for
  
  weight_id <- stress_df %>%  # filter the data to be only for that individual
    filter(TreeID == ID) %>% 
    arrange(day) %>% 
    dplyr::select(weight, day) %>% 
    filter(!is.na(weight), !is.na(day))
  
  smoother <- smooth.spline(x = weight_id$day,
                            y = weight_id$weight) # create a smooth spline for that data
  
  predict_d1 <- predict(smoother, deriv = 1,
                        x = seq(from = min(weight_id$day), 
                                to = max(weight_id$day), 
                                by = 1))$y
  
  stress_day_1 <- data.frame(TreeID = ID,
                             cpt = cpt.mean(predict_d1, 
                                            method = "AMOC", 
                                            class = F)[[1]])
  
  stress_day <- rbind(stress_day, stress_day_1)
}


stress_days <- stress_day %>% 
  inner_join(unique(dplyr::select(morph, TreeID, spp, id, temp, water)))

test <- full_join(stress_days, death_days)
cor.test(test$cpt, test$day)
cor.test(filter(test, temp == "heatwave")$cpt, filter(test, temp == "heatwave")$day)
cor.test(filter(test, temp == "ambient")$cpt, filter(test, temp == "ambient")$day)

# write_csv(stress_days, "data/Experiment/Processed/Water_Limitation_Days.csv")

ggplot(stress_days, aes(x = spp, y = cpt))+
  geom_boxplot(aes(fill = temp), alpha = 0.8)+
  theme_light(base_size = 20)+
  scale_fill_manual(values = hw_colors)+
  labs(x = "Species", y = "Day of water limitation (first derivative changepoint)", fill = "Temp")

TukeyHSD(aov(cpt ~ spp * temp, data = stress_days))

stress_means <- stress_days %>% 
  group_by(spp, temp) %>% 
  summarise(mean_lim = mean(cpt), med_lim = median(cpt))


ggplot()+
  geom_line(data = filter(morph, water == "drought"), aes(x = day, y = weight, group = TreeID, color = id))+
  geom_vline(data = filter(stress_days, water == "drought"), aes(xintercept = cpt, group = TreeID))+
  facet_wrap(~spp, scales = "free")+
  theme_minimal(base_size = 20)+
  theme(legend.position = "none")

ggplot()+
  geom_line(data = filter(morph, water == "drought"), alpha = 0.5,
            aes(x = day, y = weight, group = TreeID, color = id))+
  geom_vline(data = stress_means, aes(xintercept = mean_lim, linetype = temp))+
  facet_wrap(~spp, scales = "free")+
  theme_minimal(base_size = 20)+
  theme(legend.position = "none")



