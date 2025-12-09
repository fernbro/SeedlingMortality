library(tidyverse)

start_exp <- yday(as.POSIXct("2025-07-21"))
fl <- read_csv("data/Experiment/Processed/Fluorescence.csv") %>% 
  mutate(date = date(date), day = yday(date)-start_exp)

dead_trees <- (filter(fl, Fv_Fm_dark < 0.1)) %>% 
  group_by(TreeID) %>%
  unique() %>% 
  mutate(life = 1) %>% 
  select(TreeID, spp, date, life, temp, water, day)

alive_trees <- (filter(fl, Fv_Fm_dark >= 0.1)) %>% 
  group_by(TreeID) %>%
  unique() %>% 
  mutate(life = 0) %>% 
  select(TreeID, spp, date, life, temp, water, day)

tree_fl <- rbind(dead_trees, alive_trees) %>% 
  mutate(time = yday(date)-202)

ggplot(tree_fl, aes(x = day, y = life))+
  geom_point(aes(color = water))+
  facet_wrap(~spp)

mort_glm <- glm(life ~ day + temp*water*spp, data = tree_fl, family = binomial)
summary(mort_glm)

expit <- function(x){
  exp(x)/(1+exp(x))
}

pred <- predict(mort_glm, type = "link", se.fit = T)
lower <- expit(pred$fit + qnorm(0.025)*pred$se.fit)
upper <- expit(pred$fit + qnorm(0.975)*pred$se.fit)

tree_fl$pred <- expit(pred$fit)
tree_fl$ci_lo <- lower
tree_fl$ci_hi <- upper

ggplot(tree_fl, aes(x = day, y = life))+
  geom_point(aes(color = water))+
  geom_line(aes(color = water, linetype = temp,
                group = interaction(temp, water),
    x = day, y = pred))+
  geom_ribbon(alpha = 0.2, aes(fill = water, linetype = temp,
                group = interaction(temp, water),
                x = day, ymin = ci_lo, ymax = ci_hi))+
  facet_wrap(~spp)+
  theme_minimal(base_size = 20)+
  labs(x = "Day", y = "% mortality")
