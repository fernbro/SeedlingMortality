library(tidyverse)
library(emmeans)
library(drc)

start_exp <- yday(as.POSIXct("2025-07-21"))
fl <- read_csv("data/Experiment/Processed/Fluorescence.csv") %>% 
  mutate(date = date(date), 
         day = case_when(year(date) == 2025 ~ yday(date)-202,
                         year(date) == 2026 ~ 365 - 202 + yday(date)))

dead_trees <- (filter(fl, Fv_Fm_dark < 0.1)) %>% 
  group_by(TreeID) %>%
  unique() %>% 
  mutate(life = 0) %>% 
  dplyr::select(TreeID, spp, date, life, temp, water, day)

alive_trees <- (filter(fl, Fv_Fm_dark >= 0.1)) %>% 
  group_by(TreeID) %>%
  unique() %>% 
  mutate(life = 1) %>% 
  dplyr::select(TreeID, spp, date, life, temp, water, day)

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

tree_fl$spp <- factor(tree_fl$spp, levels = c("PSME", "PIPO", "PIEN", "PIFL"))

ggplot(filter(tree_fl, water == "drought"), aes(x = day, y = life*100))+
  # geom_point(aes(color = temp, shape = temp), alpha = 0.3)+
  # scale_shape_manual(values = c(21,2))+
  geom_hline(yintercept = 50, color = "gray50", linetype=4)+
  geom_line(aes(color = temp, linetype = temp,
                group = interaction(temp, water),
    x = day, y = 100*pred))+
  scale_color_manual(values=c("blue", "red"))+
  scale_fill_manual(values=c("blue", "red"))+
  geom_ribbon(alpha = 0.2, aes(fill = temp, linetype = temp,
                group = interaction(temp, water),
                x = day, ymin = ci_lo*100, ymax = ci_hi*100))+
  facet_wrap(~spp, ncol = 1)+
  theme_minimal(base_size = 15)+
  labs(x = "Days of drought", y = "% survival")

# which day does pred = 50 for each spp, treatment?
# LD50 of days of drought:

ED(drm(mort_glm))

get_LD50 = function(fit){
  data.frame(
    LD50 = dose.p(fit)[1],
    CI = attributes(dose.p(fit))$SE[,1]*qnorm(0.975)
  )
}

ld50s <- tree_fl %>% 
  filter(water == "drought") %>% 
  group_by(temp, spp) %>% 
  do(get_LD50(glm(life ~ day, family = "binomial", data = .))) %>% 
  mutate(ci_lo = LD50-CI, ci_hi = LD50+CI)

ggplot(ld50s, aes(x = LD50, y = spp, shape = temp, color = temp))+
  geom_errorbar(aes(xmin = LD50-CI, xmax = LD50+CI), width = 0.2, alpha = 0.6)+
  geom_point(size = 2)+
  theme_minimal(base_size = 20)+
  labs(x = "Drought days needed for 50% mortality",
       y = "Species",
       color = "Temp", shape = "Temp")

ggplot(ld50s, aes(y = LD50, x = spp, shape = temp, color = temp))+
  geom_errorbar(aes(ymin = LD50-CI, ymax = LD50+CI), width = 0.2, alpha = 0.6)+
  geom_point(size = 2)+
  theme_minimal(base_size = 20)+
  labs(y = "Drought days LD50",
       x = "Species",
       color = "Temp")


