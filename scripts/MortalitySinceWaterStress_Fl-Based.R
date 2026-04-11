library(tidyverse)
library(emmeans)
library(drc)

# read in stress days:
stress_days <- read_csv("data/Experiment/Processed/Water_Limitation_Days.csv")
start_exp <- yday(as.POSIXct("2025-07-21"))
fl <- read_csv("data/Experiment/Processed/Fluorescence.csv") %>% 
  mutate(date = date(date), 
         day = case_when(year(date) == 2025 ~ yday(date)-202,
                         year(date) == 2026 ~ 365 - 202 + yday(date)))

fl_dates <- fl %>% 
  filter(water == "drought") %>% 
  dplyr::select(day) %>% 
  unique() # 24 dates of observation
trees <- fl %>% 
  filter(water == "drought") %>% 
  dplyr::select(TreeID) %>% 
  unique() # 40 trees
dates_trees <- data.frame(rep(trees$TreeID, nrow(fl_dates))); colnames(dates_trees) <- "TreeID" 
dates_rep <- data.frame(rep(fl_dates$day, each = nrow(trees))); colnames(dates_rep) <- "day"
full_blank <- cbind(dates_trees, dates_rep)

dead_trees_gf <- (filter(fl, Fv_Fm_dark < 0.1, water == "drought")) %>% 
  group_by(TreeID) %>%
  unique() %>% 
  mutate(life = 0) %>% 
  dplyr::select(TreeID, day) %>% 
  group_by(TreeID) %>% 
  filter(day == min(day))
names(dead_trees_gf) <- c("TreeID", "dead_day")

metadata <- fl %>% 
  dplyr::select(spp, id, TreeID, water, temp)
  
trees_dates <- full_join(full_blank, dead_trees_gf) %>% 
  mutate(life = case_when(day < dead_day ~ 1,
                          day >= dead_day ~ 0)) %>% 
  full_join(metadata, relationship = "many-to-many") %>% 
  filter(!is.na(life)) %>% 
  inner_join(stress_days, relationship = "many-to-many") %>% 
  mutate(days_stressed = day - cpt) %>% 
  group_by(TreeID) %>% 
  filter(days_stressed > 0) %>% 
  dplyr::select(TreeID, life, days_stressed, spp, temp) %>% 
  unique()

# fill in data so that trees "stay dead" (are still indicated as dead, as to preserve the "odds")
# after they die. want predicted curves to bottom out
# do something to merge and keep the NAs for dead trees (unmeasured) and then change the NAs to a 0 or 1 depending on the data
# 
# dead_trees <- (filter(fl, Fv_Fm_dark < 0.1)) %>% 
#   group_by(TreeID) %>%
#   unique() %>% 
#   mutate(life = 0) %>% 
#   dplyr::select(TreeID, spp, date, life, temp, water, day) %>% 
#   group_by(TreeID)
# 
# 
# first_dead <- dead_trees %>% 
#   group_by(TreeID) %>% 
#   summarise(death = min(day))
# # use first_dead to expand/outwardly "gapfill" the dead_trees df so that 
# # life = 0 for that tree on all dates for which there are other measurements
# 
# alive_trees <- (filter(fl, Fv_Fm_dark >= 0.1)) %>% 
#   group_by(TreeID) %>%
#   unique() %>% 
#   mutate(life = 1) %>% 
#   dplyr::select(TreeID, spp, date, life, temp, water, day) %>% 
#   group_by(TreeID)
# 
# tree_fl <- rbind(dead_trees, alive_trees) %>% 
#   mutate(time = yday(date)-202)
# 
ggplot(trees_dates, aes(x = days_stressed, y = life))+
  geom_point(aes())+
  geom_line(aes(group = TreeID))+
  facet_wrap(~spp)

mort_glm <- glm(life ~ days_stressed*spp + 
                  temp*spp + days_stressed*temp -1, 
                data = trees_dates, family = binomial)
summary(mort_glm)

# individual spp GLMs with no temp effect: (for LD50s)
              mort_pifl <- glm(life ~ days_stressed,
                               data = filter(trees_dates, spp == "PIFL"),
                               family = binomial)
              dose.p(mort_pifl, p = 0.5)
              
              mort_pien <- glm(life ~ days_stressed,
                               data = filter(trees_dates, spp == "PIEN"),
                               family = binomial)
              dose.p(mort_pien, p = 0.5)
              
              mort_pipo <- glm(life ~ days_stressed,
                               data = filter(trees_dates, spp == "PIPO"),
                               family = binomial)
              dose.p(mort_pipo, p = 0.5)
              
              mort_psme <- glm(life ~ days_stressed,
                               data = filter(trees_dates, spp == "PSME"),
                               family = binomial)
              dose.p(mort_psme, p = 0.5)
              
# fitting and adding to data frames for plotting:
              
expit <- function(x){
  exp(x)/(1+exp(x))
}

pred <- predict(mort_glm, type = "link", se.fit = T)
lower <- expit(pred$fit + qnorm(0.025)*pred$se.fit)
upper <- expit(pred$fit + qnorm(0.975)*pred$se.fit)

trees_dates$pred <- expit(pred$fit)
trees_dates$ci_lo <- lower
trees_dates$ci_hi <- upper

trees_dates$spp <- factor(trees_dates$spp, levels = c("PSME", "PIPO", "PIEN", "PIFL"))




ggplot(trees_dates, aes(x = days_stressed, y = life*100))+
  # geom_point(aes(color = temp, shape = temp), alpha = 0.3)+
  # scale_shape_manual(values = c(21,2))+
  geom_hline(yintercept = 50, color = "gray50", linetype=4)+
  geom_line(aes(color = temp, linetype = temp,
    x = days_stressed, y = 100*pred))+
  scale_color_manual(values=c("blue", "red"))+
  scale_fill_manual(values=c("blue", "red"))+
  geom_ribbon(alpha = 0.2, aes(fill = temp, linetype = temp,
                x = days_stressed, ymin = ci_lo*100, ymax = ci_hi*100))+
  facet_wrap(~spp, ncol = 1)+
  theme_minimal(base_size = 15)+
  labs(x = "Days of water stress", y = "% survival")

# 


ggplot(filter(trees_dates, water == "drought"), aes(x = day, y = life*100))+
  geom_hline(yintercept = 50, color = "gray50", linetype=4)+
  geom_line(aes(color = temp, linetype = temp,
                group = interaction(temp, water, spp),
                x = day, y = 100*pred))+
  scale_color_manual(values=c("blue", "red"))+
  scale_fill_manual(values=c("blue", "red"))+
  geom_ribbon(alpha = 0.2, aes(fill = temp, linetype = temp,
                               group = interaction(temp, water, spp),
                               x = day, ymin = ci_lo*100, ymax = ci_hi*100))+
  # facet_wrap(~spp, ncol = 2)+
  theme_minimal(base_size = 15)+
  labs(x = "Days of drought", y = "% survival")

# which day does pred = 50 for each spp, treatment?
# LD50 of days of drought:

# ED(drm(mort_glm))

get_LD50 = function(fit){
  data.frame(
    LD50 = dose.p(fit)[1],
    CI = attributes(dose.p(fit))$SE[,1]*qnorm(0.975)
  )
}

ld50s <- trees_dates %>% 
  # filter(water == "drought") %>% 
  group_by(temp, spp) %>% 
  do(get_LD50(glm(life ~ days_stressed, family = "binomial", data = .))) %>% 
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


