library(tidyverse)
library(emmeans)
library(drc)

# read in stress days:
stress_days <- read_csv("data/Experiment/Processed/Water_Limitation_Days.csv")
start_exp <- yday(as.POSIXct("2025-07-21"))
brown <- read_csv("data/Experiment/Processed/Ocular_Color.csv") %>% 
  mutate(date = date(date),
         brown = case_when(brown_perc == "10" ~ 5,
                           brown_perc == "25" ~ (25+10)/2,
                           brown_perc == "50" ~ (25+50)/2,
                           brown_perc == "75" ~ (75+50)/2,
                           brown_perc == "90" ~ (75+90)/2,
                           brown_perc == ">9" ~ 95))

col_dates <- brown %>% # find dates on which color was observed
  filter(water == "drought") %>% 
  dplyr::select(day) %>% 
  mutate(day = as.numeric(day)) %>% 
  unique() # 39 dates of observation (diff days for each spp)
trees <- brown %>% # find all trees for which color was observed # change df from fl to morph data?
  filter(water == "drought") %>% 
  dplyr::select(TreeID) %>% 
  unique() # 200 trees
dates_trees <- data.frame(rep(trees$TreeID, nrow(col_dates))); colnames(dates_trees) <- "TreeID" 
dates_rep <- data.frame(rep(col_dates$day, each = nrow(trees))); colnames(dates_rep) <- "day"
full_blank <- cbind(dates_trees, dates_rep)

dead_trees_pien <- (filter(brown, spp == "PIEN",
                           water == "drought", 
                           brown > 5)) %>% 
  group_by(TreeID) %>%
  unique() %>% 
  mutate(life = 0) %>% 
  dplyr::select(TreeID, day) %>% 
  group_by(TreeID) %>% 
  filter(day == min(day))

dead_trees_pifl <- (filter(brown, spp == "PIFL",
                           water == "drought", 
                           brown >= 37.5)) %>% 
  group_by(TreeID) %>%
  unique() %>% 
  mutate(life = 0) %>% 
  dplyr::select(TreeID, day) %>% 
  group_by(TreeID) %>% 
  filter(day == min(day))

dead_trees_pipo <- (filter(brown, spp == "PIPO",
                           water == "drought", 
                           brown >= 82.5)) %>% 
  group_by(TreeID) %>%
  unique() %>% 
  mutate(life = 0) %>% 
  dplyr::select(TreeID, day) %>% 
  group_by(TreeID) %>% 
  filter(day == min(day))

dead_trees_psme <- (filter(brown, spp == "PSME",
                           water == "drought", 
                           brown >= 17.5)) %>% 
  group_by(TreeID) %>%
  unique() %>% 
  mutate(life = 0) %>% 
  dplyr::select(TreeID, day) %>% 
  group_by(TreeID) %>% 
  filter(day == min(day))


# combine spp:
dead_trees_gf <- rbind(dead_trees_pien, dead_trees_pifl, dead_trees_pipo, dead_trees_psme)
names(dead_trees_gf) <- c("TreeID", "dead_day")

metadata <- brown %>% 
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

mort_glm <- glm(life ~ days_stressed*spp + temp*spp + days_stressed*temp, data = trees_dates, family = binomial)
summary(mort_glm)

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

ggplot(filter(trees_dates), aes(x = days_stressed, y = life*100))+
  geom_hline(yintercept = 50, color = "gray50", linetype=4)+
  geom_line(aes(color = temp, linetype = temp,
                group = interaction(temp, spp),
                x = days_stressed, y = 100*pred))+
  scale_color_manual(values=c("blue", "red"))+
  scale_fill_manual(values=c("blue", "red"))+
  geom_ribbon(alpha = 0.2, aes(fill = temp, linetype = temp,
                               group = interaction(temp, spp),
                               x = days_stressed, ymin = ci_lo*100, ymax = ci_hi*100))+
  # facet_wrap(~spp, ncol = 2)+
  theme_minimal(base_size = 15)+
  labs(x = "Days of drought", y = "% survival")



library(emmeans)

emmip(mort_glm, spp ~ temp + days_stressed,
      at = list(days_stressed = c(25, 50, 75, 100, 150)),
      type = "response", CIs = T)+
  facet_wrap(~spp+temp, scales = "free")




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


