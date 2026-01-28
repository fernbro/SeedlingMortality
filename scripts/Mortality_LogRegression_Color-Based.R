library(tidyverse)
library(emmeans)
library(drc)

start_exp <- yday(as.POSIXct("2025-07-21"))
brown <- read_csv("data/Experiment/Processed/Ocular_Color.csv") %>% 
  mutate(date = date(date))

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
  full_join(metadata) %>% 
  filter(!is.na(life), TreeID %in% dead_trees_gf$TreeID)

mort_pien <- filter(trees_dates, spp == "PIEN")
ggplot(mort_pien, aes(x = day, y = life))+
  geom_point()+
  facet_wrap(~temp)


mort_pifl <- filter(trees_dates, spp == "PIFL")
mort_pipo <- filter(trees_dates, spp == "PIPO")
mort_psme <- filter(trees_dates, spp == "PSME")

pien_glm <- glm(life ~ day*temp, data = mort_pien, family = binomial); summary(pien_glm)


# single model approach...
mort_glm <- glm(life ~ day + day:spp + spp:temp, data = trees_dates, family = binomial)
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

ggplot(filter(trees_dates, water == "drought"), aes(x = day, y = life*100))+
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

# read in stress days:
stress_days <- read_csv("data/Experiment/Processed/Water_Limitation_Days.csv")
stress_means <- stress_days %>% 
  group_by(spp, temp) %>% 
  summarise(mean_lim = mean(cpt), med_lim = median(cpt))




ggplot(filter(trees_dates, water == "drought"), aes(x = day, y = life*100))+
  geom_hline(yintercept = 50, color = "gray50", linetype=4)+
  geom_line(aes(color = spp, linetype = temp,
                group = interaction(temp, water, spp),
                x = day, y = 100*pred))+
  scale_color_manual(values=c("#D81B60", "#1E88E5", "#FFC024", "#004D40"))+
  scale_fill_manual(values=c("#D81B60", "#1E88E5", "#FFC024", "#004D40"))+  
  geom_vline(data = stress_means, aes(xintercept = mean_lim, color = spp,
                                      linetype = temp))+
  geom_ribbon(alpha = 0.2, aes(fill = spp, linetype = temp,
                               group = interaction(temp, water, spp),
                               x = day, ymin = ci_lo*100, ymax = ci_hi*100))+
  facet_wrap(~spp, ncol = 2)+
  theme_minimal(base_size = 15)+
  labs(x = "Days of drought", y = "% survival")

# which day does pred = 50 for each spp, treatment?
# LD50 of days of drought:

get_LD50 = function(fit){
  data.frame(
    LD50 = dose.p(fit)[1],
    CI = attributes(dose.p(fit))$SE[,1]*qnorm(0.975)
  )
}

ld50s <- trees_dates %>%
  filter(water == "drought") %>%
  group_by(spp, temp) %>%
  nest(data = -c(spp)) %>%
  mutate(fitted = map(data, ~ get_LD50(fit = glm(life ~ day + temp, family = "binomial", data = .)))) %>%
  unnest(cols = c(data, fitted)) %>%
  dplyr::select(spp, temp, LD50, CI) %>%
  mutate(ci_lo = LD50-CI, ci_hi = LD50+CI) %>%
  unique()

# ld50s <- trees_dates %>%
#   filter(water == "drought") %>%
#   nest(data = -spp) %>% 
#   mutate(fitted = map(data, ~ get_LD50(fit = glm(life ~ day + temp, family = "binomial", data = .)))) %>%
#   unnest(cols = c(data, fitted)) %>%
#   dplyr::select(spp, temp, LD50, CI) %>%
#   group_by(spp, temp) %>%
#   summarise(ci_lo = LD50-CI, ci_hi = LD50+CI)




ggplot(ld50s, aes(x = LD50, y = spp, shape = temp, color = temp))+
  geom_errorbar(aes(xmin = LD50-CI, xmax = LD50+CI), width = 0.2, alpha = 0.6)+
  # geom_line(aes(group = temp))+
  # facet_wrap(~temp)+
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


