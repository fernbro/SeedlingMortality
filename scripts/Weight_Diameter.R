library(tidyverse)
library(rootSolve) # for derivatives
library(MuMIn)

start_exp <- yday(as.POSIXct("2025-07-21"))
water <- read_csv("data/Experiment/Raw/Watered_Plants.csv")$TreeID
weeks <- read_csv("data/Experiment/Dates.csv") %>% 
  mutate(date = as.POSIXct(date, tryFormats = "%m/%d/%y"))
sub4phys <- read_csv("data/Experiment/Subset.csv")$TreeID

morph_files <- list.files("data/Experiment/Raw/TheWorks", full.names = T)

morph_dat <- lapply(morph_files, read_csv)

names(morph_dat) <- str_sub(morph_files, start = 30, end = 42)

for(i in 1:length(morph_dat)){
  morph_dat[[i]]$textdate <- str_sub(morph_files[i], start = 44, end = 51)
}

morph <- bind_rows(morph_dat) %>% 
  mutate(date = as.POSIXct(textdate, tryFormats = "%m%d%Y"),
         spp = str_sub(TreeID, start = 1, end = 4),
         id = as.numeric(str_sub(TreeID, start = 5, end = 6)),
         brown_perc = str_sub(Perc_brown, start = 1, end = 2),
         diam_mm = round(Diameter_mm, 1)) %>%  # rounded diameter
  dplyr::select(-textdate, -Perc_brown, -Diameter_mm) %>% 
  mutate(temp = case_when(id < 31 ~ "ambient",
                          id >= 31 ~ "heatwave"),
         water = case_when(TreeID %in% water ~ "water",
                           .default = "drought"),
         brown = as.numeric(case_when(brown_perc == "10" ~ "5",
                                      brown_perc == "25" ~ "17.5",
                                      brown_perc == "50" ~ "37.5",
                                      brown_perc == "75" ~ "62.5",
                                      brown_perc == "90" ~ "82.5",
                                      brown_perc == ">9" ~ "95")),
         date = date(date),
         weight = Pot_weight_g) %>% 
  mutate(day = case_when(year(date) == 2025 ~ yday(date)-202,
                               year(date) == 2026 ~ 365 - 202 + yday(date))) %>% 
  dplyr::select(-brown_perc, -Pot_weight_g) %>% 
  full_join(weeks) %>% 
  filter(!is.na(weight))

morph %>% 
  dplyr::select(date, week, day, spp, TreeID, id, temp, water, brown) %>% 
  write_csv("data/Experiment/Processed/Ocular_Color.csv")

ggplot(morph, aes(x = day, y = weight))+
  # geom_line(alpha = 0.4, aes(group = TreeID, color = water, linetype = temp))+
  geom_point(size = 2, alpha = 0.4, aes(group = TreeID, color = water, shape = temp))+
  #geom_point(aes(shape = spp))+
  # geom_boxplot(aes(group = interaction(date, spp), fill = spp))+
  # facet_wrap(~interaction(water, spp), nrow = 4)+
  geom_smooth(aes(fill = water, linetype = temp,
                  group = interaction(temp, water)))+
  theme_light(base_size = 20)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))+
  labs(x = "Day", y = "Weight (g) ", color = "Water", fill = "Water", 
       linetype = "Temperature", shape = "Temperature")+
  facet_wrap(~spp)


ggplot(filter(morph, water == "drought"), aes(x = day, y = weight))+
  geom_line(alpha = 0.5, aes(group = TreeID, color = water, linetype = temp))+
  # geom_point(size = 1, alpha = 0.4, aes(group = TreeID, color = water, shape = temp))+
  # geom_smooth(aes(fill = water, linetype = temp,
  #                 group = TreeID), se = F, alpha = 0.2)+
  # geom_label(data = filter(morph, water == "drought", day == 46), 
  #            aes(x = day, y = weight, label = TreeID))+
  theme_light(base_size = 20)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))+
  labs(x = "Day", y = "Weight (g) ", color = "Water", fill = "Water", 
       linetype = "Temperature", shape = "Temperature")+
  facet_wrap(~spp)

# calculate weight inflection points:

# fit a loess smoother to each individual

ind <- unique(morph$TreeID)

# https://stackoverflow.com/questions/50163106/loess-regression-on-each-group-with-dplyrgroup-by

# actually probably want to fit a GAM

library(mgcv)

# models <- morph %>% 
#   tidyr::nest(data = -TreeID) %>% 
#   dplyr::mutate(m = map(data, smooth.spline, 
#                         x = weight, y = day),
#                 fitted = map(m, `[[`, "fitted")
#   )
# 
# results <- models %>%
#   dplyr::select(-m) %>%
#   tidyr::unnest(cols = c(data,fitted))

# check this post:
# https://stackoverflow.com/questions/79181587/using-map-with-gam-model

#https://stackoverflow.com/questions/6356665/how-do-i-plot-the-first-derivative-of-the-smoothing-function
# dY <- diff(results$fitted)/diff(results$day) 
# dX <- rowMeans(embed(results$day, 2))
# 
# plot(dX, dY)

# diff() utilizes lagged differences, so it's like a "piecewise" derivative; i want a smooth function

#https://stats.stackexchange.com/questions/76959/finding-inflection-points-in-r-from-smoothed-data#:~:text=To%20find%20inflection%20points%20in%20smoothed%20data,dependent%20on%20the%20smoothing%20function%20you%20use.

results <- results %>% 
  arrange(day) %>% 
  group_by(TreeID) %>% 
  mutate(infl = case_when(diff(diff(fitted)) == 0 ~ T,
                          .default = F))

infl <- c(FALSE, diff(diff(results$fitted)>0)!=0)

# let's try the inflection package:

library(inflection)




# changepoint analysis?
# install.packages("changepoint")
library(changepoint)

# allie's methods
# https://github.com/alexandralalor/HeatwaveProject/blob/main/scripts/3_analysis/3_analysis_1_Weight.R

# remove NA values from the weight data - already done above

# include only droughted plants
stress_df <- morph %>% 
  filter(water == "drought")

# allie's code:
# for(i in 1:length(SpeciesID)) {
#   ID <- SpeciesID[i]
#   Phase1_Data_Weight_filter <- Phase1_Data_Weight %>% 
#     filter(SpeciesID == ID)
#   smooth <- smooth.spline(x = Phase1_Data_Weight_filter$Week,
#                           y = Phase1_Data_Weight_filter$Weight_g)
#   predict_d2 <- predict(smooth, deriv=2)
#   stress_week_1 <- as.matrix(uniroot.all(approxfun(predict_d2$x, predict_d2$y),
#                                          interval = range(predict_d2$x)))
#   colnames(stress_week_1) <- ID
#   stress_week <- merge(stress_week, stress_week_1, by = 0, all = T)
#   stress_week <- stress_week %>% 
#     select(c(-"Row.names"))
# }

# create a vector of the individual trees:
ind <- unique(stress_df$TreeID)

# create empty df to fill with "stress points"
stress_day <- data.frame(matrix(ncol = 0, nrow = 0))
for(i in 1:length(ind)){
  ID <- ind[i] # select the individual to fit a curve for
  
  weight_id <- stress_df %>%  # filter the data to be only for that individual
    filter(TreeID == ID) %>% 
    arrange(day)
  
  smoother <- smooth.spline(x = weight_id$day,
                            y = weight_id$weight) # create a smooth spline for that data

  predict_d2 <- predict(smoother, deriv = 2)
  stress_day_1 <- as.matrix(uniroot.all(approxfun(predict_d2$x, predict_d2$y),
                                                   interval = range(predict_d2$x)))
  
  colnames(stress_day_1) <- ID
  
  stress_day <- merge(stress_day, stress_day_1, by = 0, all = T) 
  stress_day <- stress_day %>% 
    dplyr::select(-`Row.names`)
}

# for maximum of second derivative?
for(i in 1:length(ind)){
  ID <- ind[i] # select the individual to fit a curve for
  
  weight_id <- stress_df %>%  # filter the data to be only for that individual
    filter(TreeID == ID) %>% 
    arrange(day)
  
  smoother <- smooth.spline(x = weight_id$day,
                            y = weight_id$weight) # create a smooth spline for that data
  
  predict_d2 <- predict(smoother, deriv = 2, newdata = data.frame(day = seq(from = 1, to = max(weight_id$day), by = 1)))
  stress_day_1 <- data.frame(predict_d2$x[which(predict_d2$y == max(predict_d2$y))]+1)
    # change to which is max ..? 
  
  #predict_d2[which(predict_d2==max(predict_d2))] ? 
  
  colnames(stress_day_1) <- ID
  
  stress_day <- merge(stress_day, stress_day_1, by = 0, all = T) 
  stress_day <- stress_day %>% 
    dplyr::select(-`Row.names`)
}

stress_days <- gather(stress_day, "TreeID", "day") %>% 
  filter(!is.na(day)) %>% # remove NAs introduced by merging
  inner_join(unique(dplyr::select(morph, TreeID, spp, id, temp, water))) # add metadata


ggplot(stress_days, aes(x = day, y = spp))+
  geom_boxplot()

ggplot()+
  geom_line(data = filter(morph, water == "drought"), aes(x = day, y = weight, group = TreeID, color = id))+
  geom_vline(data = filter(stress_days, water == "drought"), aes(xintercept = day, group = TreeID))+
  facet_wrap(~spp)+
  theme_minimal(base_size = 20)+
  theme(legend.position = "none")


# average limitation for each spp:

stress_avg <- stress_days %>% 
  filter(water == "drought") %>% 
  group_by(spp, temp) %>% 
  summarise(day = median(day))

ggplot()+
  geom_line(data = filter(morph, water == "drought"), aes(x = day, y = weight, group = TreeID, color = id))+
  geom_vline(data = stress_avg, aes(xintercept = day, linetype = temp))+
  scale_color_continuous(guide = "none")+
  facet_wrap(~spp)+
  theme_minimal(base_size = 20)


# other stuff.....

ggplot(filter(morph, TreeID == "PIEN10"), aes(x = day))+
  geom_line(aes(y = data.frame(predict(smooth.spline(filter(morph, TreeID == "PIEN28")$day, 
                                            filter(morph, TreeID == "PIEN28")$weight),
                                      deriv = 2,
                                      data = filter(morph, TreeID == "PIEN28")$day))$y))
  # geom_line(aes(y = weight), color = "red")

ggplot(filter(morph, TreeID == "PIEN10"), aes(x = day))+
  geom_line(aes(y = data.frame(predict(smooth.spline(filter(morph, TreeID == "PIPO40")$day, 
                                                     filter(morph, TreeID == "PIPO40")$weight),
                                       deriv = 2,
                                       data = filter(morph, TreeID == "PIPO40")$day))$y))

# stress_day_min <- stress_day %>% 
#   group_by(TreeID) %>% 
#   mutate(day_min = min(day)) %>% 
#   filter(day == day_min) %>% 
#   select(-day_min)


ID <- "PSME7"
ggplot(filter(morph, TreeID == ID), aes(x = day))+
  geom_line(aes(y = data.frame(predict(smooth.spline(filter(morph, TreeID == ID)$day, 
                                                     filter(morph, TreeID == ID)$weight),
                                       deriv = 2,
                                       data = filter(morph, TreeID == ID)$day))$y))+
  # geom_hline(yintercept = -1)+
  labs(title = "First derivative", x = "Day", y = "Weight change (g/day)")

ggplot(filter(morph, TreeID == ID), aes(x = day))+
  geom_line(aes(y = data.frame(predict(smooth.spline(filter(morph, TreeID == ID)$day, 
                                                     filter(morph, TreeID == ID)$weight),
                                       data = filter(morph, TreeID == ID)$day))$y))+
  labs(title = "Raw", x = "Day", y = "Weight (g)")

ggplot(filter(morph, TreeID == ID), aes(x = day))+
  geom_line(aes(y = -data.frame(predict(smooth.spline(filter(morph, TreeID == ID)$day, 
                                                     filter(morph, TreeID == ID)$weight),
                                       deriv = 1,
                                       data = filter(morph, TreeID == ID)$day))$y))+
  # geom_hline(yintercept = 1)+
  labs(title = "First derivative", x = "Day", y = "Water use (g/day)")

# Convert mass of water to volume and divide by the area of the opening of the pot
# for a rough estimate of depth

# 2.5 inch diameter opening  = 31.75 mm radius
# area of circle = 3166.92 mm^2
# 1 g of water = 1 mL = 1000 mm^3
# 1 g of water / day = 0.31 mm / day

# are there any literature values of rates of water loss per day that are "limited"?
# we are overall looking for a Slowing of water loss -> soil particles holding on, stomatal regulation, limited root uptake


# exponential decay regression?
# see if rates of water loss differ - get "e-folding times" ?
# from Hamerlynck et al. 2010 in Oecologia

# https://douglas-watson.github.io/post/2018-09_exponential_curve_fitting/

# using SSasymp(): self-starting nls (nonlinear least sq) asymptotic model
# evaluates initial estimates of parameters needed for nls regression

exp_fit <- nls(weight ~ SSasymp(day, yf, y0, log_alpha), data = filter(morph, water == "drought"))
summary(exp_fit)

# yf: value to which the response decays
# y0: value at which the response starts
# alpha: the rate of decay

# create empty df to fill with alphas

# alpha <- data.frame(matrix(ncol = 0, nrow = 0))
# for(i in 1:length(ind)){
#   ID <- ind[i] # select the individual to fit a curve for
#   
#   weight_id <- stress_df %>%  # filter the data to be only for that individual
#     filter(TreeID == ID) %>% 
#     arrange(day)
#   
#   exp_fit <- nls(weight ~ SSasymp(day, yf, y0, log_alpha), data = weight_id)
#   
#   
#   fitted %>% 
#     unnest(tidied) %>% 
#     select(sensor, term, estimate) %>% 
#     spread(term, estimate) %>% 
#     mutate(alpha = exp(log_alpha))
#   # predict_d2 <- predict(exp_fit, deriv = 2)
#   # stress_day_1 <- as.matrix(uniroot.all(approxfun(predict_d2$x, predict_d2$y),
#   #                                       interval = range(predict_d2$x)))
#   
#   # colnames(alpha) <- ID
#   
#   stress_day <- merge(stress_day, stress_day_1, by = 0, all = T) 
#   stress_day <- stress_day %>% 
#     select(-`Row.names`)
# }


# Fit the data
fitted <- morph %>% 
  filter(!is.na(weight), water == "drought") %>% 
  nest(data = -TreeID) %>%
  mutate(fitmod = map(data, ~ lm(log(weight) ~ day, data = .))) %>% 
  mutate(tidied = map(fitmod, tidy)) %>% 
  mutate(augmented = map(.x = fitmod, ~ augment))

# # Produce a table of fit parameters: y0, yf, alpha
# fitted %>% 
#   unnest(tidied) %>% 
#   select(sensor, term, estimate) %>% 
#   spread(term, estimate) %>% 
#   mutate(alpha = exp(log_alpha))



######################################
ggplot(stress)+
  geom_histogram(aes(x = day), binwidth = 1)+
  facet_wrap(~spp)

ggplot(stress)+
  geom_boxplot(aes(x = (day), y = spp))

ggplot(filter(morph, water == "drought"), aes(x = day, y = weight))+
  # geom_line(aes(color = spp, group = TreeID), alpha = 0.5)+
  geom_smooth(aes(group = TreeID, color = spp), se = F, alpha = 0.1)+
  facet_wrap(~spp)+
  geom_vline(xintercept = 4)

ggplot(filter(morph, water == "drought"), aes(x = day, y = weight))+
  # geom_line(aes(color = spp, group = TreeID), alpha = 0.5)+
  geom_smooth(aes(group = TreeID, color = spp), se = F, alpha = 0.1)+
  facet_wrap(~spp)+
  geom_vline(xintercept = 18)














###############
                              # Diameter analysis #

morph_sub <- filter(morph, TreeID %in% sub4phys)

ggplot(morph_sub, aes(x = day, y = Diam_mm))+
  # geom_boxplot(alpha = 0.3, aes(group = interaction(date, spp), fill = spp))+
  # geom_point()+
  geom_boxplot(aes(group = interaction(water, day)))+
  geom_smooth(method = "lm", se = T, linewidth = 0.1, aes(fill = water))+
  facet_wrap(~spp)+
  theme_minimal(base_size = 20)+
  labs(x = "Day", y = "Stem diameter (mm)", fill = "Water")

ggplot(morph, aes(x = week, y = Diam_mm))+
  # geom_smooth(method = "lm", aes(fill = water))+
  # geom_boxplot(aes(group = interaction(date, spp), fill = spp))+
  geom_line(aes(group = TreeID), alpha = 0.4)+
  facet_wrap(~interaction(water, spp), nrow = 4)+
  theme_light()+
  labs(x = "Date", y = "Stem diameter (mm) ", shape = "Species")

ggplot(filter(morph, date %in% c(as.POSIXct("2025-07-23"), as.POSIXct("2025-07-24"))), 
       aes(x = spp, y = Diam_mm))+
  geom_boxplot(aes(group = interaction(spp, water, temp), fill = water))+
  facet_wrap(~temp)+
  theme_light()

ggplot(filter(morph, date %in% c(as.POSIXct("2025-07-23"), as.POSIXct("2025-07-24"))),
       aes(x = Diam_mm))+
  geom_density(aes(fill = spp), alpha = 0.4)+
  facet_wrap(~temp)+
  theme_light()

ggplot(morph, aes(x = week, y = Diam_mm))+
  geom_line(aes(group = TreeID), alpha = 0.4)+
  geom_smooth(method = "lm", aes(group = spp))+
  # geom_boxplot(aes(group = interaction(date, spp), fill = spp))+
  facet_wrap(~(water), nrow = 4)+
  theme_light()+
  labs(x = "Date", y = "Stem diameter (mm) ", shape = "Species")

morph_stats <- morph %>% 
  group_by(TreeID) %>% 
  summarise(max_weight = max(Pot_weight_g))

morph <- morph %>% 
  full_join(morph_stats) %>% 
  mutate(weight_frac = Pot_weight_g/max_weight)

# write_csv()






soil <- read_csv("data/Experiment/Processed/VWC.csv")

soil_comp <- inner_join(soil, morph, by = join_by(date, TreeID, spp, water, temp, id)) %>% 
  mutate(el_group = case_when(spp %in% c("PSME", "PIPO") ~ "low elevation",
                              .default = "high elevation"))


# relativize weight and VWC

comp_stats <- soil_comp %>% 
  group_by(TreeID) %>%
  summarise(max_weight = max(Pot_weight_g),
            max_vwc = max(VWC_perc))


soil_comp2 <- soil_comp %>% 
  full_join(comp_stats) %>% 
  mutate(weight_frac = Pot_weight_g/max_weight,
         vwc_frac = VWC_perc/max_vwc)
  
cor.test(soil_comp2$weight_frac, soil_comp2$vwc_frac)
  
  # arrange(date, .by_group = T)

# by fraction:
ggplot(filter(soil_comp2, water == "drought"), 
       aes(x = weight_frac, y = vwc_frac))+
  geom_smooth(aes(color = spp), se = T, method = "lm")+
  # geom_path(aes(group = TreeID), lineend = "square")+
  # geom_smooth(method = "lm", se = T, aes(linetype = temp, group = spp))+
  # facet_wrap(~interaction(temp, el_group))+
  labs(x = "% of max weight", y = "% of max VWC")+
  theme_light(base_size = 24)

# mean(filter(soil_comp2, 
#             water == "drought",,
#             date >= "2025-07-30"),
#      )
# 
# ggplot(filter(soil_comp2, 
#               water == "drought",,
#               date >= "2025-07-30"), 
#        aes(x = spp, y = vwc_frac, color = temp))+
#   # geom_boxplot()+
#   geom_point()+
#   labs(x = "Spp", y = "% of max VWC")+
#   theme_light(base_size = 24)


# to assess for heatwave criteria:
soil_avgs <- filter(soil_comp2, 
                    water == "drought",,
                    date >= "2025-08-01") %>%  
  # update with more recent date after this week
  group_by(spp) %>% 
  summarise(mean_vwc_frac = mean(vwc_frac))
# want to see either ALL below 0.75 or ONE below 0.25


# by VWC and mass:
ggplot(filter(soil_comp2), 
       aes(x = Pot_weight_g, y = VWC_perc))+
  geom_point(aes(color = spp))+
  # geom_path(aes(group = TreeID), lineend = "square")+
  geom_smooth(method = "lm", se = F, aes(linetype = temp))+
  # facet_wrap(~interaction(temp, el_group))+
  labs(x = "Pot weight (g)", y = "Soil moisture (%)", linetype = "chamber")+
  theme_light(base_size = 24)
# ggsave("figures/VWC_v_weight.png", last_plot(), width = 8, height = 6)

cor.test(soil_comp2$Pot_weight_g, soil_comp2$VWC_perc)

ggplot(filter(soil_comp2, temp == "heatwave" & water == "drought"), 
       aes(x = water, y = vwc_frac, fill = spp))+
  geom_point()+
  facet_wrap(~spp)+
  labs(x = "Chamber", y = "% of maximum VWC on 7/30/2025", title = "Soil moisture in heatwave chamber")+
  theme_light(base_size = 26)
# ggsave("figures/VWC_frac_07302025.png", last_plot(), width = 9, height = 6)

ggplot(filter(morph, date >= "2025-08-01" & water == "drought"), 
       aes(x = spp, y = weight_frac))+
  geom_boxplot(alpha = 0.4)+
  geom_point()+
  labs(x = "Chamber", y = "% of max pot weight, week 2", title = "Soil moisture in heatwave chamber")+
  theme_light(base_size = 24)
# ggsave("figures/Weight_frac_07302025.png", last_plot(), width = 9, height = 6)



##################

cor.test(soil_comp$VWC_perc, soil_comp$Pot_weight_g)

ggplot(soil_comp, aes(x = Pot_weight_g, y = VWC_perc))+
  geom_point(aes(color = spp))+
 # geom_path(aes(group = TreeID), lineend = "square")+
  geom_smooth(method = "lm")+
  facet_wrap(~interaction(temp, el_group))

