library(tidyverse)
library(AICcmodavg)

death_days_fl <- read_csv("data/Experiment/Processed/Death_Day_Fl-Based.csv") %>% 
  filter(treatment == "drought") %>% 
  mutate(day = death) %>% 
  dplyr::select(TreeID, day)
death_days_col <- read_csv("data/Experiment/Processed/Death_Day_Color-Based.csv")

# set to one of the above:
death_days <- death_days_col

# morphological data:

height <- read_csv("data/Experiment/TreeHeight_ForAnalysis.csv")[,1:2] # height from soil base to top of foliage in mm
names(height) <- c("TreeID", "height")
diam <- read_csv("data/Experiment/Processed/Weight_Diam_Color.csv") %>% 
  mutate(date = date(date)) %>% 
  dplyr::select(TreeID, day, diam_mm, spp, temp) %>% 
  group_by(TreeID, spp, temp) %>% 
  filter(day < 25) %>% 
  summarise(diam = mean(diam_mm)) # avg diameter over the first 3-ish weeks of the experiment?


analysis <- inner_join(death_days, height) %>% 
  inner_join(diam)

hist(analysis$height)
hist(analysis$diam)
hist(analysis$day, breaks = 10)
hist(log(analysis$day), breaks = 10)
hist(sqrt(analysis$day), breaks = 10)

cor.test(analysis$height, analysis$diam) # they are positively correlated tho... # diam may not end up mattering much

model <- lm(day ~ poly(height, 2), data = analysis)
summary(model)
plot(model)

ggplot(analysis, aes(x = height, y = day))+
  geom_point()+
  # geom_smooth()
  geom_line(aes(y = predict(model)),
            alpha = 0.3)+
  theme_minimal(base_size = 20)+
  labs(x = "Starting seedling height (mm)", y = "Day of mortality")
  



