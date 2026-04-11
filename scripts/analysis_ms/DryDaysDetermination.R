library(tidyverse)

# check for days < 5mm of precip

prov_clim <- read_csv("data/Provenance/Daily_Provenance_Climate.csv")

dry_days <- prov_clim %>% 
  mutate(dry_day = case_when(ppt < 5 ~ T,
                             ppt >= 5 ~ F))

wet_days <- dry_days %>% 
  filter(dry_day == F)

get_dry_pds <- function(df, sp){
  df_sample <- filter(df, spp == sp)
  
  dry_lengths <- df_sample %>% 
    mutate(date = as.POSIXct(date)) %>% 
    arrange(date) %>% 
    mutate(last_wet = lag(date, 1)) %>% 
    mutate(dry_pd = interval(last_wet, date) %/% days(1))
  
  return(dry_lengths)
}


pipo_dry <- get_dry_pds(wet_days, "PIPO") %>% 
  group_by(year) %>% 
  filter(!is.na(dry_pd)) %>% 
  summarise(max_dry = max(dry_pd)) %>% 
  mutate(spp = "PIPO")
psme_dry <- get_dry_pds(wet_days, "PSME") %>% 
  group_by(year) %>% 
  filter(!is.na(dry_pd)) %>% 
  summarise(max_dry = max(dry_pd)) %>% 
  mutate(spp = "PSME")
pifl_dry <- get_dry_pds(wet_days, "PIFL") %>% 
  group_by(year) %>% 
  filter(!is.na(dry_pd)) %>% 
  summarise(max_dry = max(dry_pd)) %>% 
  mutate(spp = "PIFL")
pien_dry <- get_dry_pds(wet_days, "PIEN") %>% 
  group_by(year) %>% 
  filter(!is.na(dry_pd)) %>% 
  summarise(max_dry = max(dry_pd)) %>% 
  mutate(spp = "PIEN")

max_dry <- rbind(pipo_dry, psme_dry, pifl_dry, pien_dry)

ggplot()+
  geom_density(data = pipo_dry, aes(x = max_dry), fill = "red", alpha = 0.2)+
  geom_density(data = psme_dry, aes(x = max_dry), fill = "blue", alpha = 0.2, linetype = 2)+
  geom_density(data = pifl_dry, aes(x = max_dry), fill = "orange", alpha = 0.2, linetype = 3)+
  geom_density(data = pien_dry, aes(x = max_dry), fill = "pink", alpha = 0.2, linetype = 4)

ggplot(max_dry, aes(x = spp, y = max_dry))+
  geom_boxplot()+
  theme_minimal()+
  labs(y = "Longest dry period (days)",
       title = "Annual dry periods (ppt < 5 mm), 1984-2024")
