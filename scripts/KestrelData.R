library(tidyverse)

# VPD = 0.611*e^((17.27 × TA)/(237.3 + TA))×((100 - RH))/100

# for temp in celsius and Rh in a percent
calc_vpd <- function(air_temp, rh){
  vpd <- (0.611 * exp(17.27*air_temp/(237.3 + air_temp)))*((100-rh)/100)
  return(vpd)
}

kest <- list.files("data/Kestrel", full.names = T)

renameFunction <- function(x,someNames){
  names(x) <- c("datetime", "temp", "rh")
  return(x)
}


kdat <- lapply(kest, read_csv, skip = 3) %>% 
  lapply(filter, `Data Type` == "point") %>% 
  lapply(select, `FORMATTED DATE_TIME`, `Temperature`, `Relative Humidity`) %>% 
  lapply(renameFunction) %>%
  lapply(mutate, year = year(datetime), datetime = ymd_hms(datetime, tz = "America/Phoenix"),
         temp = as.numeric(temp), rh = as.numeric(rh)) %>% 
  lapply(filter, year >= 2025) %>%
  lapply(select, -year)


#name the list elements using the file names:
names(kdat) <- paste0("kest_", str_sub(kest, start = 14, end = 15))

#convert them to DFs in environment:
list2env(kdat, .GlobalEnv)

# work on code to label 2L, 2R as NA for a set of dates?
# or just specify elsewhere (in analysis) that
# btwn (inclusive of) 08-15-2025 @ 4:30 PM (16:00) and 08-18-2025 @ 1:30 PM (13:30)
# PIPO31-60 and PSME31-60 were in ch1, so they also get K1L and K1R
# (so starting at 2pm on 8/18, they were back in chamber 2)


#label & combine (this is messy)
kest_1L$kest <- "1L"
kest_1R$kest <- "1R"
kest_2L$kest <- "2L"
kest_2R$kest <- "2R"
kest_3L$kest <- "3L"
kest_3R$kest <- "3R"
kest_4L$kest <- "4L"
kest_4R$kest <- "4R"

kest_1L$chamber <- 1
kest_1R$chamber <- 1
kest_2L$chamber <- 2
kest_2R$chamber <- 2
kest_3L$chamber <- 3
kest_3R$chamber <- 3
kest_4L$chamber <- 4
kest_4R$chamber <- 4

chamber_data <- rbind(kest_1L, kest_1R, kest_2L, kest_2R,
                      kest_3L, kest_3R, kest_4L, kest_4R) %>% 
  mutate(set = case_when(kest %in% c("1L", "1R", "2L", "2R") ~ "low elevation",
                         .default = "high elevation"),
         chamber = factor(chamber)) %>% 
  mutate(vpd = calc_vpd(temp, rh))


# chamber_wide <- chamber_data %>% 
#   pivot_wider(names_from = chamber,
#               values_from = temp:rh) %>% 
#   filter(!is.na(temp_4L), doy >= 197) %>% 
#   mutate(temp_diff = temp_4L - temp_1L)

# ggplot(chamber_wide, aes(x = datetime, y = temp_diff))+
#   geom_line()+
#   geom_smooth(method = "lm")



#1L and 1R were in fahrenheit
# chamber_data <- mutate(chamber_data, 
#                        temp = case_when(
#                          chamber %in% c("1L", "1R") ~ (temp-32)*(5/9),
#                          .default = temp)) %>% 
#   mutate(doy = yday(datetime))


ggplot(filter(chamber_data, yday(datetime) >= 240), aes(x = (datetime), y = temp))+
  geom_line(aes(linetype = chamber, color = kest))+
  #geom_point(aes(shape = set))+
  labs(x = "Date", y = "Temperature (ºC)",
       color = "Sensor", shape = "Species group")+
  theme_light(base_size = 26)+
  facet_wrap(~set, ncol = 2)


# ggplot(filter(chamber_data, yday(datetime) >= 202), aes(x = datetime, y = rh))+
#   geom_line(aes(color = chamber))+
#   labs(x = "Date", y = "Relative humidity (%)")
ggplot(filter(chamber_data, yday(datetime) >= 245,
              chamber %in% c(1, 3)), aes(x = datetime, y = rh))+
  geom_line(aes(color = kest))+
  # geom_point(aes(shape = set))+
  labs(x = "Date", y = "Relative humidity (%)",
       color = "Sensor", shape = "Species group")+
  theme_light(base_size = 26)+
  facet_wrap(~chamber)

ggplot(filter(chamber_data, yday(datetime) >= 230), aes(x = datetime, y = vpd))+
  geom_line(aes(color = chamber))+
  # geom_point(aes(shape = set))+
  labs(x = "Date", y = "Vapor pressure deficit (kPa)",
       color = "Sensor", shape = "Species group")+
  theme_light(base_size = 26)

