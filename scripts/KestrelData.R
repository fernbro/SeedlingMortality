library(tidyverse)

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
         chamber = factor(chamber))


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


ggplot(filter(chamber_data, yday(datetime) >= 210), aes(x = datetime, y = temp))+
  geom_line(aes(linetype = chamber, color = kest))+
  #geom_point(aes(shape = set))+
  labs(x = "Date", y = "Temperature (ºC)",
       color = "Sensor", shape = "Species group")+
  theme_light(base_size = 26)+
  facet_wrap(~set)
# ggplot(filter(chamber_data, yday(datetime) >= 202), aes(x = datetime, y = rh))+
#   geom_line(aes(color = chamber))+
#   labs(x = "Date", y = "Relative humidity (%)")
ggplot(filter(chamber_data, yday(datetime) >= 208), aes(x = datetime, y = rh))+
  geom_line(aes(color = chamber))+
  geom_point(aes(shape = set))+
  labs(x = "Date", y = "Relative humidity (%)",
       color = "Sensor", shape = "Species group")+
  theme_light(base_size = 26)

