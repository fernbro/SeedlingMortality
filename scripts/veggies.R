library(tidyverse)

veg <- read_csv("data/veg.csv")

veg_new <- veg %>% 
  pivot_longer(cols = `2024`:`1999`, values_to = "price", names_to = "year")

ggplot(veg_new, aes(x = as.numeric(year), y = price))+
  geom_line(aes(group = Type, color = Type), linewidth = 3)+
  theme_light(base_size = 20)+
  scale_color_manual(values = c("darkgreen", "orange"))+
  labs(x = "Year", y = "Price (Millions $)", title = "Imported Vegetables")
# ggsave("data/Activity.png", last_plot())
