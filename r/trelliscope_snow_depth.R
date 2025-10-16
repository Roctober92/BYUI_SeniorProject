#######################################
# In depth data analysis using trelliscope
#######################################
library(trelliscopejs)
library(tidyverse)
# get_data
snow_data <- read_csv("data/full_snow.csv")

# set theme
theme_set(theme_bw())
  
# Make app
snow_data %>% 
  ggplot(aes(x = date, y = start_snow_depth, color = data_type)) +
  geom_point() +
  facet_trelliscope(~station_name)
