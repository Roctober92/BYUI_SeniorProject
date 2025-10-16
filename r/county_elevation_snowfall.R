####################
# Snow fall per Per County and elevation
####################

# get data
snow <- read_csv("data/full_snow.csv")

# theme set
theme_set(theme_bw())

# Code in elevations, order them
snow.1 <- snow %>% 
  mutate(elev_level = case_when(
           elev > 8000 & elev < 9000 ~ "8K-9K",
           elev > 9000 & elev < 10000 ~ "9K-10K",
           elev > 10000 & elev < 11000 ~ "10K-11K",
           elev > 11000 & elev < 12000 ~ "11K-12K"),
         elev_level = fct_relevel(elev_level, 
                                  levels = c("8K-9K", 
                                             "9K-10K", 
                                             "10K-11K",
                                             "11K-12K")))

# per county Graphic
snow.1 %>% 
  ggplot(aes(x = date, y = start_snow_depth, color = elev_level)) +
  geom_line() +
  facet_wrap(~county, nrow = 35, scale = "free_y")

# per elevation graphic
snow.1 %>% 
  ggplot(aes(x = date, y = start_snow_depth, color = elev_level)) +
  geom_line() 

# Yearly max depth by elevation 
snow.1 %>% 
  mutate(elev = as.factor(elev)) %>% 
  group_by(elev, year) %>% 
  summarise(top_snowpack = max(start_snow_depth)) %>% 
  ggplot(aes(x = year, y = top_snowpack, color = elev)) +
  geom_line() +
  theme(legend.position = "none")

# yearly max by elevation level
snow.1 %>% 
  group_by(elev_level, year) %>% 
  summarise(top_snowpack = max(start_snow_depth)) %>% 
  ggplot(aes(x = year, y = top_snowpack, color = elev_level)) +
  geom_line(size = 3) +
  labs(x = "Year",
       y = "Max Snowpack",
       color = "Elevation Level",
       title = "Yearly Max Snow per Elevation Level") +
  theme(plot.title = element_text(size = rel(2), face = "bold", hjust = .5),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)))
  
