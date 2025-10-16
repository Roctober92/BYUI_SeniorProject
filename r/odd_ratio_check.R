########################
# We are testing to see if the observations where the snow ratio == 10,
# or any values higher have a pattern, or are worth taking a look at
########################

#### Get data
weather_data <- read_csv("/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/na_prep_predata.csv")


#### No snow/SWE NAs & >1, get ratio, keep important columns
clean_data <- weather_data %>% 
  filter(!is.na(start_snow_depth),
         !is.na(start_snow_water),
         start_snow_water > 0,
         start_snow_depth > 0) %>% 
  mutate(snow_ratio = round(start_snow_depth/start_snow_water, 2)) %>% 
  select(station_name, 
         date, snow_ratio, 
         start_snow_water, 
         start_snow_depth)
# Observations where ratio > 10, depth values seem keepable

#### Check 10:1 % per year
ten_year <- clean_data %>% 
  mutate(year = year(date),
         is_ten = case_when(
           snow_ratio == 10 ~ 1,
           TRUE ~ 0
         )) %>% 
  group_by(year) %>% 
  summarise(count = n(),
            is_ten = sum(is_ten))

ten_year %>% 
  ggplot(aes(x = year, y = is_ten)) +
  geom_point(size = 3) +
  theme_bw() +
  geom_text_repel(aes(label = is_ten)) +
  labs(y = "# 10:1 Snow Ratio Observations",
       x = "Year",
       title = "10:1 Snow Ratio Yearly",
       caption = "Is 10:1 ratio pre-programmed? It appears frequently") + 
  theme(plot.title = element_text(hjust = .5, size = rel(2), face = "bold"),
        axis.title = element_text(size = rel(1.5), face = "bold")) +
  scale_x_continuous(breaks = seq(1995, 2018, 1))
