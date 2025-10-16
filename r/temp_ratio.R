####################################################
# Finding ratio vs temperature trends
####################################################

# get data
temp_data <- read_csv("data/full_temp.csv")

# add ratio
td2 <- temp_data %>% 
  filter(!is.na(start_snow_water),
         !is.na(start_snow_depth)) %>% 
  mutate(ratio = round(start_snow_depth/start_snow_water, 1),
         avg_temp = round((max_temp_mean + min_temp_mean)/2, 1)) %>% 
  filter(ratio < 12)

# make linear viz
td2 %>% 
  ggplot(aes(x = avg_temp, y = ratio)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = 3) +
  theme_bw() +
  labs(title = "Downwards Trend",
       x = "Average Temp", 
       y = "Snow Ratio",
       subtitle = "Average Temp Calculated by (High+Low)/2") +
  theme(plot.title = element_text(hjust = .5, size = rel(2.5), face = "bold"),
        axis.title = element_text(size = rel(1.5), face = "bold", color = "blue"),
        axis.text = element_text(size = rel(1.5)),
        plot.subtitle = element_text(size = rel(1.5), face = "bold", color = "red"))
