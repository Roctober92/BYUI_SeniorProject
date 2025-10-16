################################
# Prep data for regression
# We only want neat data: measured well, complete
# Includes snow depth, water equivalent, month, and mean temperature
# Values above 0, non NA values
################################


to_prep <- read_csv("data/full_temp.csv")

# DATA PREPARATION
no_na <- to_prep %>% 
  filter(!is.na(start_snow_depth),
         start_snow_depth > 0,
         start_snow_water > 0) %>% 
  select(start_snow_depth, 
         start_snow_water, 
         date, 
         max_temp_mean, 
         min_temp_mean,
         elev) %>% 
  mutate(mon = month(date, label = TRUE),
         avg_temp = round((max_temp_mean + min_temp_mean)/2, 1))

### Justify taking out >5 ratios after 50 inches, show graph
# subset for big ratios
big_ratio <- no_na %>% 
  mutate(ratio = round(start_snow_depth/start_snow_water, 1)) %>% 
  filter(ratio > 5)

# graph it
ggplot(no_na, aes(x = start_snow_water, y = start_snow_depth, color = mon)) +
  geom_point() +
  geom_point(data = big_ratio, color = "black") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(x = "SWE", 
       y = "Snow Depth", 
       title = "Multiple Regression Snow Ratio",
       color = "Month",
       subtitle = "Black dots represent Snow Ratio > 5") +
  theme(axis.text.x = element_text(size = rel(1.5)),
        plot.title = element_text(hjust = .5, size = rel(2)),
        axis.title = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        plot.caption = element_text(size = rel(1.5), face = "bold"),
        plot.subtitle = element_text(size = rel(1.5), face = "bold")) +
  scale_x_continuous(breaks = seq(0, 80, 5)) +
  scale_y_continuous(breaks = seq(0, 250, 25))


# don't include 5,10:1 ratios
no_na.1 <- no_na %>% 
  mutate(ratio = round(start_snow_depth/start_snow_water, 1)) %>%
  filter(start_snow_depth < 50 | ratio < 5, 
         !ratio %in% c(5, 10)) %>% 
  select(-c(date, ratio))

# Save data
write_csv(no_na.1, path = "data/na_prepped_data.csv")
