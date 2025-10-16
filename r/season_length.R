####################
# Snow Season Length Per Year
####################

# get data
snow <- read_csv("data/full_snow.csv")

# Station Length
snow %>% 
  group_by(station_name) %>% 
  mutate(month_len = round(time_length(interval(start, end), unit = "month"), 0)) %>% 
  summarise(month_len = mean(month_len)) %>% 
  top_n(30) %>% 
  ggplot(aes(x = fct_reorder(station_name, month_len), y = month_len, fill = station_name)) +
  geom_bar(stat = "identity", width = .5) +
  coord_flip() +
  theme(legend.position = "none") +
  labs(y = "# of Months Reporting",
       x = "Station",
       title = "Stations Duration in Reporting")

# Temperature and number of 15 day periods with snow depth
snow_day <- snow %>% 
  filter(!mon %in% c("Aug", "Sep")) %>% # these have 0 snow
  mutate(date = date + months(3), # start October as January
         year = year(date),
         if_snow = case_when(
           start_snow_depth == 0 ~ 0,
           TRUE ~ 1)) %>% 
  group_by(year, station_name) %>% 
  summarize(snow_days = sum(if_snow)*15,
            elev = mean(elev),
            mean_temp = round((mean(max_temp_mean) + mean(min_temp_mean)/2),0),
            count = n()) %>% 
  mutate(elev_level = case_when(
    elev > 8000 & elev < 9000 ~ "8K-9K",
    elev > 9000 & elev < 10000 ~ "9K-10K",
    elev > 10000 & elev < 11000 ~ "10K-11K",
    elev > 11000 & elev < 12000 ~ "11K-12K"),
    elev_level = fct_relevel(elev_level, 
                             levels = c("8K-9K", 
                                        "9K-10K", 
                                        "10K-11K",
                                        "11K-12K")),
    elev_fit = case_when(
      elev < 9250 ~ 9000,
      elev < 9750 & elev > 9249 ~ 9500,
      elev < 10250 & elev > 9749 ~ 10000,
      elev < 10750 & elev > 10249 ~ 10500,
      elev < 11250 & elev > 10749 ~ 11000,
      elev < 11750 & elev > 10749 ~ 11500,
      TRUE ~ 12000
    )) %>% 
  filter(count > 9, year != 2018)

# Quick regression
lm.snow <- lm(snow_days ~ mean_temp*as.factor(elev_fit), data = snow_day)
summary(lm.snow)

# faceted regression
snow_day %>% 
  ggplot(aes(x = mean_temp, y = snow_days, color = as.factor(elev_fit))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_bw() +
  facet_wrap(~as.factor(elev_fit), nrow = 4) +
  theme(panel.grid.major = element_line(color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = rel(1.5)))

# or
snow_day %>% 
  ggplot(aes(x = mean_temp, y = snow_days, color = as.factor(elev_fit))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_bw() +
  facet_wrap(~as.factor(elev_fit), nrow = 1) +
  labs(x = "Mean Temperature",
       y = "Days With Snow",
       subtitle = "Temperature Calculated October - July") +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = rel(1.5)),
        legend.position = "none",
        axis.title = element_text(size = rel(2), face = "bold"),
        plot.subtitle = element_text(size = rel(2), face = "bold", color = "blue"))


# 3D plot
plot_ly(snow_day, 
        x = ~elev_fit, 
        y = ~mean_temp,
        z = ~snow_days,
        color = ~elev_fit) %>% 
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'Elevation levels'),
                      yaxis = list(title = 'Average Temp (F)'),
                      zaxis = list(title = 'Days of Snow')))

# subset data for yearly regression
snow_lm <- snow_day %>% 
  group_by(year) %>% 
  summarise(mean_temp = round(mean(mean_temp),1),
            snow_days = round(mean(snow_days),0),
            obs_num = n())

# perfrom lm
lm.snow <- lm(snow_days ~ mean_temp, data = snow_lm)
summary(lm.snow)

# create plot
ggplot(snow_lm, aes(x = mean_temp, y = snow_days)) +
  geom_point(size = 2, aes(size = obs_num)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(x = "Mean Temperature",
       y = "Days With Snowpack",
       title = "Linear Days With Snowpack and Temperature",
       subtitle = ".25 correlation") +
  theme(plot.title = element_text(size = rel(2), face = "bold", hjust = .5),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "none",
        plot.subtitle = element_text(face = "bold", size = rel(1.5), color = "red"))

# snow days over years
snow_lm %>% 
  ggplot(aes(x = year, y = snow_days)) +
  geom_point(aes(size = obs_num)) +
  theme_bw()

snow_day %>% 
  ggplot(aes(x = year, y = snow_days, color = as.factor(year))) +
  geom_jitter() +
  geom_boxplot(color = "black", 
               aes(group = as.factor(year)),
               outlier.shape = NA) +
  theme_bw() +
  labs(x = "Year",
       y = "Days With Snowpack",
       title = "Snowpack Days By Year",
       subtitle = "LM Says negative trend, but looks pretty linear here") +
  theme(plot.title = element_text(size = rel(2), face = "bold", hjust = .5),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = "none",
        plot.subtitle = element_text(face = "bold", size = rel(1.5), color = "red"))

# Deviates from mean
snow_dev <- snow_day %>% 
  group_by(year) %>% 
  summarise(snow_days = mean(snow_days)) %>% 
  mutate(dev = (snow_days - mean(snow_days))/(max(snow_days) - min(snow_days)))

# graphed
snow_dev %>% 
  ggplot(aes(x = year, y = dev)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) 


#### THOUGHTS
# The point of this was to see any trend. If mean_temp = x, how many
# snow days per elevation?
# With warming temps, it looks like they all have less snow days at same pace
#
