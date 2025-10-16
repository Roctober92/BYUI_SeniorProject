########################
# Check distribution of NA per station, just to make sure we should include all stations
# Also check distrubtion per year
########################

# READ IN DATA
pre_data <- read_csv("/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/na_prep_predata.csv")


# MAKE YEAR COLUMN
year_data <- pre_data %>% 
  mutate(year = year(date),
         mon = month(date, label = TRUE))  

#### NA PER YEAR PLOT
year_plot_data <- year_data %>% 
  group_by(year) %>% 
  summarize(na_count = sum(is.na(start_snow_depth)),
            total = n()) %>% 
  mutate(percent = round(na_count/total, 2),
         year = as.factor(year))

year_plot_data %>% 
  ggplot(aes(x = year, y = percent)) +
    geom_point(aes(size = total)) +
    theme_classic() +
    labs(title = "Percent NA Per Year", 
         x = "Year", 
         y = "Percent NA",
         size = "Total Observations") +
    theme(axis.text.x = element_text(angle = 90),
          plot.title = element_text(hjust = .5, size = rel(2)),
          axis.title = element_text(size = rel(1.5)),
          legend.position = c(.8, .8),
          legend.text = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.5)))


###### Hathaway stuff #######

# Percentage NA per year, and number of observations
year_data %>%
  group_by(year) %>%
  summarize(naperc = sum(is.na(start_snow_depth))/n(), n = n()) %>%
  ggplot(aes(x = year, y = naperc)) + 
  geom_point(aes(size = n)) + 
  geom_text_repel(aes(label = n), nudge_x = -4) +
  scale_x_continuous(breaks = seq(1970,2015, by = 3)) +
  theme_bw()

# For 2003, observation # and NA percentage per station
year_data %>%
  filter(year == 2003) %>%
  group_by(station_name) %>%
  summarize(naperc = sum(is.na(start_snow_depth))/n(), n = n()) %>%
  ggplot(aes(y = station_name, x = naperc)) + 
  geom_point(aes(size = n)) + 
  geom_text(aes(label = n), color = "white") +
  theme_bw()


# Mean Density per station per year
year_data %>%
  group_by(station_name, year)  %>%
  summarise(density_mean = mean(start_snow_water / start_snow_depth, na.rm = TRUE),
            density_sd = sd(start_snow_water / start_snow_depth, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = density_mean)) +
  geom_point() +
  geom_line(aes(group = station_name)) 

## Figure out what the 2.5 value is.
year_data %>%
  group_by(station_name, year)  %>%
  summarise(density_mean = mean(start_snow_water / start_snow_depth, na.rm = TRUE),
            density_sd = sd(start_snow_water / start_snow_depth, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.infinite(density_mean), density_mean < 1) %>%
  ggplot(aes(x = year, y = density_mean)) +
  geom_point() +
  geom_line(aes(group = station_name), alpha = .15) +
  geom_smooth()


# This shows snow-to-rain ratios
lineplot_data <- year_data %>%
  group_by(station_name, year)  %>%
  summarise(ratio_mean = mean(start_snow_depth / start_snow_water, na.rm = TRUE),
            density_sd = sd(start_snow_water / start_snow_depth, na.rm = TRUE))

lineplot_data_two <- lineplot_data %>% 
  filter(ratio_mean < 20) %>% 
  ungroup()
  
ggplot(data = lineplot_data_two, aes(x = year, y = ratio_mean)) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_bw() +
  labs(y = "Depth / Water Equivalent", 
       title = "Colorado SNOTEL Snow to Water",
       x = "Year") +
  theme(plot.title = element_text(size = rel(2), hjust = .5))

###########################

# STATION NA DISTRIBUTION CHECK
# Count station values with and w/o NA values - just for comparison
na_station <- with_na %>% 
  group_by(station_name) %>% 
  summarize(w_count = n())
# Has 104 stations

wo_na_station <- wo_na %>% 
  group_by(station_name) %>% 
  summarize(wo_count = n())
# Has 114 stations

# join tables, calculate NA_ratio
# high ratio stations have highest proportion of NA values
combined_station <- wo_na_station %>% 
  left_join(na_station, by = "station_name") %>% 
  replace_na(list(w_count = 0)) %>% 
  mutate(ratio = round(w_count/wo_count, 2),
         ratio_type = case_when(
           ratio < 1 ~ "less_na",
           TRUE ~ "more_na"
         )) %>% 
  arrange(ratio)

# bar plot
ggplot(combined_station, aes(y = ratio, x = fct_reorder(station_name, ratio))) +
  geom_bar(stat = "identity", aes(fill = ratio_type), width = .5) +
  theme_classic() +
  theme(axis.text.y = element_blank()) +
  coord_flip() +
  labs(y = "Ratio", x = "Weather Station") +
  scale_fill_manual(name = "NA Ratio",
                    labels = c("Less NA", "More NA"),
                    values = c("less_na" = "#176BCF",
                               "more_na" = "#EFC020"))




# YEAR DISTRIBUTION CHECK
na_year <- with_na %>% 
  group_by(year) %>% 
  summarize(w_count = n())
# 35 rows

wo_na_year <- wo_na %>% 
  group_by(year) %>% 
  summarize(wo_count = n())
# 23 rows

combined_year <- na_year %>% 
  full_join(wo_na_year, by = "year") %>% 
  replace_na(list(wo_count = 0)) %>% 
  replace_na(list(w_count = 0)) %>% 
  mutate(plus_minus = wo_count - w_count,
         na_type = case_when(
           plus_minus >0 ~ "less_na",
           TRUE ~ "more_na"
         )) %>% 
  arrange(desc(plus_minus)) 

# bar plot
ggplot(combined_year, aes(y = plus_minus, x = year)) +
  geom_bar(stat = "identity", aes(fill = na_type), width = .5) +
  theme_classic() +
  scale_fill_manual(name = "NA Status",
                    labels = c("More W/O NA", "More NA"),
                    values = c("less_na" = "#00ba38", "more_na" = "#f8766d")) +
  coord_flip() +
  labs(y = "Filled Observations - NA Observations")




