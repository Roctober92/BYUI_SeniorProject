################################
## Analysis of shape and nature of temperature data
## Possible outliers exposed through boxplots
## Used in determining whether dropping/estimating temp values needed
# before regression
## Data saved to be used in na_knn.R
################################

# Get data
pre_data <- read_csv("data/na_prep_predata.csv")

# So we'll separate them to one at a time: 64 Rows
data.max.na <- pre_data %>% 
  filter(!is.na(min_temp_mean),
         is.na(max_temp_mean))

# 50 Rows: since some max seem bad, we'll refit end 4 values on each side.
data.min.na <- pre_data %>% 
  filter(is.na(min_temp_mean),
         !is.na(max_temp_mean))



# histogram
data.min.na %>% 
  mutate(mon = as.factor(month(date, label = TRUE))) %>% 
  ggplot(aes(x = max_temp_mean)) +
  geom_histogram(bins = 80, fill = "red", color = "black") +
  theme_bw() 

data.max.na %>% 
  mutate(mon = as.factor(month(date, label = TRUE))) %>% 
  ggplot(aes(x = min_temp_mean)) +
  geom_histogram(bins = 80, fill = "red", color = "black") +
  theme_bw() 



# boxplot
data.min.na %>% 
  mutate(mon = as.factor(month(date, label = TRUE))) %>%
  ggplot(aes(x = mon, y = max_temp_mean)) +
  geom_jitter(width = .2) +
  geom_boxplot(alpha = .2, outlier.shape = NA) +
  theme_bw() +
  labs(x = "Month",
       y = "15 Daily Average High (F)",
       title = "Checking Value Validity",
       color = "",
       subtitle = "The 100+ dots and below 25 summer month dots are concerning") +
  theme(plot.title = element_text(hjust = .5, size = rel(2)),
        axis.title = element_text(size = rel(1.5)),
        plot.subtitle = element_text(face = "bold"),
        legend.position = "none")

# boxplot
data.max.na %>% 
  mutate(mon = as.factor(month(date, label = TRUE))) %>%
  ggplot(aes(x = mon, y = min_temp_mean)) +
  geom_jitter(width = .2) +
  geom_boxplot(alpha = .2, outlier.shape = NA) +
  theme_bw() +
  labs(x = "Month",
       y = "15 Daily Average Min (F)",
       title = "Checking Value Validity",
       color = "",
       subtitle = "Just at a glance, no values looks extremely unreasonable") +
  theme(plot.title = element_text(hjust = .5, size = rel(2)),
        axis.title = element_text(size = rel(1.5)),
        plot.subtitle = element_text(face = "bold"),
        legend.position = "none")

# we are going to intuitively turn all non-sensical Max temp values
# to integers, and add them to (both_na)
data.min.na.2 <- data.min.na %>% 
  mutate(season = case_when(
    month(date) > 5 & month(date) < 11 ~ "summer",
    TRUE ~ "other"),
    temp = case_when(
      max_temp_mean > 100 ~ NA_integer_,
      max_temp_mean < 40 & season == "summer" ~ NA_integer_,
      TRUE ~ max_temp_mean),
    max_temp_mean = temp) %>% 
  select(-c(season, temp))

# keep non-na max temp values
data.min.na.3 <- data.min.na.2 %>% filter(!is.na(max_temp_mean))

# filter out NA to add to both.na
bad.min <- data.min.na.2 %>% filter(is.na(max_temp_mean))

# Check NA count: 7,029 Rows
# There are only 114 rows where only 1 of the 2 columns is NA
data.both.na <- pre_data %>% 
  filter(is.na(max_temp_mean),
         is.na(min_temp_mean)) %>% 
  rbind(bad.min)


# Get rid of temp NAs: 67,944 Rows
data.2 <- pre_data %>% 
  filter(!is.na(max_temp_mean),
         !is.na(min_temp_mean)) %>% 
  mutate(mon = month(date, label = TRUE),
         avg_temp = round((max_temp_mean + min_temp_mean)/2, 1),
         year = year(date))

# subset 32 degree avg temperatures: 922 Rows
data.32 <- data.2 %>% 
  filter(max_temp_mean == 32 & min_temp_mean == 32)

# take the compliment of the above
data.3 <- data.2 %>% 
  filter(max_temp_mean != 32 | min_temp_mean != 32)



#### Data subsets
# This will take top .2% of each month's temp
# keep grouped, to avoid repeating values in high frequency months

# top .8% of data
black_dots_max <- data.3 %>%
  group_by(mon) %>%
  mutate(rank = rank(desc(max_temp_mean))) %>%
  filter(rank < max(unique(rank))/125) #lower than .8% bench mark

# bottom .8% of data
red_dots_max <- data.3 %>%
  group_by(mon) %>%
  mutate(rank = rank(desc(max_temp_mean))) %>%
  filter(rank > max(unique(rank)) - max(unique(rank))/125)

# middle 98.4% of data
regular_dots_max <- data.3 %>% 
  group_by(mon) %>%
  mutate(rank = rank(desc(max_temp_mean))) %>%
  filter(rank > max(unique(rank))/125 & rank < max(unique(rank)) - max(unique(rank))/125)





#top .8% of data
black_dots_mean <- data.3 %>%
  group_by(mon) %>%
  mutate(rank = rank(desc(avg_temp))) %>%
  filter(rank < max(unique(rank))/125)

# bottom .8% of data
red_dots_mean <- data.3 %>%
  group_by(mon) %>%
  mutate(rank = rank(desc(avg_temp))) %>%
  filter(rank > max(unique(rank)) - max(unique(rank))/125)

# middle 98.4% of data
regular_dots_mean <- data.3 %>%
  group_by(mon) %>%
  mutate(rank = rank(desc(avg_temp))) %>%
  filter(rank > max(unique(rank))/125 & rank < max(unique(rank)) - max(unique(rank))/125)






# top .8% of data
black_dots_min <- data.3 %>% 
  group_by(mon) %>% 
  mutate(rank = rank(desc(min_temp_mean))) %>% 
  filter(rank < max(unique(rank))/125)

# bottom .8% of data
red_dots_min <- data.3 %>% 
  group_by(mon) %>% 
  mutate(rank = rank(desc(min_temp_mean))) %>% 
  filter(rank > max(unique(rank)) - max(unique(rank))/62.5)

# middle 98.4% of data
regular_dots_min <- data.3 %>% 
  group_by(mon) %>% 
  mutate(rank = rank(desc(min_temp_mean))) %>% 
  filter(rank > max(unique(rank))/125 & rank < max(unique(rank)) - max(unique(rank))/62.5)





### Save all subsets
write_csv(data.32, path = "/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_knn/data32.csv")
write_csv(black_dots_max, path = "/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_knn/black_dots_max.csv")
write_csv(black_dots_min, path = "/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_knn/black_dots_min.csv")
write_csv(regular_dots_max, path = "/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_knn/regular_dots_max.csv")
write_csv(regular_dots_min, path = "/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_knn/regular_dots_min.csv")
write_csv(red_dots_max, path = "/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_knn/red_dots_max.csv")
write_csv(red_dots_min, path = "/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_knn/red_dots_min.csv")
write_csv(data.both.na, path = "/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_knn/data.both.na.csv")
write_csv(data.max.na, path = "/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_knn/data.max.na.csv")
write_csv(data.min.na.3, path = "/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_knn/data.min.na.csv")


### Visualizations

# month re-level function
sort_month <- function(x) {
  x %>% 
    mutate(mon = fct_relevel(mon, levels = c("Jan",
                                             "Feb",
                                             "Mar",
                                             "Apr",
                                             "May",
                                             "Jun",
                                             "Jul",
                                             "Aug",
                                             "Sep",
                                             "Oct",
                                             "Nov",
                                             "Dec")))
}


# 32 Degree temperature anamoly
data.3 %>% 
  sort_month() %>% 
  ggplot(aes(x = mon, y = avg_temp)) +
  geom_jitter(aes(color = mon), width = .2, alpha = .3) +
  geom_jitter(data = data.32, 
              aes(x = mon, y = avg_temp),
              color = "black", width = .2) +
  geom_boxplot(outlier.shape = NA, width = .4, alpha = .5) +
  theme_bw() +
  labs(x = "Month",
       y = "15 Daily Average Temp (F)",
       title = "Visualizing Temperature Observations",
       color = "",
       subtitle = "Black dots emphasize 32 degree observation anamoly ") +
  theme(plot.title = element_text(hjust = .5, size = rel(2)),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.5)),
        plot.subtitle = element_text(face = "bold", size = rel(1.5)),
        legend.position = "none") +
  ylim(-50,120)




# HIGH TEMP BOXPLOT
regular_dots_max %>% 
  ungroup() %>% # can't relevel a grouping variable
  sort_month() %>% 
  ggplot(aes(x = mon, y = max_temp_mean)) +
  geom_jitter(aes(color = mon), width = .2, alpha = .3) +
  geom_jitter(data = black_dots_max, 
              aes(x = mon, y = max_temp_mean),
              color = "black", size = 3, width = .2, alpha = .4) +
  geom_jitter(data = red_dots_max, 
              aes(x = mon, y = max_temp_mean),
              color = "red", size = 3, width = .2, alpha = .4) +
  geom_boxplot(outlier.shape = NA, width = .4) +
  theme_bw() +
  labs(x = "Month",
       y = "15 Day Mean High Temperature",
       title = "Visualizing Temperature Outliers",
       color = "",
       subtitle = "Black and Red top .8% and bottom .8% of data") +
  theme(plot.title = element_text(hjust = .5, size = rel(2), color = "blue"),
        axis.title = element_text(size = rel(1.5), face = "bold"),
        plot.subtitle = element_text(face = "bold", size = rel(1.5)),
        axis.text = element_text(size = rel(1.2)),
        legend.position = "none") +
  ylim(-10, 90)



# Should I get rid of outlier values?
# Will i have to predict temps as well?

# boxplot for average temperatures
# regular_dots_mean %>% 
#   ungroup() %>% 
#   mutate(mon = fct_relevel(mon, levels = c("Jan",
#                                            "Feb",
#                                            "Mar",
#                                            "Apr",
#                                            "May",
#                                            "Jun",
#                                            "Jul",
#                                            "Aug",
#                                            "Sep",
#                                            "Oct",
#                                            "Nov",
#                                            "Dec"))) %>% 
#   ggplot(aes(x = mon, y = avg_temp)) +
#   geom_jitter(data = black_dots_mean, 
#               aes(x = mon, y = avg_temp),
#               color = "black", size = 3, width = .2) +
#   geom_jitter(data = red_dots_mean, 
#               aes(x = mon, y = avg_temp),
#               color = "red", size = 3, width = .2) +
#   geom_jitter(aes(color = mon)) +
#   geom_boxplot(outlier.shape = NA) +
#   theme_bw() +
#   labs(x = "Month",
#        y = "Average Temperature",
#        title = "Visualizing Temperature Outliers",
#        color = "",
#        subtitle = "Black and Red are outside .4% of data") +
#   theme(plot.title = element_text(hjust = .5, size = rel(2)),
#         axis.title = element_text(size = rel(1.5)),
#         plot.subtitle = element_text(face = "bold"))
# some stations have 32 programmed for high and low during summer months






# LOW TEMP BOXPLOT
regular_dots_min %>% 
  ungroup() %>% 
  sort_month() %>% 
  ggplot(aes(x = mon, y = min_temp_mean)) +
  geom_jitter(data = black_dots_min, 
              aes(x = mon, y = min_temp_mean),
              color = "black", size = 3, width = .2, alpha = .4) +
  geom_jitter(data = red_dots_min, 
              aes(x = mon, y = min_temp_mean),
              color = "red", size = 3, width = .2, alpha = .4) +
  geom_jitter(aes(color = mon)) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  labs(x = "Month",
       y = "Mean Low Temperature",
       title = "Visualizing Temperature Outliers",
       color = "",
       subtitle = "Black is top .8% | Red is bottom 1.6%") +
  theme(plot.title = element_text(hjust = .5, size = rel(2), color = "blue"),
        axis.title = element_text(size = rel(1.5), face = "bold"),
        axis.text = element_text(size = rel(1.5)),
        plot.subtitle = element_text(face = "bold", size = rel(1.5)),
        legend.position = "none") +
  ylim(-100, 150)
