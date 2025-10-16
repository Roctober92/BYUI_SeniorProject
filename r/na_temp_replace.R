########################
# Retrieves predicted data
# Adds predicted data to regular dataframes
# Prediction made with 10 nearest neighbors
########################


#### Import changed data
high_32_preds <- read_csv("data/knn_to_r/high_32_preds.csv")
low_32_preds <- read_csv("data/knn_to_r/low_32_preds.csv")
high_max_preds <- read_csv("data/knn_to_r/high_max_preds.csv")
low_max_preds <- read_csv("data/knn_to_r/low_max_preds.csv")
high_min_preds <- read_csv("data/knn_to_r/high_min_preds.csv")
low_min_preds <- read_csv("data/knn_to_r/low_min_preds.csv")


#### Take away X1
high_32_pred <- high_32_preds %>% 
  select(-X1)

low_32_pred <- low_32_preds %>% 
  select(-X1)

high_max_pred <- high_max_preds %>% 
  select(-X1)

low_max_pred <- low_max_preds %>% 
  select(-X1)

high_min_pred <- high_min_preds %>% 
  select(-X1)

low_min_pred <- low_min_preds %>% 
  select(-X1)


#### Load in data from na_tempdata_analysis.R
data_32 <- read_csv("data/to_knn/data32.csv")
high_max <- read_csv("data/to_knn/black_dots_max.csv")
high_min <- read_csv("data/to_knn/black_dots_min.csv")
max_mid <- read_csv("data/to_knn/regular_dots_max.csv")
min_mid <- read_csv("data/to_knn/regular_dots_min.csv")
low_max <- read_csv("data/to_knn/red_dots_max.csv")
low_min <- read_csv("data/to_knn/red_dots_min.csv")


####### Combine predictions to corresponding datasets ##########

### Data 32
data_32_final <- cbind(data_32, high_32_pred, low_32_pred)

# Replace 32 with predictions
final.32 <- data_32_final %>% 
  rename("high_preds" = "predictions_high_32_preds",
         "low_preds" = "predictions_low_32_preds") %>% 
  mutate(max_temp_mean = high_preds,
         min_temp_mean = low_preds) %>% 
  select(-c(high_preds, low_preds))


# Plot Monthly Means for Changed temperatures
final.32 %>% 
  group_by(mon) %>% 
  summarise(high_mean = round(mean(max_temp_mean), 0),
            low_mean = round(mean(min_temp_mean), 0)) %>% 
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
                                           "Dec"))) %>% 
  ggplot(aes(x = mon, y = high_mean)) +
  geom_point(size = 9) +
  geom_point(aes(y = low_mean), color = "green", size = 9) +
  theme_bw() +
  geom_text(aes(label = high_mean), color = "white") +
  geom_text(aes(y = low_mean, label = low_mean)) +
  labs(title = "Repredicted Temps for 32 Anamoly",
       x = "Month",
       y = "15 Day Avg High") +
  theme(plot.title = element_text(hjust = .5, size = rel(2)),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.2)))

# Visualization of Mean Temp per Elevation
final.32 %>% 
  #mutate(elev = as.factor(elev)) %>% 
  group_by(mon, elev) %>% 
  summarise(avg_mean = round((mean(max_temp_mean) + mean(min_temp_mean))/2, 1)) %>% 
  ungroup() %>% 
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
                                           "Dec"))) %>% 
  ggplot(aes(x = mon, y = avg_mean, color = elev)) +
  geom_point() +
  geom_line(aes(group = elev)) +
  scale_color_viridis_c(option = "inferno", direction = -1) +
  theme_bw() +
  labs(title = "Replaced Temp Values For 32 Degree Anamolies",
       x = "Month",
       y = "Average Temp Per 15 Days Per Station",
       color = "Elevation",
       caption = "Not all elevations had 32 degree anamalies reported every month. This explains lines with only a few month observations.") +
  theme(plot.title = element_text(hjust = .5, size = rel(2)),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.2)))
  
### Max Data

# Combine datasets
max_high <- cbind(high_max, high_max_pred) 
max_low <- cbind(low_max, low_max_pred)

# Check observed/replaced
max_high %>% 
  group_by(mon) %>% 
  summarize(mean_before = round(mean(max_temp_mean), 0),
            mean_after = round(mean(predictions_high_max_preds)), 0) %>% 
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
                                           "Dec"))) %>%
  ggplot(aes(x = mon)) +
  geom_point(aes(y = mean_before), color = "black", size = 9) +
  geom_point(aes(y = mean_after), color = "red", size = 9) + 
  geom_text(aes(y = mean_before, label = mean_before), color = "white") +
  geom_text(aes(y = mean_after, label = mean_after)) +
  theme_bw() +
  labs(title = "Original vs Predicted 15 Day Mean Max Temps",
       x = "Month",
       y = "Average 15 Day Max Temp Per Station",
       subtitle = "Black = Original | Red = Predicted",
       caption = "This is the top .2% outlier data") +
  theme(plot.title = element_text(hjust = .5, size = rel(2)),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.2)),
        plot.caption = element_text(size = rel(1.5)))

max_low %>% 
  group_by(mon) %>% 
  summarize(mean_before = round(mean(max_temp_mean), 0),
            mean_after = round(mean(predictions_low_max_preds)), 0) %>% 
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
                                           "Dec"))) %>%
  ggplot(aes(x = mon)) +
  geom_point(aes(y = mean_before), color = "red", size = 9) +
  geom_point(aes(y = mean_after), color = "black", size = 9) + 
  geom_text(aes(y = mean_before, label = mean_before)) +
  geom_text(aes(y = mean_after, label = mean_after), color = "white") +
  theme_bw() +
  labs(title = "Original vs Predicted 15 Day Mean Max Temps",
       x = "Month",
       y = "Average 15 Day Max Temp Per Station",
       subtitle = "Black = Original | Red = Predicted",
       caption = "This is the bottom .2% outlier data") +
  theme(plot.title = element_text(hjust = .5, size = rel(2)),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.2)),
        plot.caption = element_text(size = rel(1.5)))

# Combine all Max data together, replace Max temp values
max_high.2 <- max_high %>% 
  mutate(max_temp_mean = predictions_high_max_preds) %>% 
  select(-predictions_high_max_preds) # preds --> original

max_low.2 <- max_low %>% 
  mutate(max_temp_mean = predictions_low_max_preds) %>% 
  select(-predictions_low_max_preds)

max_final <- rbind(max_high.2, max_mid, max_low.2) %>%
  mutate(mon = fct_relevel(mon, levels = c("Jan", # months in order in vis
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
  

# Boxplot to check data shape

max_final %>% 
  ggplot(aes(x = mon, y = max_temp_mean, color = mon)) +
  geom_jitter(width = .2, alpha = .05) +
  geom_boxplot(outlier.shape = NA, color = "darkgrey", fill = NA) +
  geom_jitter(data = max_high.2, 
              aes(y = max_temp_mean), 
              color  = "black", 
              width = .2) +
  geom_jitter(data = max_low.2, 
              aes(y = max_temp_mean), 
              color  = "brown", 
              width = .2) +
  theme_bw() +
  labs(title = "15 Day Max Temps After Predictions Added",
       x = "Month",
       y = "Average 15 Day Max Temp Per Station",
       caption = "32 Degree Temps Not Added In Yet",
       subtitle = "Black = Former Top .8% Data | Red = Former Bottom .8% Data") +
  theme(plot.title = element_text(hjust = .5, size = rel(2)),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.2)),
        plot.caption = element_text(size = rel(1.5)),
        plot.subtitle = element_text(size = rel(1.5)),
        legend.position = "none")


### Min Data

# Combine datasets
min_high <- cbind(high_min, high_min_pred) 
min_low <- cbind(low_min, low_min_pred)

# Check observed/replaced
min_high %>% 
  group_by(mon) %>% 
  summarize(mean_before = round(mean(min_temp_mean), 0),
            mean_after = round(mean(predictions_high_min_preds)), 0) %>% 
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
                                           "Dec"))) %>%
  ggplot(aes(x = mon)) +
  geom_point(aes(y = mean_before), color = "black", size = 9) +
  geom_point(aes(y = mean_after), color = "red", size = 9) + 
  geom_text(aes(y = mean_before, label = mean_before), color = "white") +
  geom_text(aes(y = mean_after, label = mean_after)) +
  theme_bw() +
  labs(title = "Original vs Predicted 15 Day Mean Min Temps",
       x = "Month",
       y = "Average 15 Day Minimum Temp Per Station",
       subtitle = "Black = Original | Red = Predicted",
       caption = "This is the top .2% outlier data") +
  theme(plot.title = element_text(hjust = .5, size = rel(2)),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.2)),
        plot.caption = element_text(size = rel(1.5)))

min_low %>% 
  group_by(mon) %>% 
  summarize(mean_before = round(mean(min_temp_mean), 0),
            mean_after = round(mean(predictions_low_min_preds)), 0) %>% 
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
                                           "Dec"))) %>%
  ggplot(aes(x = mon)) +
  geom_point(aes(y = mean_before), color = "black", size = 9) +
  geom_point(aes(y = mean_after), color = "red", size = 9) + 
  geom_text(aes(y = mean_before, label = mean_before), color = "white") +
  geom_text(aes(y = mean_after, label = mean_after)) +
  theme_bw() +
  labs(title = "Original vs Predicted 15 Day Mean Min Temps",
       x = "Month",
       y = "Average 15 Day Minimum Temp Per Station",
       subtitle = "Black = Original | Red = Predicted",
       caption = "This is the bottomw .2% outlier data") +
  theme(plot.title = element_text(hjust = .5, size = rel(2)),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.2)),
        plot.caption = element_text(size = rel(1.5)))

# Combine all Min data together, replace Min temp values
min_high.2 <- min_high %>% 
  mutate(min_temp_mean = predictions_high_min_preds) %>% 
  select(-predictions_high_min_preds)

min_low.2 <- min_low %>% 
  mutate(min_temp_mean = predictions_low_min_preds) %>% 
  select(-predictions_low_min_preds)

# Only select 3 columns: 2 for join with max_final
min_final <- rbind(min_high.2, min_mid, min_low.2) %>%
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
                                           "Dec"))) %>% 
  select(date, station_name, min_temp_mean) %>% 
  rename("min_temp_avg" = "min_temp_mean") %>% # so we can join without col.y 
  mutate(mon = month(date, label = TRUE)) # mon created for vis below

# Save to use in roctober.github.io/portfolio
write_csv(min_final, path = "/Users/Wolfe/portfolio_site/docs/topics/dv/boxplot.csv")
write_csv(min_high.2, path = "/Users/Wolfe/portfolio_site/docs/topics/dv/min_high.2.csv")
write_csv(min_low.2, path = "/Users/Wolfe/portfolio_site/docs/topics/dv/min_low.2.csv")




# Boxplot to check data shape
min_final %>% 
  ggplot(aes(x = mon, y = min_temp_avg, color = mon)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = .2, alpha = .5) +
  geom_jitter(data = min_high.2, 
              aes(y = min_temp_mean), 
              color  = "black", 
              width = .2) +
  geom_jitter(data = min_low.2, 
              aes(y = min_temp_mean), 
              color  = "brown", 
              width = .2) +
  theme_bw() +
  labs(title = "15 Day Min Temps After Predictions Added",
       x = "Month",
       y = "Average 15 Day Min Temp Per Station",
       caption = "32 Degree Temps Not Added In Yet",
       subtitle = "Black = Former Top .2% Data | Red = Former Bottom .2% Data") +
  theme(plot.title = element_text(hjust = .5, size = rel(2)),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.2)),
        plot.caption = element_text(size = rel(1.5)),
        legend.position = "none")


####### Combine all data into one big dataset ##########

# Join max and min final datasets together, rbind with 32 data
final_temp_data <- max_final %>% 
  left_join(min_final, by = c("date", "station_name")) %>%  
  mutate(min_temp_mean = min_temp_avg) %>% 
  select(-c(min_temp_avg, rank, mon.y)) %>% # mon.y created because vis above
  rename("mon" = "mon.x") %>% # same with mon.x
  rbind(final.32) %>% 
  mutate(avg_temp = round((max_temp_mean + min_temp_mean)/2, 1))


####### Visualization(s) of Data ##########
final_temp_data %>% 
  ggplot(aes(x = mon, y = avg_temp, color = mon)) +
  geom_boxplot(outlier.shape = NA, color = "black") +
  geom_jitter(alpha = .3, width = .2) +
  theme_bw() +
  labs(title = "Average 15 Day Temperature Per Station",
       x = "Month",
       y = "Average 15 Day Per Temp") +
  theme(plot.title = element_text(hjust = .5, size = rel(2)),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.2)),
        legend.position = "none") 

