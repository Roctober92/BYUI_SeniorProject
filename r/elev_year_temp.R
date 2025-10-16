####################
# Elevation Yearly Trends
####################

# get data
snow <- read_csv("data/full_snow.csv")

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

# yearly max temp
high_plot <- snow.1 %>% 
  filter(year != 2018) %>% 
  group_by(elev_level, year) %>% 
  summarise(high_temp = mean(max_temp_mean),
            low_temp = mean(min_temp_mean)) %>% 
  ggplot(aes(x = year, y = high_temp, color = elev_level)) +
  geom_line(size = 2) +
  theme_bw() +
  labs(x = "Year",
       y = "High Temp",
       color = "Elevation Level",
       title = "Yearly Max Temp per Elevation Level") +
  theme(plot.title = element_text(size = rel(2), face = "bold", hjust = .5),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5))) +
  scale_x_continuous(breaks = seq(1975, 2020, 2)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .3))

# yearly low temp
low_plot <- snow.1 %>% 
  filter(year != 2018) %>% 
  group_by(elev_level, year) %>% 
  summarise(high_temp = mean(max_temp_mean),
            low_temp = mean(min_temp_mean)) %>% 
  ggplot(aes(x = year, y = low_temp, color = elev_level)) +
  geom_line(size = 2) +
  geom_smooth(method = "lm", color = "black", se = FALSE, size = 2) +
  theme_bw() +
  labs(x = "Year",
       y = "High Temp",
       color = "Elevation Level",
       title = "Yearly Low Temp per Elevation Level") +
  theme(plot.title = element_text(size = rel(2), face = "bold", hjust = .5),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5))) +
  scale_x_continuous(breaks = seq(1975, 2020, 2)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .3))

# yearly high-low temp scale
scale_plot <- snow.1 %>% 
  filter(year != 2018) %>% 
  group_by(elev_level, year) %>% 
  summarize(mean_t = mean(max_temp_mean),
            mean_l = mean(min_temp_mean)) %>% 
  mutate(scale_temp = round(mean_t/mean_l, 3)) %>%
  ggplot(aes(x = year, y = scale_temp, color = elev_level)) +
  geom_line(size = 2) +
  theme_bw() +
  labs(x = "Year",
       y = "High/Low",
       color = "Elevation Level",
       title = "Yearly Mean High/Low Proportion") +
  theme(plot.title = element_text(size = rel(2), face = "bold", hjust = .5),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5))) +
  scale_x_continuous(breaks = seq(1975, 2020, 2)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .3))

# arranged
ggarrange(high_plot, low_plot, scale_plot, nrow = 3)

