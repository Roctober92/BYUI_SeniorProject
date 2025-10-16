#######################################
# Visualizing the Snow Depths
#######################################

# get data
depths <- read_csv("data/full_snow.csv")

# set theme
theme_set(theme_bw())

# Function to order months
month_set <- function(x) {
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

# histogram of frequencies
depths %>% 
  ggplot(aes(x = start_snow_depth)) +
  geom_histogram(bins = 200, color = "black", fill = "blue")

# Number of < 1
depths %>% filter(start_snow_depth < 1) %>% count()

# Summer monthly observations
depths %>% filter(mon == "Aug") %>% nrow()
depths %>% filter(mon == "Jul") %>% nrow()
depths %>% filter(mon == "Sep") %>% nrow()

depths %>% 
  month_set() %>% 
  filter(start_snow_depth < 1) %>% 
  group_by(mon) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(x = mon, y = count)) + 
  geom_point(size = 15) +
  geom_text(aes(y = count, label = count), color = "white") +
  theme_bw() +
  labs(x = "Month",
       y = "Number of Observations",
       title = "Observation Amount When Depth = 0",
       color = "",
       subtitle = "") +
  theme(plot.title = element_text(hjust = .5, size = rel(2)),
        axis.title = element_text(size = rel(1.5)),
        plot.subtitle = element_text(face = "bold"),
        axis.text = element_text(size = rel(1.5)),
        legend.position = "none")

# Histogram of frequencies >= 1
depths %>% filter(start_snow_depth > 60) %>% nrow()

depths %>%
  filter(start_snow_depth > 1) %>% 
  ggplot(aes(x = start_snow_depth)) +
  geom_histogram(bins = 200, color = "black", fill = "darkgreen") +
  scale_x_continuous(breaks = seq(0, 200, 20)) +
  scale_y_continuous(breaks = seq(0, 850, 100)) +
  annotate("text", x = 30, y = 845, label = "69643 Observations") +
  annotate("text", x = 80, y = 845, label = "5118 Observations") +
  geom_vline(xintercept = 60, color = "red", linetype = "dashed") +
  labs(x = "Snow Depth",
       y = "Number of Observations",
       title = "Snow Depth Observation Amount",
       color = "",
       subtitle = "Excluding ratios < 1") +
  theme(plot.title = element_text(hjust = .5, size = rel(2)),
        axis.title = element_text(size = rel(1.5)),
        plot.subtitle = element_text(face = "bold",size = rel(1.5)),
        axis.text = element_text(size = rel(1.5)),
        legend.position = "none")


#### Ratios per month, no 0
depths %>% 
  month_set() %>%
  #filter(start_snow_depth >= 1) %>% 
  ggplot(aes(x = mon, y = start_snow_depth, color = mon)) +
  geom_jitter(width = .2, alpha = .3) +
  geom_boxplot(width = .4, alpha = .5, outlier.shape = NA, color = "black") +
  labs(x = "Months",
       y = "Snow Depth",
       title = "Snow Depth Per Month",
       color = "",
       subtitle = "",
       caption = "High May outlier values?") +
  theme(plot.title = element_text(hjust = .5, size = rel(2)),
        axis.title = element_text(size = rel(1.5)),
        plot.subtitle = element_text(face = "bold",size = rel(1.5)),
        axis.text = element_text(size = rel(1.5)),
        plot.caption = element_text(face = "bold"),
        legend.position = "none") 

