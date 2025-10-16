####################
# ggridge graphics
####################

# get data
snow <- read_csv("data/full_snow.csv")

# order month
snow.1 <- snow %>% 
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

# yearly summary temp
snow.1 %>% 
  ggplot(aes(x = avg_temp, y = mon, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(option = "B", name = "Mean Temp") +
  labs(x = "15 Day Mean Temp",
       y = "Month",
       title = "Yearly Temperature Distribution",
       subtitle = "Summer temps less varied") +
  theme(plot.title = element_text(size = rel(2), face = "bold", hjust = .5),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.5)),
        plot.subtitle = element_text(size = rel(1.5), face = "bold"))

# yearly summary snow
snow.1 %>% 
  ggplot(aes(x = start_snow_depth, y = mon, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(option = "B", name = "Mean Temp") +
  labs(x = "Snow Depth",
       y = "Month",
       title = "Yearly Snow Depth Distribution",
       subtitle = "Why aren't >150 values showing up?") +
  theme(plot.title = element_text(size = rel(2), face = "bold", hjust = .5),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.5)),
        plot.subtitle = element_text(size = rel(1.5), face = "bold"))

# 
