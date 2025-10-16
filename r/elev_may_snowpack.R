####################
# May Snow Depth
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

# May Snow Depth
may_plot <- snow.1 %>% 
  filter(mon == "May",
         start_snow_depth > 0) %>% 
  group_by(year, elev_level) %>% 
  summarise(may_snow = mean(start_snow_depth)) %>% 
  ggplot(aes(x = year, y = may_snow, color = elev_level)) +
  geom_line(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(x = "Year",
       y = "Snow Depth",
       color = "Elevation Level",
       title = "Yearly May Snow Depth per Elevation Level") +
  theme(plot.title = element_text(size = rel(2), face = "bold", hjust = .5),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5))) +
  scale_x_continuous(breaks = seq(1975, 2020, 2)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .3)) +
  scale_color_manual(values = c("#E0E015",
                                "#12A21B",
                                "#9E1D10",
                                "#091661"))

# June Snow Depth
jun_plot <- snow.1 %>% 
  filter(mon == "Jun",
         start_snow_depth > 0,
         elev_level != "8K-9K") %>% 
  group_by(year, elev_level) %>% 
  summarise(june_snow = mean(start_snow_depth)) %>% 
  ggplot(aes(x = year, y = june_snow, color = elev_level)) +
  geom_line(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(x = "Year",
       y = "Snow Depth",
       color = "Elevation Level",
       title = "Yearly June Snow Depth per Elevation Level") +
  theme(plot.title = element_text(size = rel(2), face = "bold", hjust = .5),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5))) +
  scale_x_continuous(breaks = seq(1975, 2020, 2)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .3)) +
  scale_color_manual(values = c("#12A21B",
                                "#9E1D10",
                                "#091661"))


# Nov Snow Depth
nov_plot <- snow.1 %>% 
  filter(mon == "Nov",
         start_snow_depth > 0) %>% 
  group_by(year, elev_level) %>% 
  summarise(nov_snow = mean(start_snow_depth)) %>% 
  ggplot(aes(x = year, y = nov_snow, color = elev_level)) +
  geom_line(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(x = "Year",
       y = "Snow Depth",
       color = "Elevation Level",
       title = "Yearly November Snow Depth per Elevation Level") +
  theme(plot.title = element_text(size = rel(2), face = "bold", hjust = .5),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5))) +
  scale_x_continuous(breaks = seq(1975, 2020, 2)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .3)) +
  scale_color_manual(values = c("#E0E015",
                                "#12A21B",
                                "#9E1D10",
                                "#091661"))
