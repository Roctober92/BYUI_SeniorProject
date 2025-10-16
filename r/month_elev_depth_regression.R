####################
# Trying to find if depth varies with combinations of elevation, month, and temperature
####################

# get data
snow <- read_csv("data/full_snow.csv")

# create elevation levels, reorder levels and months
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
                                        "11K-12K")),
    mon = fct_relevel(mon, levels = c("Jan",
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

# LM with all month, temp, elev combinations
lm.1 <- lm(start_snow_depth ~ mon + elev_level + avg_temp + mon:elev_level:avg_temp, data = snow.1)
summary(lm.1)
plot(lm.1, which = 3)

# lm for April
april <- snow.1 %>% filter(mon == "Apr")
lm.april <- lm(start_snow_depth ~ avg_temp*elev_level, data = april)
summary(lm.april)

# plotly 3d
plot_ly(april, 
        x = ~elev, 
        y = ~avg_temp,
        z = ~start_snow_depth,
        color = ~avg_temp) %>% 
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'Elevation'),
                      yaxis = list(title = 'Average Temp (F)'),
                      zaxis = list(title = 'Snow Depth')))

# Refit some elevations so we only have like 7 lines to compare
april.1 <- april %>% 
  mutate(elev_fit = case_when(
    elev < 9250 ~ 9000,
    elev < 9750 & elev > 9249 ~ 9500,
    elev < 10250 & elev > 9749 ~ 10000,
    elev < 10750 & elev > 10249 ~ 10500,
    elev < 11250 & elev > 10749 ~ 11000,
    elev < 11750 & elev > 10749 ~ 11500,
    TRUE ~ 12000
  )) 

# Re-do graphic
plot_ly(april.1, 
        x = ~elev_fit, 
        y = ~avg_temp,
        z = ~start_snow_depth,
        color = ~elev_fit) %>% 
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'Elevation levels'),
                      yaxis = list(title = 'Average Temp (F)'),
                      zaxis = list(title = 'Snow Depth')))

# facetted ggplot
april.1 %>% 
  ggplot(aes(x = avg_temp, y = start_snow_depth, color = as.factor(elev_fit))) +
  geom_point() +
  geom_smooth(color = "black", method = "lm", se = FALSE) +
  facet_wrap(~elev_fit) +
  theme_bw() +
  labs(x = "Average 15 Day Temp",
       y = "Snow Depth",
       color = "Elevation",
       title = "April Depth ~ Elev*Temp") +
  theme(plot.title = element_text(size = rel(2), face = "bold", hjust = .5),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)))

# Whole dataset: facet grid
snow.2 <- snow.1 %>% 
  mutate(elev_fit = case_when(
    elev < 9250 ~ 9000,
    elev < 9750 & elev > 9249 ~ 9500,
    elev < 10250 & elev > 9749 ~ 10000,
    elev < 10750 & elev > 10249 ~ 10500,
    elev < 11250 & elev > 10749 ~ 11000,
    elev < 11750 & elev > 10749 ~ 11500,
    TRUE ~ 12000
  ))

# facet grid
snow.2 %>% 
  ggplot(aes(x = avg_temp, y = start_snow_depth, color = as.factor(elev_fit))) +
  geom_point() +
  geom_smooth(color = "black", method = "lm", se = FALSE) +
  facet_grid(elev_fit ~ mon, scales = "free_y") +
  theme_bw() +
  labs(x = "Average 15 Day Temp",
       y = "Snow Depth",
       color = "Elevation",
       title = "Depth by Temp, Elevation, and Mon") +
  theme(plot.title = element_text(size = rel(2), face = "bold", hjust = .5),
        axis.title = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = rel(1.5)),
        panel.border = element_rect(colour = "black"))

# Perform manual lm to find values
# lm.2 <- lm(start_snow_depth ~ elev_fit*mon, data = snow.2)
# summary(lm.2)
