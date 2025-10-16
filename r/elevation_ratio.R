####################################################
# This is to check if there is a difference in ratio
# in respect to elevation
####################################################

in_data <- read_csv("data/full_temp.csv")

# Make sure there is no NA, all > 0, code elevation levels,
# only include logical ratios, order elev 
elev_data <- in_data %>% 
  select(date, start_snow_water, start_snow_depth, elev) %>% 
  filter(!is.na(start_snow_depth),
         start_snow_depth > 0,
         start_snow_water > 0) %>% 
  mutate(ratio = round(start_snow_depth/start_snow_water, 2),
         elev_level = case_when(
           elev > 8000 & elev < 9000 ~ "8K-9K",
           elev > 9000 & elev < 10000 ~ "9K-10K",
           elev > 10000 & elev < 11000 ~ "10K-11K",
           elev > 11000 & elev < 12000 ~ "11K-12K"
         ),
         elev_level = fct_relevel(elev_level, 
                                  levels = c("8K-9K", 
                                             "9K-10K", 
                                             "10K-11K",
                                             "11K-12K"))) %>% 
  filter(ratio < 12) 


### Make boxplot 
# 10:1, 5:1, and other ratios showing up
ggplot(elev_data, aes(x = elev_level, y = ratio)) +
  geom_jitter(aes(color = elev_level), alpha = .3, width = .3) +
  geom_boxplot(outlier.shape = NA, alpha = .3, width = .4) +
  theme_classic() +
  labs(y = "Snow Ratio", 
       x = "Elevation Levels",
       title = "Elevation Doesn't Change Ratio") +
  theme(axis.text = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(hjust = .5, size = rel(2), face = "bold"),
        legend.position = "none")

