####################################################
# This is to check if there is a difference in ratio
# between the months
####################################################


# Get Data
month_data <- read_csv("/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/na_prep_predata.csv")

# Make sure there is no NA, all > 0, extract month, calculate ratio,
# all ratios make sense
ratio_data <- month_data %>% 
  select(date, start_snow_water, start_snow_depth, elev, station_name) %>% 
  filter(!is.na(start_snow_depth),
         start_snow_depth > 0,
         start_snow_water > 0) %>% 
  mutate(ratio = round(start_snow_depth/start_snow_water, 2),
         mon = fct_relevel(month(date, label = TRUE), 
                           levels = c("Oct", 
                                      "Nov", 
                                      "Dec", 
                                      "Jan", 
                                      "Feb", 
                                      "Mar", 
                                      "Apr", 
                                      "May", 
                                      "Jun", 
                                      "Jul"))) %>% 
  filter(ratio < 12) 

# Make boxplot to show good representation of data
# Used in email to NRCS to ask about 10:1 questions
ggplot(data = ratio_data, aes(x = mon, y = ratio)) +
  geom_jitter(aes(color = mon), width = .2, alpha = .4) +
  geom_boxplot(outlier.shape = NA, width = .3, alpha = .5) +
  theme_classic() +
  labs(title = "Snow/SWE (in)",
       x = "Month", 
       y = "Snow Ratio") +
  theme(plot.title = element_text(hjust = .5, size = rel(2.5), face = "bold"),
        axis.title = element_text(size = rel(1.5), face = "bold"),
        axis.text = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.5)),
        legend.position = "none") + 
  scale_color_manual(values = c("#D6DE33",
                                "#BADE33",
                                "#77CB18",
                                "#58E1C8",
                                "#26CBDE",
                                "#267FDE",
                                "#9373E7",
                                "#D473E7",
                                "#E773A4",
                                "#C1545B"))

# Regression with mon factors
mon.lm <- lm(start_snow_depth ~ start_snow_water + mon , data = ratio_data)
summary(mon.lm)

# Make summarized ratio per month
ratio_month <- ratio_data %>% 
  select(-ratio) %>% 
  group_by(mon) %>% 
  summarise(prec_sum = sum(start_snow_water),
            snow_sum = sum(start_snow_depth),
            count = n()) %>% 
  mutate(ratio = round(snow_sum/prec_sum, 2))

# Make visualization to show summary of data
ggplot(ratio_month, aes(x = mon, y = ratio, group = ratio)) +
  geom_point(size = 3) +
  geom_line() +
  theme_bw()
