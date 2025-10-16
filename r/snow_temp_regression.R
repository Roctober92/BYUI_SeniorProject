####################
# Analyzes temperature effect upon snow depth
####################

# get data
# EDIT: There are NA and bad depths still!
snow <- read_csv("data/full_snow.csv")

# make avg_temp column
snow.1 <- snow %>% mutate(avg_temp = round((max_temp_mean + min_temp_mean)/2, 1))

# make lm()
temp.lm <- lm(start_snow_depth ~ avg_temp*elev, data = snow.1)
summary(temp.lm)
t <- coef(temp.lm)
plot(start_snow_depth ~ avg_temp, data = snow.1)
abline(t[1], t[2], col = "green")
