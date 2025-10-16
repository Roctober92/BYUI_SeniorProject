#######################################
# We will apply the regression predictions, both month and temp,
# to the NA data and combine it with the non - NA data
#######################################

# Get data
with_na <- read_csv("data/to_pred_with_na.csv")
wo_na <- read_csv("data/to_pred_wo_na.csv")
mr_data <- read_csv("data/na_prepped_data.csv")

#### Recreate Regression
all_sqrtx <- mr_data %>% 
  mutate(x_trans = sqrt(start_snow_water))
xy_root_all.lm <- lm(I(start_snow_depth^.5) ~ x_trans*mon + avg_temp, data = all_sqrtx)

####### REPLACE NA VALUES
# August is probably most similary to July, same with Sep --> Oct
# This is done because there wasn't any Sep or Aug in the regression
with_na.1 <- with_na %>% 
  mutate(mon = fct_recode(mon,
                          "Jul" = "Aug",
                          "Oct" = "Sep"),
         x_trans = sqrt(start_snow_water))

# list of items
x_trans <- with_na.1$x_trans
mon <- with_na.1$mon
avg_temp <- with_na.1$avg_temp

# create function
make_preds <- function(x_trans.x, mon.x, avg_temp.x) {
  round((predict(xy_root_all.lm, 
                 data.frame(x_trans = x_trans.x,
                            mon = mon.x,
                            avg_temp = avg_temp.x),
                 level = .95,
                 interval = "prediction")^2)[1], 0)
}

# function to lists
preds.x <- mapply(make_preds, x_trans, mon, avg_temp) 

# add in predictions
predicted_data <- with_na.1 %>% 
  cbind(preds.x) %>% 
  mutate(start_snow_depth = preds.x,
         data_type = "imputed") %>% 
  select(-c(preds.x, x_trans))

# combine
total_month <- predicted_data %>% rbind(wo_na)
write_csv(total_month, path = "data/full_snow.csv")


