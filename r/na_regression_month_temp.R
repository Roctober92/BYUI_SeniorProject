##########################
# Here we will explore multiple regression with our snowpack data
# seeing that month may be a variable we want to add
# we are also adding avg temp between high and low per day
##########################


### DATA PREP #####################
# Get data
mr_data <- read_csv("data/na_prepped_data.csv")

# give key to be able to anti-join below
mr_data.2 <- mr_data %>% mutate(key = 1:nrow(.))

# determine % given to train
train_size <- round(nrow(mr_data.2)*.7, 0)

# make sets
train <- mr_data.2 %>% 
  sample_n(train_size)

test <- mr_data.2 %>% 
  anti_join(train, by = "key")

####################################

# LM with Month factor, avg_temp
multiple.lm <- lm(start_snow_depth ~ start_snow_water*mon + avg_temp, data = train)
summary(multiple.lm) #r = .937
plot(multiple.lm, which = 3)



###### SQRT(X) Transformation
# We might do this because the data was most linear with this model
train_sqrtx <- train %>% 
  mutate(x_trans = sqrt(start_snow_water),
         x_tf = start_snow_water^(.75))

x_root_train.lm <- lm(start_snow_depth ~ x_trans*mon + avg_temp, data = train_sqrtx)
summary(x_root_train.lm) #r = .9264
plot(x_root_train.lm, which = 1)
# Concern: Residuals show non-linear data, negative fitted values??
# Let's check the BoxCox

boxCox(x_root_train.lm, lambda = seq(.4, .6, .05))
# It suggests a .5 transformation on Y


### Y,X TRANSFORMATION
xy_root_train.lm <- lm(I(start_snow_depth^.5) ~ x_trans*mon + avg_temp, data = train_sqrtx)
summary(xy_root_train.lm) #r = .944
plot(xy_root_train.lm, which = 3)
# Improvement in residuals
# y = b0 + b1X1^.5 + b2X2 + b3X1^.4X2, y = ((b0 + b2) + (b1 + b3)X^.5)^2)



### VALIDATION SET
test_sqrtx <- test %>% 
  mutate(x_trans = sqrt(start_snow_water),
         x_tf = start_snow_water^(.75))

xy_root_test.lm <- lm(I(start_snow_depth^.5) ~ x_trans*mon + avg_temp, data = test_sqrtx)
summary(xy_root_test.lm) #r = .9452
plot(xy_root_test.lm, which = 1)
# The r-squared looks about the same, with more or less the same
# significant terms



### ENTIRE DATA REGRESSION
# We will use these for the actual regression
all_sqrtx <- mr_data.2 %>% 
  mutate(x_trans = sqrt(start_snow_water))

# try to see if we can put temperature 
# email them about snow depth possibilities
# use predict.lm() to map values
xy_root_all.lm <- lm(I(start_snow_depth^.5) ~ x_trans*mon +avg_temp, data = all_sqrtx)
summary(xy_root_all.lm) #r = .9445
plot(xy_root_all.lm, which = 1)
fitted.y <- round(xy_root_all.lm$fitted.values^2, 0)
res <- all_sqrtx$start_snow_depth-fitted.y
fit <- tibble(
  fit.y = fitted.y,
  y = all_sqrtx$start_snow_depth,
  res = as.integer(res),
  month = all_sqrtx$mon,
  elevation = all_sqrtx$elev
)

# get 1 sd value
sd_res_high_one <- mean(fit$res) + sd(fit$res)
sd_res_low_one <- mean(fit$res) - sd(fit$res)
sd_res_high_two <- mean(fit$res) + sd(fit$res)*2
sd_res_low_two <- mean(fit$res) - sd(fit$res)*2

# fitted residual graph
ggplot(data = fit, aes(x = fit.y, y = res)) + 
  geom_point(alpha = .5) + 
  theme_bw() +
  labs(x = "Predicted Y Values", 
       y = "Residuals", 
       title = "Fitted Vs Residuals",
       subtitle = "Colored lines are deviations") +
  theme(axis.text = element_text(size = rel(1.5)),
        plot.title = element_text(hjust = .5, size = rel(2), face = "bold"),
        axis.title = element_text(size = rel(1.5)),
        plot.subtitle = element_text(size = rel(1.5))) +
  geom_hline(aes(yintercept = sd_res_high_one), color = "blue") +
  geom_hline(aes(yintercept = sd_res_low_one), color = "blue") +
  geom_hline(aes(yintercept = sd_res_low_two), color = "red") +
  geom_hline(aes(yintercept = sd_res_high_two), color = "red") +
  geom_hline(aes(yintercept = 0), color = "orange") +
  scale_y_continuous(breaks = seq(-80, 90, 20)) +
  scale_x_continuous(breaks = seq(0, 200, 20))



# histogram
fit %>% 
  ggplot(aes(x = res)) +
  geom_histogram(bins = 100, color = "black", fill = "blue") +
  theme_bw() +
  xlim(-40, 40) +
  labs(x = "Residuals", 
       y = "Frequency", 
       title = "Residuals Histogram",
       subtitle = "Colored lines are deviations") +
  theme(axis.text = element_text(size = rel(1.5)),
        plot.title = element_text(hjust = .5, size = rel(2), face = "bold"),
        axis.title = element_text(size = rel(1.5)),
        plot.subtitle = element_text(size = rel(1.5))) +
  geom_vline(aes(xintercept = sd_res_high_one), color = "black") +
  geom_vline(aes(xintercept = sd_res_low_one), color = "black") +
  geom_vline(aes(xintercept = sd_res_low_two), color = "red") +
  geom_vline(aes(xintercept = sd_res_high_two), color = "red") +
  geom_vline(aes(xintercept = 0), color = "orange")
