################################
# Script for multiple regression using temperature
# Model for b0 + b1X1 + b2X2 + b3X1X2 seems best, validates closely
# Month regression still correlates better, decision upcoming
################################

# Read in data
temp_data <- read_csv("data/na_prepped_data.csv")

# give key to be able to anti-join below
temp_data.2 <- temp_data %>% mutate(key = 1:nrow(.))

# determine % given to train
train_size <- round(nrow(temp_data.2)*.7, 0)

# make sets
train <- temp_data.2 %>% 
  sample_n(train_size)

test <- temp_data.2 %>% 
  anti_join(train, by = "key")

#### Train Regression
temp.lm <- lm(start_snow_depth ~ start_snow_water*avg_temp, data = train)
summary(temp.lm) 
tlm <- coef(temp.lm)
plot(temp.lm, which = 3)

# BoxCox 
boxCox(temp.lm, lambda = seq(.8,.9,.02)) #.8 transformation suggested

#### Y Transformation
y_train.lm <- lm(I(start_snow_depth^.8) ~ start_snow_water*avg_temp, data = train)
summary(y_train.lm) #.872
yt <- coef(y_train.lm)
plot(y_train.lm, which = 3)
# Data doesn't look anywhere better, maybe an X transformation to
# linearize it?


#### X transformation
x_tran <- train %>% 
  mutate(avg_temp_x = avg_temp^.5)

x_sqrt_train.lm <- lm(start_snow_depth ~ start_snow_water*avg_temp_x, data = x_tran)
summary(x_sqrt_train.lm) #.875
plot(x_sqrt_train.lm, which = 4) # not much change from simple regression

#### Simple Regression
simple.lm <- lm(start_snow_depth ~ start_snow_water, data = train)
summary(simple.lm) #.818
plot(simple.lm, which = 3) # seems to provide less than multiple


#### General linear F test
anova(simple.lm, temp.lm) # fuller model suggested


#### Test Regression
temp_test.lm <- lm(start_snow_depth ~ start_snow_water*avg_temp, data = test)
summary(temp_test.lm) #.868
plot(temp_test.lm, which = 3)

# Normalized SSE comparison
SSE_train <- sum((train$start_snow_depth - temp.lm$fitted.values)^2)
SSE_test <- sum((test$start_snow_depth - temp_test.lm$fitted.values)^2)
(SSE_train/nrow(train)) #62.18
(SSE_test/nrow(test)) #57.89



#### FULL DATA REGRESSION
full.lm <- lm(start_snow_depth ~ start_snow_water*avg_temp, data = temp_data)
summary(full.lm) #.88
plot(full.lm, which = 2)

res <- temp_data$start_snow_depth-full.lm$fitted.values
fit <- tibble(
  fit.y = full.lm$fitted.values,
  y = temp_data$start_snow_depth,
  res = res
)

ggplot(data = fit, aes(x = fit.y, y = res)) + geom_point(alpha = .5) +ylim(-50, 50) 

### Plotly 3d Graphic
plot_ly(temp_data, 
        x = ~start_snow_water, 
        y = ~avg_temp,
        z = ~start_snow_depth,
        color = ~avg_temp) %>% 
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'SWE'),
                      yaxis = list(title = 'Average Temp (F)'),
                      zaxis = list(title = 'Snow Depth')))
