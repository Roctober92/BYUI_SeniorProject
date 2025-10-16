#####################################
# We are looking for a simple regression that passes assumptions
# and have a strong correlation, low SSE
#####################################


sr_data <- read_csv("/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/na_prepped_data.csv")


# Get 70% Size
smp_size <- floor(0.70 * nrow(sr_data))

# Split Randomely
set.seed(92)
train_data <- sample(seq_len(nrow(sr_data)), size = smp_size)

# Make subsets to make train/test data
train <- sr_data[train_data, ]
test <- sr_data[-train_data, ]

# Make train regression
# Y_hat = 2.6X + 7.96
# R = .82
train.lm <- lm(start_snow_depth ~ start_snow_water, data = train)
summary(train.lm)
plot(train.lm, which = c(1:4))
tr <- coef(train.lm)



# Data Look like it could need a slight X transformation, but we'll check Y
boxCox(train.lm, lambda = seq(0, 1, .2))




# Y Transformed Data
# R = .81
train.trans <- lm(I(start_snow_depth)^(.8) ~ start_snow_water, data = train)
summary(train.trans)
trt <- coef(train.trans)
plot(train.trans, which = c(1:4))
# The transformation on Y probably isn't needed



# SQRT(X) TRANSFORMATION
# y = 16.58X^.5 - 14.3
# R = .832
train_x_trans <- train %>% 
  mutate(x_trans = sqrt(start_snow_water),
         x_tf = start_snow_water^(.75))

x_trans.lm <- lm(start_snow_depth ~ x_trans, data = train_x_trans)
summary(x_trans.lm)
plot(x_trans.lm, which = c(1:4))
xt <- coef(x_trans.lm)


# X^.75 TRANFORMATION
# y = 6.41x^.75 - .07
# R = .84
x_transtf.lm <- lm(start_snow_depth ~ x_tf, data = train_x_trans)
summary(x_transtf.lm)
plot(x_transtf.lm, which = c(1:4))
xtf <- coef(x_transtf.lm)


# Prediction in small quantites. The y intercept of the simple
# regression worries me a little bit
predict(train.lm, data.frame(start_snow_water = 1), level = 0.95, interval = "confidence")
predict(x_trans.lm, data.frame(x_trans = 1), level = 0.95, interval = "confidence")
predict(x_transtf.lm, data.frame(x_tf = 1), level = 0.95, interval = "confidence")


# Boxplot to check
# round to tenths, make factors to create multiple successive boxes
box_data <- data %>% 
  filter(start_snow_water < 1.5) %>% 
  mutate(start_snow_water = as.factor(round(start_snow_water, 1)))

ggplot(box_data, aes(x = start_snow_water, y = start_snow_depth)) +
  geom_boxplot() +
  theme_bw()

# After looking at the data when rain is around 1, the snow_depth seems to favor
# more the sqrt(x) transformation



# Y Transformation on SQRT(x)
# There seems to be a possible Y transformation based on
# standardized residuals plot
# It may not be needed, the sqrt(x) data looked good otherwise
boxCox(x_trans.lm, lambda = seq(.5, .8, .05))
# A .6 transformation is suggested


yx <- lm(I(start_snow_depth^.5) ~ x_trans, data = train_x_trans)
summary(yx)
#.869 correlation
plot(yx, which = c(1:2))
plot(yx$residuals^2)
yxc <- coef(yx)


# Check them on a plot
plot(start_snow_depth ~ start_snow_water, 
     data = train, 
     col = "gold", 
     main = "Simple Linear Regression",
     ylab = "Snow Depth",
     xlab = "Water Equivalent")
abline(tr[1], tr[2], col = "green")
curve((trt[1] + trt[2]*x)^(1.2), add = TRUE, col = "purple")
curve(xt[1] + xt[2]*x^.5, add = TRUE, col = "red")
curve(xtf[1] + xtf[2]*x^.75, add = TRUE, col = "brown")
curve((yxc[1] + yxc[2]*x^.5)^(2), add = TRUE, col = "black")

# Check Residuals Vs Fitted again
plot(train.lm, which = 1) #.82
plot(train.trans, which = 1) #.81
plot(x_trans.lm, which = 1) #.832
plot(x_transtf.lm, which = 1) #.84
plot(yx, which = 1) #.874

################# TEST DATA SECTION #####################

# Test simple regression
# 2.58X + 8.13
# R = .83
test.lm <- lm(start_snow_depth ~ start_snow_water, data = test)
summary(test.lm)
plot(test.lm, which = c(1:4))
te <- coef(test.lm)


# Test Y Trans Regression
# R = .82
test.trans <- lm(I(start_snow_depth)^(.8) ~ start_snow_water, data = test)
summary(test.trans)
tet <- coef(test.trans)
plot(test.trans, which = c(1:4))


# SQRT(X) TRANSFORMATION
# y = 
# R = .846
test_x_trans <- test %>% 
  mutate(x_trans = sqrt(start_snow_water),
         x_tf = start_snow_water^(.75))

x_test_trans.lm <- lm(start_snow_depth ~ x_trans, data = test_x_trans)
summary(x_test_trans.lm)
plot(x_test_trans.lm, which = c(1:4))
xte <- coef(x_test_trans.lm)


# X^.75 TRANFORMATION
# y = 
# R = .851
x_test_tf.lm <- lm(start_snow_depth ~ x_tf, data = test_x_trans)
summary(x_test_tf.lm)
plot(x_test_tf.lm, which = c(1:4))
xtef <- coef(x_test_tf.lm)


# Y & X Transformation
yx_test <- lm(I(start_snow_depth^.5) ~ x_trans, data = test_x_trans)
summary(yx_test)
#.877 correlation
plot(yx_test, which = c(1:2))
plot(yx_test$residuals^2)
yxt <- coef(yx_test)


# Check Scatter plots
plot(start_snow_depth ~ start_snow_water, data = test, col = "gold")
abline(te[1], te[2], col = "green")
curve((tet[1] + tet[2]*x)^(1.2), add = TRUE, col = "purple")
curve(xte[1] + xte[2]*x^.5, add = TRUE, col = "red")
curve(xtef[1] + xtef[2]*x^.75, add = TRUE, col = "brown")
curve((yxt[1] + yxt[2]*x^.5)^(2), add = TRUE, col = "black")

# Check residuals/fitted again
plot(test.lm, which = 1)
plot(test.trans, which = 1)
plot(x_test_trans.lm, which = 1)
plot(x_test_tf.lm, which = 1)
plot(yx_test, which = 1)


# FULL DATA
full_x_trans <- sr_data %>% 
  mutate(x_trans = sqrt(start_snow_water))

yx_full <- lm(I(start_snow_depth^.5) ~ x_trans, data = full_x_trans)
summary(yx_full)
#.871 correlation
plot(yx_full, which = 3)
f <- coef(yx_full)
plot(start_snow_depth ~ start_snow_water, data = sr_data, col = "gold")
curve((f[1] + f[2]*x^.5)^(2), add = TRUE, col = "black")

################## THOUGHTS #######################
# I had to alter the data, and re-run the tests, which altered 
# all of the outcomes. I did this because somehow I was allowing
# erroneos ratios and <1 data to be included, that I should have 
# disallowed in the prep script
# In any case, the transformations seem to improve the correlation,
# and the X only changes seem to also fix linearity problems best
# My biggest question: Should we include months as factors
# in multiple regression? I'm thinking we try.
# I'll create another script for probing a little
