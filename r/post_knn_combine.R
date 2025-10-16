####################
# Combine final dataset 
# Predictions + good data
####################


### Get data
# Good data
max_mid <- read_csv("data/to_knn/regular_dots_max.csv") 
min_mid <- read_csv("data/to_knn/regular_dots_min.csv") 

# Re-predicted Data
data_32 <- read_csv("data/rknn_to_r/data.32.csv")
high_max <- read_csv("data/rknn_to_r/high.max.csv")
high_min <- read_csv("data/rknn_to_r/high.min.csv")
low_max <- read_csv("data/rknn_to_r/low.max.csv")
low_min <- read_csv("data/rknn_to_r/low.min.csv")
both_na <- read_csv("data/rknn_to_r/both.na.csv")
max_na <- read_csv("data/rknn_to_r/max.na.csv")
min_na <- read_csv("data/rknn_to_r/min.na.csv")
