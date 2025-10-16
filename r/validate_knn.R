#########
# Calculate SSE
#########

# get data
knn_date <- read_csv("data/knn_to_r/to_validate_high.csv")

# calculate SSE
(SSE <- sum((knn_date$predictions-knn_date$real_values)^2))
