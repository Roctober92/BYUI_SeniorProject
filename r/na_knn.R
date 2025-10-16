########################
# Selects columns, dispatches and receives data from KNN regression in Python
# Used before regression prep
# Uses elevation, year, month, county, and middle 99.6% temp columns as preds
########################

### Get data from to_knn directory
data_32 <- read_csv("/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_knn/data32.csv")
high_max <- read_csv("/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_knn/black_dots_max.csv")
high_min <- read_csv("/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_knn/black_dots_min.csv")
low_max <- read_csv("/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_knn/red_dots_max.csv")
low_min <- read_csv("/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_knn/red_dots_min.csv")
max_mid <- read_csv("/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_knn/regular_dots_max.csv")
min_mid <- read_csv("/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_knn/regular_dots_min.csv")

# put in list
df_list <- c(data_32,
             high_max,
             high_min,
             low_max,
             low_min)
 
### Select wanted columns
sel_col <- function(x) {
  x %>% 
    select(elev, mon)
}

data.32.selected <- data_32 %>% 
  sel_col()

high_max_selected <- high_max %>% 
  sel_col() 

high_min_selected <- high_min %>% 
  sel_col()

max_mid_selected <- max_mid %>% 
  select(max_temp_mean, elev, mon) 

low_max_selected <- low_max %>% 
  sel_col() 

low_min_selected <- low_min %>% 
  sel_col() 

min_mid_selected <- min_mid %>% 
  select(min_temp_mean, elev, mon)

### Write to to_python directory
write_csv(data.32.selected, path = "/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_python/data32.csv")
write_csv(high_max_selected, path = "/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_python/high_max_selected.csv")
write_csv(high_min_selected, path = "/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_python/high_min_selected.csv")
write_csv(max_mid_selected, path = "/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_python/max_mid_selected.csv")
write_csv(low_max_selected, path = "/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_python/low_max_selected.csv")
write_csv(low_min_selected, path = "/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_python/low_min_selected.csv")
write_csv(min_mid_selected, path = "/Users/Wolfe/Desktop/spring_2018/Senior_Project/data/to_python/min_mid_selected.csv")

