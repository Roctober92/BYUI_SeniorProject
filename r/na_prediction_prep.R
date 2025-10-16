########################
# Split data into where snow depth does and doesn't have NA values
# This will be used for setting up the model for predictions
########################

# READ IN DATA
pre_data <- read_csv("data/full_temp.csv")

# Year column, when SWE = 0, so does snow depth
year_data <- pre_data %>% 
  mutate(year = year(date),
         start_snow_depth = as.double(start_snow_depth),
         start_snow_depth = case_when(
           start_snow_water == 0 ~ 0,
           TRUE ~ start_snow_depth
         ))


### NA
na <- year_data %>% filter(is.na(start_snow_depth))

### Not Na
not_na <- year_data %>% filter(!is.na(start_snow_depth))

### change 10:1 and 5:1 to 0, id created for anti-join below
not_na.2 <- not_na %>% 
  mutate(id = 1:nrow(.),
         ratio = round(start_snow_depth/start_snow_water, 1),
         start_snow_depth = case_when(
           ratio == 10 | ratio == 5 ~ NA_real_,
           ratio > 5 & start_snow_depth > 50 ~ NA_real_,
           TRUE ~ start_snow_depth
         )) %>%
  select(-ratio)


###  New NAs
ten_five_na <- not_na.2 %>% filter(is.na(start_snow_depth))

### And the no_na, which is the compliment
not_na.3 <- not_na.2 %>% anti_join(ten_five_na, by = "id")

# take out id column
no_id <- function(x) {
  x %>% 
    select(-id)
}

# apply function
ten_five_na.2 <- ten_five_na %>% no_id()
wo_na <- not_na.3 %>% no_id() 

# make avg_temp column
avg <- function(x) {
  x %>% 
    mutate(avg_temp = round((max_temp_mean + min_temp_mean)/2, 1))
}

# 5,10 to na
with_na <- na %>% rbind(ten_five_na.2) %>% avg()
wo_na.1 <- wo_na %>% avg() %>% mutate(data_type = "original")

# Save data to use in script to apply predictions, combine
write_csv(with_na, path = "data/to_pred_with_na.csv")
write_csv(wo_na.1, path = "data/to_pred_wo_na.csv")
