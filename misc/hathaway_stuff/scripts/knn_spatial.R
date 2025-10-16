library(pacman)

p_load(tidyverse, purrr)

dat <- read_csv("https://query.data.world/s/v6tq5zqshpnqsfompjdiwirqqb2jv4") %>%
  mutate(day = case_when(
    str_detect(month, '1st') ~ 1,
    str_detect(month, '2nd') ~ 15
  )) %>% 
  separate(month, into = c("mon", "part"), sep = 3) %>% 
  mutate(mon = case_when(
    mon == "Jan" ~ 01,
    mon == "Feb" ~ 02,
    mon == "Mar" ~ 03,
    mon == "Apr" ~ 04,
    mon == "May" ~ 05,
    mon == "Jun" ~ 06,
    mon == "Jul" ~ 07,
    mon == "Aug" ~ 08,
    mon == "Sep" ~ 09,
    mon == "Oct" ~ 10,
    mon == "Nov" ~ 11,
    mon == "Dec" ~ 12
  )) %>% 
  unite(date, year, mon, day, sep = '-') %>% 
  mutate(date = as.Date(date))

dat_missing <- dat %>%
  filter(is.na(max_temp_mean) | (max_temp_mean == 32 & min_temp_mean == 32))  %>%
  select(date, station_name, max_temp_mean, latitude, longitude, elev) %>%
  mutate(key = str_c(date, station_name, sep = "_"))

dat_avail <- dat %>%
  filter(!is.na(max_temp_mean), !(max_temp_mean == 32 & min_temp_mean == 32))  %>%
  select(date, station_name, max_temp_mean, latitude, longitude, elev)




#### Example functions
my_fun <- function(x) paste(x, collapse = " | ") # simple example


# return rows where train and test have same date
find_same_dates <- function(x, good_dat = dat_avail){
  good_dat %>%
    filter(date == x$date)
}

# Train and test within 800 feet of each other
find_same_elev <- function(x, good_dat = dat_avail, elev_match = 1000){
  good_dat %>%
    filter(abs(elev - x$elev) < elev_match) 
}


# Find stations that are within a certain distance of each other
# 50 miles are .5 differences
# 62 miles are .8
# 69 miles are 1
# 83 miles are 1.2
find_same_xy <- function(x, good_dat = dat_avail) {
  good_dat %>% 
    filter(sqrt((latitude - x$latitude)^2 + (longitude - x$longitude)^2) < 1)
}

  
  
# apply all above functions at once
big_func <- function(x, good_dat = dat_avail, elev_match = 1000){
  
  good_dat %>%
    filter(sqrt((latitude - x$latitude)^2 + (longitude - x$longitude)^2) < 1,
           abs(elev - x$elev) < elev_match)
  
}







# here is the idea.
object <- dat_missing %>%
  split(dat_missing$key) %>% # for every row
  .[1:5] %>% # Take top 5 rows
  map(big_func) # apply the function
object[[1]]

x = dat_missing %>% split(dat_missing$key) %>% .[[1]]
  
x$date






