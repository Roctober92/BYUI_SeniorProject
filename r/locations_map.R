###########################
# Mapping the locations
###########################
library(leaflet)

# get data
full_data <- read_csv("data/full_snow.csv")


#### Make Station directory
# get distinct values
unique_data <- full_data %>% 
  select(station_name, latitude, longitude, elev) %>% 
  distinct() %>% 
  as.tibble()

# make snowflake icon
snow_icon <- makeIcon(iconUrl = "docs/pics/snowflake.jpg", 
                      iconWidth = 30, 
                      iconHeight = 40)


# make directory leaflet
leaflet() %>% 
  setView(lng = -106, lat = 39.5, zoom = 8) %>% 
  addTiles() %>% 
  addMarkers(data = unique_data, label = unique_data$station_name, icon = snow_icon,
             labelOptions = labelOptions(noHide = T, direction = "bottom",
                                         style = list(
                                           "color" = "darkblue",
                                           "font-family" = "Titillium Web",
                                           "font-style" = "italic",
                                           "box-shadow" = "1px 1px rgba(0,0,0,0.25)",
                                           "font-size" = "10px",
                                           "border-color" = "rgba(0,0,0,0.5)"
                                         )))

#### Stations & Elevation graphic
# get color
getColor <- function(x) {
  sapply(x$elev, function(elev) {
    if(elev < 9500){
      "green"
    } else if(elev < 10000 & elev > 9499){
      "orange"
    } else if(elev < 11000 & elev > 9999){
      "red"
    } else {
      "blue"
    }
  })
}

# Make icon
icons <- awesomeIcons(
  markerColor = getColor(unique_data)
)

# make map
leaflet(unique_data) %>% 
  addTiles() %>% 
  addAwesomeMarkers(~longitude,
                    ~latitude, 
                    icon = icons,
                    label = paste(unique_data$station_name, unique_data$elev),
                    labelOptions = labelOptions(noHide = F, direction = "bottom",
                                                style = list(
                                                  "color" = "black",
                                                  "font-family" = "Titillium Web",
                                                  "font-style" = "italic",
                                                  "box-shadow" = "1px 1px rgba(0,0,0,0.25)",
                                                  "font-size" = "20px",
                                                  "border-color" = "rgba(0,0,0,0.5)"
                                                ))) %>% 
  addLegend(colors = c("green", "orange", "red", "blue"), 
            labels = c("Below 9500", "9500-10000", "10000-11000", "11000+"),
            title = "Elevation Levels")



#### Dispersing Icons
leaflet(unique_data) %>% 
  addTiles() %>% 
  addMarkers(clusterOptions = markerClusterOptions())
  