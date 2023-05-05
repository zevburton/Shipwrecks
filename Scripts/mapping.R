# mapping shipwrecks
library(readr)
library(tidyverse)
wrecks <- read.csv("wrecks.csv") %>% select(Longitude, Latitude)

library(leaflet)
library(htmlwidgets)

# Initialize the Leaflet map
m <- leaflet() %>%
  addTiles() # Add a default tile layer

# Add the "wrecks" data as markers with the custom icon
m <- m %>% addCircleMarkers(data = wrecks, 
                            ~Longitude,
                            ~Latitude, 
                            color = "red", 
                            fillOpacity = 0.3,
                            radius = 2)

# Set the view to the "wrecks" data
m <- m %>% fitBounds(
  lng1 = min(wrecks$Longitude),
  lat1 = min(wrecks$Latitude),
  lng2 = max(wrecks$Longitude),
  lat2 = max(wrecks$Latitude)
)

# Show the map
m


### currents_map

sink <- read.csv("../sink.csv")

currents <- sink %>% select(Longitude, Latitude, u_current, v_current)
currents$Current <- sqrt((currents$u_current)^2 + (currents$v_current)^2)
currents <- currents %>% select(Longitude, Latitude, Current)

library(ggplot2)
library(maps)

# Create a map of the Eastern US coast
us_map <- map_data("state", region = c("maryland", "virginia", "north carolina", "south carolina", "georgia", "florida", ))

# Create a data frame with latitude, longitude, and current data
currents_df <- data.frame(lat, lon, curr)

# Plot the map and data points using ggplot2
ggplot() +
  geom_point(data = currents_df, aes(x = lon, y = lat, color = curr), size = 2) +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), color = "gray", fill = "white") +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Longitude", y = "Latitude", color = "Current (knots)", title = "Shipwrecks and Ocean Currents on the Eastern US Coast") +
  xlim(-80, -65) + 
  theme_minimal()

library(ggplot2)

# Define the color palette for currents
palette <- colorRampPalette(c("blue", "red"))

# Create a base map of the US
us_map <- map_data("state")

east_coast_states <- c("Maine", "New Hampshire", "Massachusetts", "Rhode Island", "Connecticut", "New York", "New Jersey", "Pennsylvania", "Delaware", "Maryland", "Virginia", "North Carolina", "South Carolina", "Georgia", "Florida")

us_map$region <- tolower(us_map$region)
east_coast_states <- tolower(east_coast_states)

east_coast_map <- subset(us_map, region %in% east_coast_states)

# Subset the map to include only land east of -80 longitude
land_east_of_80 <- subset(us_map, long > -80)

library(rnaturalearth)
library(sf)

# Get shapefile data for Canada
canada <- ne_states(country = "canada", returnclass = "sf")


# Add a layer for the data points
ggplot(currents, aes(x = lon, y = lat, color = curr)) +
  geom_point() +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group), color = "black", fill = "white") + 
  geom_sf(data = canada) +
  geom_polygon(data = land_east_of_80, aes(x = long, y = lat, group = group), color = "black", fill = "gray") +
  scale_color_gradientn(colors = palette(10), na.value = "gray") +
  labs(x = "Longitude", y = "Latitude", color = "Current (knots)") +
  theme_minimal() +
  xlim(-80,-65) +
  theme(legend.position = "none")

library(rnaturalearth)

# Load world map data
world_map <- ne_countries(scale = "medium", returnclass = "sf")

