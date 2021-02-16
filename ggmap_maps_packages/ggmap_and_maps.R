# Introduction to Spatial Data

# For a long time, R has had relatively simple mechanism to create maps because it is a statistical software. 
# But right now, many packages have been built by geospatial information scientists, such as sp, and rgdal. R has been acquiring much of the functionality of traditional GIS packages, but you still need be considerable familiar with GIS concept.
# The third approach is using ggmap package, which allows you tiling of base maps download from Google Earth, Open Street Maps, Stamen and Naver Map servers etc. 

# Coordinate Reference System: Geographical coordinates, latitude-longitude pairs
# Positive Numbers for one direction: North for latitude, and East for longitude
# Negative umber for the other direction: South for latitude, and West for longitude
# Usually specifies longitude in x-coordinate, and latitude in y-coordinate

# Load Packages
library(ggmap)
library(tidyverse)
library(ggthemes)
library(keyring)
library(ggrepel)
library(RColorBrewer)

# House sale dataset for Corvallis
sales <- readRDS("../data/01_corv_sales.rds")
str(sales)

# You can simply display house sale locations by using ggplot2 package
# Plotting points on a map
ggplot(data = sales, aes(x = lon, y = lat))+
  geom_point()
# Doesn't look like a map, because it is missing spatial cues, like terrain and roads.

# ggmap package could help us to add those spatial cues to our plots. It download maps from web services and adds them as a layer in ggplot2 plots.

# You need first register a Google Maps API key via Google Cloud Platform
# There are limits on Geocoding for using Google services, but you have $200 dollars in free usage every month.

# Start use Google API
register_google(key = keyring::key_get("MY_GOOGLE_API"))

# Example: Get a map of New York City
# First you could Google New York City coordinates
nyc <- c(lon = -74.0060, lat = 40.7128)

# or Geocoding with Google API
geocode("New York City", source = "google")

# Download the relevant map
nyc_map <- get_map(location = nyc, 
                   zoom = 10) 
# map zoom, an integer from 3 (continent) to 21 (building), default value 10 (city), openstreetmaps limits a zoom of 18, and the limit on stamen maps depends on the maptype. You can also use 'auto'. Maps of the whole world currently not supported.

# Display the map
ggmap(nyc_map)

# Use a pair of coordinates called corvallis
corvallis <- c(lon = -123.2620, lat = 44.5646)

# Get map at zoom level 5: map_5
# zoom = 5 (Corvallis is in the State of Oregon on the West Coast of the USA)

map_5 <- get_map(location = corvallis, zoom = 5, scale = 1)
# Here I am using an additional argument, scale. This controls the resolution of the downloaded maps and I'll set it lower (the default value is 2, returns 1280*1280 pixels) to reduce how long it takes for the downloads. 4 is reserved for Google Business Users only.

ggmap(map_5)

# Get map at zoom level 13: corvallis_map
# zoom = 13 (The Willamette River runs through town, and Corvallis is the home of Oregon State University)
corvallis_map <- get_map(location = corvallis, zoom = 13, scale = 1)

ggmap(corvallis_map)

head(sales)

# Putting all together: Put locations of House Sales on Map
# ggmap sets the map as the default dataset and also sets the default aesthetic mappings. If you want to add an additional layer from something other than the map, for example, you want to use a new dataset. Then you need to specify both the mapping and data arguments to the geom.
ggmap(corvallis_map)+
  geom_point(aes(x = lon, y = lat), data = sales)

# Looks like there are no house sales East of the Willamette River or on the Oregon State University Campus

# Adding additional aesthetics
# Mapping the size of points to the number of bedrooms of each house.
ggmap(corvallis_map)+
  geom_point(aes(x = lon, y = lat, size = bedrooms), data = sales)

# Map color to price per squarefoot
ggmap(corvallis_map)+
  geom_point(aes(x = lon, y = lat, color = price / finished_squarefeet))
# Some of the points are gray even though gray isn't in our color scale. The gray points are locations with missing values for the variable we have mapped to color, that is, scales with NA for either price or finished_squarefeet.

# The default Google Map downloaded by get_map() is useful when you need major roads, basic terrain, and places of interests, but visually it can be a little bit busy. Sometimes, you aren't really interested in the roads and places, but more what's on the ground, like grass, trees, or snow. Then, switching to a satellite view migh be more useful.

# Add a maptype argument to get a satellite map
corvallis_map_sat <- get_map(location = corvallis, maptype = "satellite", zoom = 13)

# Edit to display satellite map
ggmap(corvallis_map_sat)+
  geom_point(data = sales, aes(x = lon, y = lat, color = year_built))

# You can grab Stamen Maps by using source argument in get_map(), along with specifying a maptype argument.
# You don't need API for Stamen Maps Server
# Stamen has a map type called "toner", which is a high-contrast black and white map, could be very useful sometime

# Add source and maptype to get toner map from Stamen Maps server
corvallis_map_bw <- get_map(location = corvallis, source = "stamen", maptype = "toner", zoom = 13)

# Edit to display Toner Map
ggmap(corvallis_map_bw)+
  geom_point(data = sales, aes(x = lon, y = lat, color = year_built))

# Want other map images sources? 
?get_map

# You can add layers to ggmap() plot by adding geom layers and specifying the data and aesthetic mapping. But this approach has some downsides: First, further layers also ned to specify the data and mapping. Second, facetting function will not work at all. The best way is to set up a base layer, and set a default dataset and mapping as we did for ggplot().

# The initial plot we made
ggmap(corvallis_map)+
  geom_point(data = sales, aes(x = lon, y  = lat))

# Define the base_layer inside of ggmap
ggmap(corvallis_map, base_layer = ggplot(data = sales, aes(x = lon, y = lat)))+
  geom_point()
# Now you can add facets, or extra layers as you did for ggplot2

# For example, add a facet_wrap() to facet by class of house: Dwelling, Mobile Home, Commercial etc.
ggmap(corvallis_map_bw, base_layer = ggplot(data = sales, aes(x = lon, y = lat)))+
  geom_point(aes(color = class))+
  facet_wrap(~class)

# Quick alternative, help you to reduce typing more codes, very similar to qplot() in ggplot2
# qmplot() function
# qmplot replaces both steps, downloading the map and displaying the map, and its syntax is a blend between qplot(), get_map() and ggmap()

qmplot(x = lon, y = lat, data = sales, geom = "point", color = class)+
  facet_wrap(~class) # default source is stamen
# Just want to let you know, CRAN stable released version of ggmap could not returning correct map types
# See bug report here: https://github.com/dkahle/ggmap/issues/281

# This is intend to give you quick plots and you don't need to specify a map, it will grah one on its own. But this function is less flexible than a full specification. Also, gmplot will sets the default dataset and mapping without the need for base_layer argument

# Types of spatial data
# Point
# Line: When the data is associated with a collection of points in a line, we call it line data. For example, cycling, running, swimming, or streams. To describe the line, we need multiple points, and we assume that they are connected with straight lines.
# Polygon: Polygon data occurs when the data is associated with an area. To describe the area we take a collection of points and join them with lines to enclose a polygon.
# Raster (aka gridded): A regular grid is specified by an origin and steps in the x and y axes. Data is associated with every cell in the grid. This is commonly occurs in remote sensing, where a satellite is used to image the earh. For example, for each cell in our grid, we might have the type of vegetation it contains, its elevation and slope.

# Dataframe Structures for Polygons
# Let's look at another dataset: House prices by ward
# Wards are areas that have roughly equal numbers of people, and can be described by polygons

ward_sales <- readRDS("../data/01_corv_wards.rds")
head(ward_sales)

# We have columns with group and order

# Polygons are described by a collection of points, in this data frame each point is a row. Since it takes many rows to describe the shape of a single ward (area), you could see the summary data is repeated many times. 

# Also, order does matter when you join the data points because different orders will result in different polygons.

# Additionally, a single area might need multiple polygons to describe it. For example, one area might have two pieces or more divided by a river. So you may have additional layers within one ward.

# To begin with, we first add a point layer with color mapped to ward
ggplot(data = ward_sales, aes(x = lon, y = lat))+
  geom_point(aes(color = ward)) # We have 9 wards in Corvallis

# What if you add a point layer with color mapped to group
ggplot(data = ward_sales, aes(x = lon, y = lat))+
  geom_point(aes(color = group)) # Some wards are described by more than one polygon

# Now, lets connected those points by group use geom_path()
ggplot(data = ward_sales, aes(x = lon, y = lat))+
  geom_path(aes(group = group), size = 0.5, color = "black") # We map the group aesthetic argument with variable group

# Add a polygon layer with fill mapped to ward, and group to group with geom_polygon()
ggplot(data = ward_sales, aes(x = lon, y = lat))+
  geom_polygon(aes(fill = ward, group = group)) # fill aesthetic mapped to ward, and the group aethestic mapped to group

# What if you want both path and polygon layer?
ggplot(data = ward_sales, aes(x = lon, y = lat))+
  geom_polygon(aes(fill = ward, group = group))+
  geom_path(aes(group = group), size = 1, color = "black") # you must call geom_path() after geom_polygon(), otherwise, you the fill aesthetic will overlap your path

# Choropleth with ggmap
# You may think this way
ggmap(corvallis_map_bw, 
      base_layer = ggplot(data = ward_sales, aes(x = lon, y = lat)))+
  geom_polygon(aes(fill = ward, group = group))


# What happens to ward 1, 3, 8. Part of the ward boundaries are beyond the map boundary. By default settings in ggmap(), any data off the map is dropped before plotting. That's why you find some polygon boundaries are dropped and the remaining points are joined up and connected, resulted in a wrong shapes.
ggmap(corvallis_map, 
      base_layer = ggplot(data = ward_sales, aes(x = lon, y = lat)), 
      extent = "normal", # extent equals normal forces the plot to use the data range
      maprange = FALSE)+ # maprange = FALSE means the map should not define the x and y limits
  # Those 2 arguments together force the plot to use the data range rather than map range
  geom_polygon(aes(fill = ward, group = group))

# Repeat, but map fill to num_sales
ggmap(corvallis_map_bw, 
      base_layer = ggplot(ward_sales, aes(lon, lat)),
      extent = "normal", 
      maprange = FALSE) +
  geom_polygon(aes(group = group, fill = num_sales))

# How about average prices?
ggmap(corvallis_map_bw, 
      base_layer = ggplot(ward_sales, aes(lon, lat)),
      extent = "normal", 
      maprange = FALSE) +
  geom_polygon(aes(group = group, fill = avg_price), alpha = 0.8)+ # add opacity to allow map to show through
  geom_path(aes(group = group), size = 0.5, color = "#FFA500")

# What if you want to download enough map to cover the whole range of data

# Use qmplot()
qmplot(x = lon, y = lat, data = ward_sales, geom = "polygon", 
       group = group, fill = avg_price, alpha = 0.8)+
  geom_path(aes(group = group), color = "#FFA500", size = 0.5)+
  theme_map()+
  guides(alpha = FALSE)+
  theme(legend.background = element_blank())
# But again, the CRAN version could not return correct map type for you.

# Alternative Method
# Use bbox argument to decide your range of map. First, we go to openstreetmap website: https://www.openstreetmap.org/
# Them enter your location, set to right zoom, and adjust your map, click export, you will get left, right, top and bottom numbers. You use the numbrs in the corresponding boxes. The map should also have a zoom argument.

corvallis_map_bw2 <- get_stamenmap(
  bbox = c(left = -123.3538, right = -123.1969, top = 44.6066, bottom = 44.5189), 
  maptype = "toner", 
  zoom = 13)

ggmap(corvallis_map_bw2,
      base_layer = ggplot(ward_sales, aes(lon, lat)),
      extent = "normal",
      maprange = FALSE) +
  geom_polygon(aes(group = group, fill = avg_price), alpha = 0.8)+
  geom_path(aes(group = group), size = 0.5, color = "#FFA500")+
  theme_map()+
  guides(alpha = FALSE)+
  theme(legend.background = element_blank(),
        legend.position = "top")

# Raster data as Heatmap
preds <- readRDS("../data/01_corv_predicted_grid.rds")

# The predicted house prices in preds are called raster data. You have a variable measured (or in this case predicted) at every location in a regular grid. The lat values stepping up in intervals of about 0.002, as lon is constant. After 40 rows, lon increases by about 0.003, as lat runs through the same values. 
# When data forms a regular grid, one approach to displaying it is as a heatmap. 

# Create a simple dot plot of the locations
ggplot(data = preds, aes(x = lon, y = lat))+
  geom_point()

# Add a tile layer will fill mapped to predicted_price
ggplot(data = preds, aes(x = lon, y = lat))+
  geom_tile(aes(fill = predicted_price))

# Now we use ggmap() function
ggmap(corvallis_map_bw)+
  geom_tile(data = preds, aes(x = lon, y = lat, fill = predicted_price), alpha = 0.8)

# This conclude a very brief introduction for ggmap package.

# What if you want to draw world map? You need other useful packages, like `maps`
library(maps)

# Look at what kinds of data we have
help(package = "maps")
# the packages include USA, New Zealand, France, Italy, and low resolution world map etc.

# The main function you will use to get geospatial data
?map_data
# map_data() could easily turn data from the maps package in to a data frame suitable for plottling with ggplot2.

# Suppose I want to plot a world map
world_map <- map_data("world")

head(world_map)

# Draw a simple world map
ggplot(data = world_map, aes(x = long, y = lat))+
  geom_polygon(aes(group = group), fill = "white", color = "black")+
  theme_map()+
  labs(x = "Longitude", y = "Latitude")

# What if the package do not provide the data for the country you want
# Suppose you want to draw a map of Germany 
germany_map <- world_map %>%
  filter(region == "Germany")

head(germany_map)

ggplot(data = germany_map, aes(x = long, y = lat, group = group))+
  geom_polygon(fill = "white", color = "black")

# You can also choose more countries
france_germany <- world_map %>%
  filter(region == "Germany" | region == "France")

ggplot(data = france_germany, aes(x = long, y = lat, group = group))+
  geom_polygon(fill = "white", color = "black")

# Add points 
# Suppose we want to point out all Japan cities that have population larger than 1 million
world_cities <- maps::world.cities
head(world_cities)

japan_cities <- world_cities %>%
  filter(country.etc == "Japan" & pop >= 1000000)

japan_map <- world_map %>%
  filter(region == "Japan") %>%
  as_tibble()

ggplot(japan_map, aes(x = long, y = lat, group = group))+
  geom_polygon(fill = "white", color = "black")+
  geom_point(data = japan_cities, aes(x = long, y = lat, group = NULL), color = "red")+
  geom_text_repel(data = japan_cities, aes(x = long, y = lat, group = NULL, label = name), 
                  min.segment.length = 0, 
                  nudge_y = 0.5, 
                  fontface = "bold")+
  theme_map()

# Choropleth Map
# You can also draw US states 
# Make a map of US states using state data from map_data command
us_states <- map_data("state")
# This provides a list of continental states plus District of Columbia, no AK and HI

us_states <- us_states %>%
  dplyr::select(-subregion) %>%
  dplyr::rename(state = region)

us_states$state <- str_to_title(us_states$state) # Change the first letter of state to Uppercase

head(us_states)

ggplot(data = us_states, aes(x = long, y = lat, group = state))+
  geom_polygon(fill = "white", color = "black")+
  coord_map(projection = "mercator")+
  theme_map()

# Make some fake data for presentation

states <- unique(us_states$state)
length(states)

fake_data <- data.frame(
  "state" = states, 
  "qual_value" = c(rep(LETTERS[1:10], 4), 
                   LETTERS[1:9]), 
  "quan_value" = runif(49, min = 0, max = 10)
)

head(fake_data)

# Merge Data
combinned_data <- left_join(us_states, fake_data, by = "state")

head(combinned_data)
  
# Make a choropleth map with qualitative data (categorical)
ggplot(data = combinned_data, aes(x = long, y = lat, group = state))+
  geom_polygon(aes(fill = qual_value), color = "black")+
  scale_fill_brewer(type = "qual", palette = "Paired")+
  theme_map()+
  theme(legend.position = "top")

# Make a choropleth map with quantitative data
ggplot(data = combinned_data, aes(x = long, y = lat, group = state))+
  geom_polygon(aes(fill = quan_value), color = "black")+
  scale_fill_distiller(type = "seq", palette = "Greens",
                       breaks = seq(0, 10, by = 2), 
                       limits = c(0, 10), 
                       direction = "horizontal")+
  theme_map()+
  theme(legend.position = "top")







