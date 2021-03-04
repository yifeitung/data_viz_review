# Introduction to Leaflet in R
# Open-source JavaScript library
# Popular option for creating interactive mobile-friendly maps
# Can be created using only R code via the htmlwidgets package

library(leaflet)
library(tidyverse)
library(leaflet.providers)

leaflet() %>%
  addTiles()

# Calling the leaflet() function without any arguments will initialize the htmlwidget. You can supply arguments to customize your map, but they are not required.

# Provider Tiles
# Selecting a Base Map
# Why are you making this map?
# Exploratory analysis
# Deliverable product
# Just for fun!

# What types of data are you plotting?
# Points
# Paths
# Polygons
# Will the geographic and topographic features of the base map add to the information you are presenting or confuse your users?

# There are over 100 provider tiles included in the leaflet package. Most of these tiles you can use by calling the addProviderTiles() function. However, there are a few, like mapbox, that you will need to register for prior to using them. You can access the names of the provider tiles included in the leaflet package by calling the names() function on the providers list.

names(providers)[1:5]

# Exploring Leaflet Provider Tiles
names(providers)[str_detect(names(providers), "OpenStreetMap")]
# print all of the tiles provided by OpenStreetMap

leaflet() %>%
  # addTiles()
  addProviderTiles(provider = "CartoDB.Positron")
# OpenStreetMap Black and White is removed for future use.
# Use "CartoDB.Positron" if you want alternative

# Use str_detect() to determine if the name of each provider tile contains the string "CartoDB"
str_detect(names(providers), "CartoDB")

# Use str_detect() to print only the provider tile names that include the string "CartoDB"
names(providers)[str_detect(names(providers), "CartoDB")]

# Setting the Default Map View
# Rather than having to manually zoom the map to find the area that we are most interested in, we can load it centered on a particular point with a specific zoom level. 

# Geocoding in R
# geocode(location, output = c("latlon", "latlona", "more", "all"), source = c("google", "dsk"))
# latlon: latitude and longitude
# latlona: all of the above plus address
# more: all of the above plus place's type and geographical boundaries
# all: all of the above plus some additional information

library(keyring)
library(ggmap)

register_google(key = keyring::key_get("MY_GOOGLE_API"))
geocode("350 5th Ave, New York, NY 10118", source = "google")
geocode("350 5th Ave, New York, NY 10118", output = "latlona", source = "google")
geocode("350 5th Ave, New York, NY 10118", output = "more", source = "google")
geocode("350 5th Ave, New York, NY 10118", output = "all", source = "google")

# datasciencetoolkit.org terminated its map service, sorry!

# Setting the Default Map View
# setView(): allows you to pick a single point at the center of your map
# fitBounds(): allows you to set the view based on a rectangles. To use fitBounds(), we specify two diagonal corners of a rectangle. 

leaflet() %>%
  addTiles() %>%
  setView(lng = -73.98575, 
          lat = 40.74856, 
          zoom = 13)

leaflet() %>%
  addTiles() %>%
  fitBounds(
    lng1 = -73.910, lat1 = 40.773, 
    lng2 = -74.060, lat2 = 40.723
  )

# Staying Focused
# Sometimes we will want our map to remain focused on a particular geographic area. One way to accomplish this is to turn off the ability to pan the map and to limit the allowed zooom levels. Switching dragging to FALSE will prevent panning and setting the min and max zoom arguments will limit the zoom, effectively setting and maintaining the focal point, while preserving the interactive features of our web map.

# For example, viusalizing Korea Town in New York City
leaflet(options = leafletOptions(dragging = FALSE,
                                 minZoom = 14, 
                                 maxZoom = 18)) %>%
  addProviderTiles("CartoDB") %>%
  setView(lng = -73.98575, 
          lat = 40.74856, 
          zoom = 18)

# An alternative approach to keeping your map's focus on a particular area is to use the setMaxBounds() function instead of switching the dragging option to FALSE. If users try to pan your map outside of the max bounds they will be automatically back into the boundary

leaflet() %>%
  addTiles() %>%
  setView(lng = -73.98575, 
          lat = 40.74856, 
          zoom = 18) %>%
  setMaxBounds(lng1 = -73.98575, lat1 = 40.74856, 
               lng2 = -73.98575, lat2 = 40.74856)

# Map with CartoDB tile centered on DataCamp's NYC office with zoom of 6
leaflet()  %>% 
  addProviderTiles("CartoDB")  %>% 
  setView(lng = -73.98575, lat = 40.74856, zoom = 6)

dc_hq <- tibble(
  hq = c("DataCamp - NYC", 
         "DataCamp - Belgium"), 
  lon = c(-73.98575, 4.717863), 
  lat = c(40.74856, 50.881363)
)

# Map with CartoDB.PositronNoLabels tile centered on DataCamp's Belgium office with zoom of 4
leaflet() %>% 
  addProviderTiles("CartoDB.PositronNoLabels") %>% 
  setView(lng = dc_hq$lon[2], lat = dc_hq$lat[2], zoom = 4)

leaflet(options = leafletOptions(
  # Set minZoom and dragging 
  minZoom = 12, dragging = FALSE))  %>% 
  addProviderTiles("CartoDB")  %>% 
  # Set default zoom level 
  setView(lng = dc_hq$lon[2], lat = dc_hq$lat[2], zoom = 14)

# Use setMaxBounds 
leaflet(options = leafletOptions(
  # Set minZoom and dragging 
  minZoom = 12, dragging = TRUE))  %>% 
  addProviderTiles("CartoDB")  %>% 
  # Set default zoom level 
  setView(lng = dc_hq$lon[2], lat = dc_hq$lat[2], zoom = 14) %>% 
  # Set max bounds of map: Use maximum bounds of .05 decimal degrees from the headquarters 
  setMaxBounds(lng1 = dc_hq$lon[2] + .05, 
               lat1 = dc_hq$lat[2] + .05, 
               lng2 = dc_hq$lon[2] - .05, 
               lat2 = dc_hq$lat[2] - .05) 

# Plotting a Point
# One of the most common layers to add are location markers, which you can add by piping the result of addTiles() into the addMarkers() function.
# There are several options for supply the data for your markers:
# Using numeric columns from a data frame and using numeric vectors with a length of 1

leaflet() %>%
  addTiles() %>%
  addMarkers(lng = -73.98575, 
             lat = 40.74856)

# You may have noticed that our map is zoommed in, but we didn't use the setView or fitBounds. When you add markers to your map without setting the view, leaflet will automatically set the boundaries of the base map based on the markers that you're plotting. 

# If you're plotting a single marker, leaflet will center the map on that marker. 
# If you're plotting multiple markers, leaflet will set the bounds so that they're all visible.

leaflet() %>%
  addTiles() %>%
  addMarkers(lng = dc_hq$lon, 
             lat = dc_hq$lat)

# Alternative approach: pipe the data. addMarkers() will search the columns names for names that are most likely your coordinates and the leaflet package will send you a message to let you know if a match was found

# When piping a data frame into the leaflet function, R will search for columns named lat/latitude and lon/lng/longitude

dc_hq %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers()

# Pop-ups
# It is often helpful to provide users with information about the points we have mapped. 
# A common way to do this is by adding pop-ups that will appear whenever a user clicks on a marker. We can add pop-ups by specifying the popup argument in the addMarkers() function.

leaflet() %>%
  addTiles() %>%
  addMarkers(lng = dc_hq$lon, 
             lat = dc_hq$lat, 
             popup = dc_hq$hq)

# Pop-ups II
# It may be more useful to add pop-ups to the map without markers. We can do this by replacing addMarkers() with addPopups() function. If we had more than a few markers, this approach may be problematic

leaflet() %>%
  addTiles() %>%
  addPopups(lng = dc_hq$lon, 
            lat = dc_hq$lat, 
            popup = dc_hq$hq)

# Storing leaflet Maps as Objects
# Store base map
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = dc_hq$lon[1], 
          lat = dc_hq$lat[1], 
          zoom = 12)

class(m)

# %>% leaflet objects to functions to add or edit layers
m %>% 
  addMarkers(lng = dc_hq$lon, 
             lat = dc_hq$lat, 
             popup = dc_hq$hq)

# Store leaflet hq map in an object called map
map <- leaflet() %>%
  addProviderTiles("CartoDB") %>%
  # Use dc_hq to add the hq column as popups
  addMarkers(lng = dc_hq$lon, lat = dc_hq$lat,
             popup = dc_hq$hq)

# Center the view of map on the Belgium HQ with a zoom of 5 
map_zoom <- map %>%
  setView(lat = 50.881363, lng = 4.717863,
          zoom = 5)

# Print map_zoom
map_zoom

# Plotting Points
# Build on the Leaflet map that we created in the first chapter to make an interactive web map of every four-year college in California.

# Cleaning up the Base Map

# Restore view based on data displayed on map
map_zoom %>%
  clearBounds()

# Remove markers
map_zoom %>%
  clearBounds() %>%
  clearMarkers()

# Since LeafLet maps are built in layers, we cannot simply add more markers to replace the existing markers. This would add another layer on top of our existing markers ranther than replacing them.

# IPEDS
# Integrated Postsecondary Education Data System (IPEDS)
# Administered by National Center for Education Statistics (NCES)
# Data from all institutions participating in federal student financial aid programs: colleges, universities, technical and vocational institutions

# The dataset provided by DataCamp loses important features
# ipeds <- read_csv("datasets/ipeds.csv")
# glimpse(ipeds)

# Download another dataset from Kaggle: https://www.kaggle.com/sumithbhongale/american-university-data-ipeds-dataset
library(readxl)
mydata <- readxl::read_excel("datasets/IPEDS_data.xlsx", sheet = 1)
# This datasets has details about the universities and colleges in USA for year 2013. It has details of degrees' offered, enrollment, graduation and fees and so on. I am only using some important features for analysis, such as geographical locations, name, state, and either private or public.

mydata <- mydata %>%
  # First rename some variables
  rename(name = Name,
         lng = `Longitude location of institution`, 
         lat = `Latitude location of institution`, 
         sector_label = `Control of institution`, 
         state = `State abbreviation`)

ipeds <- mydata %>%
  dplyr::select(name, lng, lat, state, sector_label) %>%
  filter(!is.na(lng) & !is.na(lat))

glimpse(ipeds)

# Check sectors 
unique(ipeds$sector_label)
# [1] "Public"                 "Private not-for-profit"

# Objectives
# Create a subset of the IPEDS data to focus on the colleges in California
# Plot every college in California on a LeafLet map while learning how to use different types of markers.
# Conclude with adding some colors to gray base map when we color code colleges by sector

# Private colleges outnumber public colleges in America
ipeds %>%
  group_by(sector_label) %>%
  count()

# Count the number of four-year colleges in each state
ipeds %>%
  group_by(state) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  top_n(5)

# Mapping a State's Colleges: California

# How many 4-year colleges are there in Maine?

maine_colleges <- ipeds %>%
  filter(state == "Maine")

# Assuming 'lng' and 'lat' are longitude and latitude, respectively
maine_colleges_map <- 
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addMarkers(data = maine_colleges)

# Adding more markers will not remove existing markers

maine_colleges_map %>%
  addCircleMarkers(data = maine_colleges)

# The pin markers are still showing up on our map. Each set of markers are their own layer, so adding a new set of will not replace the existing ones.

maine_colleges_map %>%
  clearMarkers() %>%
  addCircleMarkers(data = maine_colleges, 
                   radius = 3) # render a map with blue circle markers with a radius of 3 pixels

maine_colleges_map <- maine_colleges_map %>%
  clearMarkers()

# Customize color, radius, and popups

maine_colleges_map %>%
  addCircleMarkers(data = maine_colleges, 
                   color = "red", 
                   radius = 4,
                   popup = ~name)

# Create a dataframe with data on only colleges in California
ca <- ipeds %>%
  filter(state == "California")

# Use `addMarkers` to plot all of the colleges in California on the Leaflet Map

# Base Map
map <- leaflet() %>%
  addProviderTiles("CartoDB")

map %>%
  addMarkers(lng = ca$lng, 
             lat = ca$lat)

# It appears that there is a cluster of colleges in and around the City of Angels (e.g., Los Angeles)
# Center the map on LA
la_coords <- data.frame(
  lat = 34.05223, 
  lon = -118.2437
)

map %>%
  addMarkers(data = ca, 
             popup = ~name) %>%
  setView(lng = la_coords$lon, 
          lat = la_coords$lat, 
          zoom = 12)

map_zoom <- map %>%
  addMarkers(data = ca) %>%
  setView(lng = la_coords$lon, 
          lat = la_coords$lat, 
          zoom = 8)

map_zoom
# That is a lot of markers. Let's try to make our map clearer by replacing the pins with circles

map2 <- map_zoom %>%
  clearMarkers()

map2 %>%
  addCircleMarkers(lng = ca$lng, 
                   lat = ca$lat)

# Change the radius of each circle to be 2 pixels and the color to red
map2 %>%
  addCircleMarkers(lng = ca$lng, 
                   lat = ca$lat, 
                   color = "red", 
                   radius = 2)
# Now we can more easily see the colleges in the LA area. Let's add some information to the markers so we can tell which college is represented by each circle.

# Labels and Pop-ups

# Piping and the ~ operator
# Pipes data into the leaflet() function, this approach is more readable and it prevents us from repeatly specifying the dataframe name.

ipeds %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lng = ~lng, 
                   lat = ~lat, 
                   color = "#FF0000", 
                   popup = ~name, 
                   radius = 2)

# Building a Better Pop-up

ipeds %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lng = ~lng, 
                   lat = ~lat, 
                   color = "#FF0000", 
                   popup = ~paste0(name, "-", sector_label), 
                   radius = 2)

# Use HTML codes
ipeds %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lng = ~lng, 
                   lat = ~lat, 
                   color = "#FF0000", 
                   popup = ~paste0("<b>", name, "</b>", 
                                   "<br/>", sector_label), 
                   radius = 2)

# Pop-ups are great but they require a little extra work and sometimes we don't want to click the mouse again. When we use labels, we get the same information with a simple hover. A trick to quickly zoom the map is to hold shift to draw a rectangle.

ipeds %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lng = ~lng, 
                   lat = ~lat, 
                   color = "#FF0000", 
                   label = ~name, 
                   radius = 2)

# Practice
# Add Circle Markers with popups for college names in California
map %>%
  addCircleMarkers(data = ca, 
                   popup = ~name, 
                   radius = 2)

# Change circle markers'color 
map_color <- map %>%
  addCircleMarkers(data = ca, 
                   popup = ~name, 
                   radius = 2, 
                   color = "#2cb42c")

map_color
# Not surprisingly, most of the colleges in California are concentrated in and around metropolitan area, like LA

# Clear the bounds and markers on the map object and store in map2

map2 <- map_color %>%
  clearMarkers() %>%
  clearBounds()

# Add Circle Markers with popups that display both institution name and sector
map2 %>%
  addCircleMarkers(data = ca, 
                   radius = 2, 
                   popup = ~paste0(name, "<br/>", sector_label))

# Make the institution name in each popup bold
map2 %>%
  addCircleMarkers(data = ca, 
                   radius = 2, 
                   popup = ~paste0("<b>", name, "</b>", sector_label))

# Make the institution name in each popup Italics
map2 %>%
  addCircleMarkers(data = ca, 
                   radius = 2,
                   popup = ~paste0("<i>", name, "</i>", "<br>", sector_label))

# Add Circle Markers with labels identifying the name for each college
map2 %>%
  addCircleMarkers(data = ca, 
                   label = ~name, 
                   radius = 2)

# Use paste0 to add sector information to the label inside parentheses
map2 %>%
  addCircleMarkers(data = ca, 
                   label = ~paste0(name, " (", sector_label, ")"), 
                   radius = 2)

# Now we will work to add both style and information to our map using color
# Filter out Colleges in Oregon 
OR <- ipeds %>%
  filter(state == "Oregon")

# colorFactor()
# We use the colorFactor() function to build a color palette that connects our colors to the levels in the sector variable
unique(OR$sector_label)

pal <- colorFactor(palette = c("red", "blue"), 
                   levels = c("Public", "Private not-for-profit"))

oregon_colleges <- OR %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(radius = 2, 
                   color = ~pal(sector_label), # We pass this palette to the color argument of addCircleMarkers
                   label = ~name)

# Add Legend using the addLegend() function
oregon_colleges %>%
  addLegend(position = "bottomright", 
            pal = pal, 
            values = c("Public", "Private not-for-profit"))

# If you want to color points by a continuous variable instead of a factor, we can use the colorNumeric() function.
# Example: Let's build a map of every college in America that admitted half or fewer of its applicants in 2013.

# First filter out admission rate smaller than 50%
admit <- mydata %>%
  rename(rate = `Percent admitted - total`) %>%
  dplyr::select(name, lng, lat, state, rate) %>%
  filter(!is.na(rate) & rate > 0 & rate < 50)

pal <- colorNumeric(palette = "Reds", 
                    domain = c(1:50), 
                    reverse = TRUE) # if reverse is TRUE, then darker red means lower admit rate

admit_map <- admit %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(radius = 4, 
                   color = ~pal(rate), 
                   label = ~name) %>%
  addLegend(position = "bottomright", 
            pal = pal, 
            values = c(1:50), 
            title = "Admit Rate")

admit_map

# We are down to 272 colleges from more than 1500. Only 17.7% of colleges in our admit data admit less than half of their applicants. Furthermore, the number of lightly colored institutions on our map appear to outnumer the darker markers.

# Practice
# Make a color palette called pal for the values of `sector_label` using `colorFactor()`  
# Colors should be: "red"and "#9b4a11" for "Public", "Private not-for-profit" colleges, respectively
pal <- colorFactor(palette = c("red", "#9b4a11"), 
                   levels = c("Public", "Private not-for-profit"))

# Add circle markers that color colleges using pal() and the values of sector_label
map2 <- 
  map %>% 
  addCircleMarkers(data = ca, radius = 2, 
                   color = ~pal(sector_label), 
                   label = ~paste0(name, " (", sector_label, ")")) %>%
  # Add a legend that displays the colors used in pal
  addLegend(position = "bottomright", 
            pal = pal, 
            values = c("Public", "Private not-for-profit"))

# Print map2
map2

# Customize the legend
map %>% 
  addCircleMarkers(data = ca, radius = 2, 
                   color = ~pal(sector_label), 
                   label = ~paste0(name, " (", sector_label, ")")) %>%
  # Opacity of .5, title of sector, and postion of topright
  addLegend(position = "topright", 
            pal = pal, 
            values = c("Public", "Private not-for-profit"), 
            title = "Sector", 
            opacity = 0.5)

# The leaflet.extras Package
library(leaflet.extras)

# This package provides a set of useful functions that extend the leaflet package. 
# addSearchOSM(): make map searchable, add a magnifying glass icon to our map, clicking this icon will reveal a search box that we can use to search all of the locations included on our map
# addReverseSearchOSM(): geocode with a mouse click
# addResetMapButton(): reset map view

leaflet() %>%
  addTiles() %>%
  addSearchOSM()

# Previously, we used the geocode() function from the ggmaps package to find a location’s coordinates. 
# With one additional line of code, the addReverseSearchOSM() function makes it possible to use OpenStreetMap to geocode simply by clicking on our leaflet map. 

leaflet() %>%
  addTiles() %>%
  addSearchOSM() %>%
  addReverseSearchOSM()

# When you are searching and zooming in on locations, it's often helpful to have a shortcut to get back to the default map view. 
# Using the addResetMapButton() function will create a reset icon below our magnifying glass that we can use to quickly return to the default view. 

leaflet() %>%
  addTiles() %>%
  addSearchOSM() %>%
  addReverseSearchOSM() %>%
  addResetMapButton()

# Building a Base
# We are working toward mapping all of America's colleges 
# Center our map (Lat = 39.8282, Lng = -98.5795)
base_map <- ipeds %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  # center on the middle of the US with zoom of 3
  setView(lng = -98.5795, lat = 39.8282, zoom = 3)

# Map all American Colleges
pal <- colorFactor(palette = c("red", "blue"), 
                   levels = c("Public", "Private not-for-profit"))

base_map %>%
  addCircleMarkers(radius = 2, 
                   label = ~name, 
                   color = ~pal(sector_label))

# Overlay Groups
# Building up our maps in layers and will use a new concept, groups, to give our users the ability to control which college sectors are displayed on the map.

# Grouping Colleges by Sector: Use California as an example
ca_public <- ipeds %>%
  filter(state == "California" & sector_label == "Public")

ca_private <- ipeds %>%
  filter(state == "California" & sector_label == "Private not-for-profit")

map %>%
  addCircleMarkers(data = ca_public, 
                   radius = 2, 
                   label = ~name, 
                   color = ~pal(sector_label), 
                   group = "Public") %>%
  addCircleMarkers(data = ca_private, 
                   radius = 2,
                   label = ~name, 
                   color = ~pal(sector_label), 
                   group = "Private not-for-profit")

# To capitalize on the work, we need add the addLayersControl() function to the end of our chain of leaflet function calls
# Passing our two overlay groups to this function will add a menue to our map that allows users to toggle which sectors are displayed.

map %>%
  addCircleMarkers(data = ca_public, 
                   radius = 2, 
                   label = ~name, 
                   color = ~pal(sector_label), 
                   group = "Public") %>%
  addCircleMarkers(data = ca_private, 
                   radius = 2,
                   label = ~name, 
                   color = ~pal(sector_label), 
                   group = "Private not-for-profit") %>%
  addLayersControl(
    overlayGroups = c("Public", "Private not-for-profit"), 
    options = layersControlOptions(collapsed = FALSE)) # This will show the toggle

# Practice Mapping all US colleges in our dataset
library(htmltools)

public <- ipeds %>%
  filter(sector_label == "Public")

private <- ipeds %>%
  filter(sector_label == "Private not-for-profit")

map %>%
  # Center the map on the middle of the US with a zoom of 4
  setView(lng = -98.5795, lat = 39.8282, zoom = 4) %>%
  addCircleMarkers(data = public, 
                   radius = 2,
                   label = ~htmlEscape(name), 
                   # sanitize characters that may be interpreted as HTML. 
                   # This prevents any of the college names from appearing with unintended formatting.
                   color = ~pal(sector_label),
                   group = "Public"
                   ) %>%
  addCircleMarkers(data = private, 
                   radius = 2,
                   label = ~htmlEscape(name), 
                   color = ~pal(sector_label), 
                   group = "Private not-for-profit") %>%
  addLayersControl(
    overlayGroups = c("Public", "Private not-for-profit"), 
    options = layersControlOptions(collapsed = FALSE), 
    position = "bottomright"
  )

# Base Groups
# Similar to how we added overlay groups, we can add base map groups to allow our users to toggle between difffernt maps. 
# Unlike overlay groups, only one base group may be selected at a time

# Base Groups and Multiple Map Tiles
# Add multiple base groups, we neeed to call addProviderTiles() one time for each base map we want to make available to our users. 
leaflet() %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("CartoDB", group = "Carto") %>%
  addProviderTiles("Esri", group = "Esri") %>%
  # We need to include a call to addLayersControl() in our chain of functions to give our users the ability
  # to toggle between them. Otherwise, the last map tile that we add will be the only one that our users can see
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri"), 
                   options = layersControlOptions(collapsed = FALSE))

# Overlay and Base Groups
# Let's combine all the things together
leaflet() %>%
  # add basemaps with groups
  addTiles(group = "OSM") %>%
  addProviderTiles("CartoDB", group = "Carto") %>%
  addProviderTiles("Esri", group = "Esri") %>%
  setView(lng = -98.5795, lat = 39.8282, zoom = 4) %>%
  # add marker layer for each sector with corresponding group name
  addCircleMarkers(data = public, 
                   radius = 2,
                   label = ~htmlEscape(name), 
                   color = ~pal(sector_label), 
                   group = "Public") %>%
  addCircleMarkers(data = private, 
                   radius = 2, 
                   label = ~htmlEscape(name), 
                   color = ~pal(sector_label), 
                   group = "Private not-for-profit") %>%
  # add layer controls for base and overlay groups
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri"), 
                   overlayGroups = c("Public", "Private not-for-profit"), 
                   options = layersControlOptions(collapsed = FALSE), 
                   position = "bottomright")
  
# Pieces of Flair
# Customizing the search function from the leaflet extras package to search for specific colleges we have on our map
# Presenting a large number of points on a leaflet map, called clustering

# Public College Search in California
ca_public %>%
  leaflet() %>%
  addProviderTiles("Esri") %>%
  addCircleMarkers(radius = 2,
                   color = ~pal(sector_label), 
                   label = ~htmlEscape(name), 
                   group = "Public") %>%
  # Search for markers
  # Tell the function that we want to search for elements on the map in the group called "public"
  # Options argument tell leaflet that we want to zoom it to 10
  addSearchFeatures(targetGroups = "Public", # This can also be a vector, if you want to make each sector colleges searchable 
                    options = searchFeaturesOptions(zoom = 10))


# Clustering Colleges
# We have tried to address the large number of points on our map by adding the colleges in different layers. 
# If we still have an overwhelming number of points on the map at times. An alternative solution is to cluster the colleges using the cluster options argument to the addCircleMarkers() function. By default, the clusters are orange, yellow, and green.
# The colors correspond to the number of the colleges represented by a cluster.

ipeds %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(radius = 2,
                   color = ~pal(sector_label), 
                   label = ~htmlEscape(name), 
                   clusterOptions = markerClusterOptions())

# When you hover over a cluster, its boundaries will appear. Clicking on a cluster will zoom to that area and the clustered colleges will spread out automatically. 
# Eventually the map will show individual colleges in a region. These points will still be color coded and will have all of the other features that we built previously, like labels. 

# Plotting Polygons
# Polygons have many points, which would require us to have the data repeat. We can leverage a different type of object from the sp package called the spatial polygons data frame.

nc_income <- read_csv("datasets/mean_income_by_zip_nc.csv")

# SpatialPolygonsDataFrame
# This data frame contains information on North Carolina zip codes. You'll notice that there are five slots in the shp object: data, polygons, plotOrder, bbox and projection string

# data: data associated with each polygon
# polygons: coordinates to plot polygons
# plotOrder: order in which polygons are plotted
# bbox: bounding box for geographic data (i.e., a rectangle)
# proj4string: coordinate reference system

load("datasets/nc_zips.Rda")
summary(shp)

# Print the class of shp
class(shp)
# [1] "SpatialPolygonsDataFrame"
# attr(,"package")
# [1] "sp"

# Print the slot names of `shp`
slotNames(shp)
# [1] "data"        "polygons"    "plotOrder"   "bbox"        "proj4string"

# You can access the information in any of the five slots using the @ symbol. 
# The data slot in this example stores zip codes and the area of each polygon
# Since the information in the data slot is stored as a data frame, we can join on additional variables using dplyr
glimpse(shp@data)

class(shp@data)
# [1] "data.frame"

# Print GEOID10
head(shp@data$GEOID10)

# Join on income information from the IRS including the mean income per zip code
class(nc_income$zipcode)
# [1] "numeric"

class(shp@data[, 1])
# [1] "factor"

# The two do not match, we need to change them to same class

nc_income$zipcode <- as.factor(nc_income$zipcode)

shp@data <- shp@data %>%
  left_join(nc_income, by = c("GEOID10" = "zipcode"))

glimpse(shp@data)

# Plotting Polygon 1
# Similar to how we mapped points, we can pipe our polygon data into a call to leaflet(). 
# Then instead of using addMarkers(), we can use addPolygons(). 
shp@polygons[[1]] %>%
  leaflet() %>%
  addPolygons()

# We can also addTiles 
shp@polygons[[1]] %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons()

# Print the number of missing values of each variable in shp@data
# Use lapply: This will returns a list
lapply(shp@data, function(x) sum(is.na(x)))

# Use sapply
sapply(shp@data, function(x) sum(is.na(x)))
# GEOID10     ALAND10     returns      income mean_income 
#       0           0          85          85          85 
# Some of zip codes in our data are missing income information

# Use vapply
vapply(shp@data, function(x) sum(is.na(x)), FUN.VALUE = numeric(1))

# Mapping Polygons
# Creating choropleth maps that color zip codes based on their total and mean incomes

# Options with addPolygons() function
# weight: the thickness of the boundary lines in pixels
# color: the color of the polygons
# label: the information to appear on hover
# highlight: options to highlight a polygon on hover

library(scales)

shp %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(weight = 1, 
              color = "grey", 
              label = ~paste0("Total Income:", dollar(income)), 
              highlight = highlightOptions(weight = 3, 
                                           color = "red", 
                                           bringToFront = TRUE))

# There are three common approaches when mapping color palettes to numeric data
# colorNumeric, colorBin, and colorQuantile, which color the numeric data based on a specific number of groups using the cut function and the quantile function

# colorNumeric()

nc_pal <- colorNumeric(palette = "Blues", 
                       domain = shp@data$mean_income)

# colorBin
nc_pal2 <- colorBin(palette = "YlGn", 
                    bins = 5, 
                    domain = shp@data$mean_income)

# colorQuantile
nc_pal3 <- colorQuantile(palette = "YlGn", 
                         n = 4, 
                         domain = shp@data$mean_income)

# colorBin and colorQuantile color the numeric data based on a specified number of groups using the cut function and the quantile function

# colorNumeric() and preview colors
summary(shp@data$mean_income)
previewColors(pal = nc_pal, 
              values = c(seq(100000, 550000, by = 100000)))

shp %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(weight = 1, 
              fillOpacity = 1, 
              color = ~nc_pal(mean_income), 
              label = ~paste0("Mean Income: ", dollar(mean_income)), 
              highlight = highlightOptions(weight = 3, 
                                           color = "red", 
                                           bringToFront = TRUE)
              )

# We have a right skewed data, meaning there are a small number of zip codes with very large mean incomes
ggplot(shp@data)+
  geom_histogram(aes(x = mean_income))

# We can log transform the mean income variable: Log transforming pulls those large values closer to the mean and yields
# a more symmetrically distributed variable
ggplot(shp@data)+
  geom_histogram(aes(x = log(mean_income)))

# Logging
shp@data$log_mean_income <- log(shp@data$mean_income)
glimpse(shp@data)

nc_pal <- colorNumeric(palette = "Blues", 
                       domain = shp@data$log_mean_income)

shp %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(weight = 1, 
              color = ~nc_pal(log_mean_income), 
              label = ~paste0("Log Mean Income: ", log_mean_income), 
              fillOpacity = 1, 
              highlight = highlightOptions(weight = 3, 
                                           color = "red", 
                                           bringToFront = TRUE))

# We could subset our SpatialPolygonsDataFrame
# In case if we cares about zip codes with NA mean income data
which(is.na(shp$mean_income))

shp_na <- shp[is.na(shp$mean_income), ]
  
# Map the polygons in shp_na
class(shp_na)

shp_na %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons()

# Did you have a hypothesis of why certain zip codes are missing income information? 
# It looks to me like many of them are areas that likely have low populations (e.g., parks, colleges, etc.) and the IRS only reports income data on zip codes with more than 100 filers. 

# Now let's focus in on a subset of zip codes with income data, namely the 25% of zip codes in NC with the highest mean incomes. 
# Where do think these will fall within the states?

# NC High Income Zips
# summarize the mean income variable
summary(shp$mean_income)

# subset shp to include only zip codes in the top quartile of mean income
high_inc <- shp[!is.na(shp$mean_income) & shp$mean_income > 55917, ]

# map the boundaries of the zip codes in the top quartile of mean income
# create color palette with colorNumeric()
nc_pal <- colorNumeric(palette = "YlGn", domain = high_inc@data$mean_income)

high_inc %>%
  leaflet() %>%
  addTiles() %>%
  # set boundary thickness to 1 and color polygons
  addPolygons(weight = 1, 
              color = ~nc_pal(mean_income), 
              # add labels that display mean income
              label = ~paste0("Mean Income: ", dollar(mean_income)), 
              highlight = highlightOptions(
                weight = 5, 
                color = "white", 
                bringToFront = TRUE
              )
              )

# Log transforming the mean income on our map increases the variation in the color gradient across the high income zip codes and enables better visualization of the distribution of mean income across the state.

# Use the log function to create a new version of nc_pal
nc_pal <- colorNumeric(palette = "YlGn", 
                       domain = log(high_inc@data$mean_income))

# comment out the map tile
high_inc %>%
  leaflet() %>%
  #addProviderTiles("CartoDB") %>%
  addPolygons(weight = 1, 
              fillOpacity = 1,
              color = ~nc_pal(log(mean_income)), 
              label = ~paste0("Mean Income: ", dollar(mean_income)), 
              highlight = highlightOptions(
                weight = 5, 
                color = "white", 
                bringToFront = TRUE
              ))

# Putting it All together

# Task: 
# According to the 2015 IRS data, there are 427 zip codes in America with a mean income of $200,000 or more. It might be interesting to know which colleges are located in these affluent areas. 

load("datasets/wealthiest_zips.Rda")
glimpse(wealthy_zips)

# Print the slot names of "wealthy_zips"
slotNames(wealthy_zips)
# [1] "data"        "polygons"    "plotOrder"   "bbox"        "proj4string"

# What kinds of data we have for data frame
colnames(wealthy_zips@data)
# [1] "GEOID10"     "ALAND10"     "returns"     "income"      "mean_income"

length(wealthy_zips@data$GEOID10)
# [1] 427

# print a summary of the `mean_income` variable
summary(wealthy_zips@data$mean_income)

# Plot zip codes with mean incomes >= $200k
wealthy_zips %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  # set color to green and create Wealth Zipcodes group
  addPolygons(weight = 1, 
              color = "green", 
              fillOpacity = 0.7, 
              group = "Wealthy Zipcodes", 
              label = ~paste0("Mean Income: ", dollar(mean_income)), 
              highlight = highlightOptions(
                weight = 5, 
                color = "white", 
                bringToFront = TRUE
              ))

# Final Map, add Polygons to our College Maps
# When adding new layers based on different data to an existing leaflet object we must specify the data argument with the function that creates the new layer to override the data that is piped through the chain of functions. 

leaflet() %>%
  # add basemaps with groups
  addTiles(group = "OSM") %>%
  addProviderTiles("CartoDB", group = "Carto") %>%
  addProviderTiles("Esri", group = "Esri") %>%
  setView(lng = -98.5795, lat = 39.8282, zoom = 4) %>%
  # add marker layer for each sector with corresponding group name
  addCircleMarkers(data = public, 
                   radius = 2,
                   label = ~htmlEscape(name), 
                   color = ~pal(sector_label), 
                   group = "Public") %>%
  addCircleMarkers(data = private, 
                   radius = 2, 
                   label = ~htmlEscape(name), 
                   color = ~pal(sector_label), 
                   group = "Private not-for-profit") %>%
  addPolygons(data = wealthy_zips, 
              weight = 1, 
              fillOpacity = 0.5, 
              color = "Grey", 
              group = "Wealthy Zip Codes", 
              label = ~paste0("Mean Income: ", dollar(mean_income)), 
              highlight = highlightOptions(
                weight = 5, 
                color = "white", 
                bringToFront = TRUE
              )) %>%
  # add layer controls for base and overlay groups
  # Update layer controls including "Wealthy Zip Codes"
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri"), 
                   overlayGroups = c("Public", "Private not-for-profit", 
                                     "Wealthy Zip Codes"), 
                   options = layersControlOptions(collapsed = FALSE), 
                   position = "bottomright") %>%
  addResetMapButton()

# Referece
# DataCamp: Interactive Maps with leaflet in R
# Kaggle: American University Data IPEDS dataset 



















































































































































































