# DataCamp: Where Would You Open a Chipotle?
# The data for this Project was Thinknum, an alternative data provider focused on generating insights off the web.

# Load tidyverse, leaflet and leaflet.extras
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(sf)

# Read datasets/chipotle.csv into a tibble named chipotle using read_csv
chipotle <- read_csv("datasets/chipotle.csv")

# Print out the chipotle tibble using the head function
head(chipotle)
glimpse(chipotle)

# Create a leaflet map of all close Chipotle stores
chipotle %>%
  # Filter the chipotle tibble to store with a value of TRUE for closed
  filter(closed == "TRUE") %>%
  leaflet() %>%
  # Use addTiles to plot the closed stores on the default Open Street Map tile
  addTiles() %>%
  # Plot the closed stores using add Circles
  addCircles()

# How many Chipotle stores have closed?
# Use count from dplyr to count the values for the closed variable
chipotle %>%
  filter(closed == TRUE) %>%
  count()

# Create a new tibble named chipotle_open that contains only open chipotle
chipotle_open <- chipotle %>%
  filter(closed == FALSE) %>%
  # Drop the closed column from chipotle_open
  dplyr::select(-last_col())

# Pipe chipotle_open into a chain of leaflet functions
chipotle_open %>%
  leaflet() %>%
  # Use addProviderTiles to add the CartoDB provider tile
  addProviderTiles("CartoDB") %>%
  # Use addHeatmap with a radius of 8
  addHeatmap(radius = 8)

# Panning and zooming the map reveals that Chipotles are often located on horizontal or vertical lines. Zooming in further reveal that this is because stores are often located near interstate highways.

# Take a closer look at where there are not Chipotle stores
# Count the number of Chipotle locations in each US state
# Create a new tibble called chipotle_by_state to store the results
chipoteles_by_state <- chipotle_open %>%
  # Filter the data to only Chipotles in the United States
  filter(ctry == "United States") %>%
  # Count the number of stores in chipotle_open by st
  group_by(st) %>%
  tally() %>%
  ungroup() %>%
  # Arrange the number of stores by state in ascending order
  arrange(n)

# Print the state counts
chipoteles_by_state

# Print the state.abb vector
state.abb

# Use the %in% operator to determine which states are in chipotles_by_state
state.abb %in% chipoteles_by_state$st

# Use the %in% and ! operator to determine which states are not in chipotles_by_state
!(state.abb %in% chipoteles_by_state$st)

# Create a states_wo_chipotles vector
states_wo_chipotles <- state.abb[!(state.abb %in% chipoteles_by_state$st)]

# Print states with no chipotles
states_wo_chipotles

chipoteles_by_state$st[!(chipoteles_by_state$st %in% state.abb)]
# [1] "DC"
# The chipotles_by_state also include District of Columbia

# Let's focus on the only state in the contiguous United States that does not have a Chipotle: South Dakota.

# Look at how South Dakota's population is distributed across the state using data from the US Census. Here, I am using tidycensus package to download the data directrly from Census API. The dataset provided by DataCamp is broken.

library(keyring)
library(tidycensus)

# keyring::key_set("CENSUS_API_KEY")

census_api_key(key = keyring::key_get("CENSUS_API_KEY"))

# The Census population variable we'll use is "B01003_001".

south_dakota_pop <- get_acs(geography = "county", 
                            variables = "B01003_001", 
                            state = "SD", 
                            geometry = TRUE)

south_dakota_pop <- south_dakota_pop %>%
  arrange(NAME)

# Save as RDS file
# saveRDS(south_dakota_pop, file = "datasets/south_dakota_pop.rds")
# Load Data

south_dakota_pop <- readRDS("datasets/south_dakota_pop.rds")
head(south_dakota_pop)

# Create color palette to color map by country population estimate
pal <- colorQuantile(palette = "viridis", domain = south_dakota_pop$estimate, n = 10)

south_dakota_pop$county_name <- gsub(pattern = "\\,(.*)", 
                         replacement = "",
                         south_dakota_pop$NAME)

south_dakota_pop$label <- paste("County: ", south_dakota_pop$county_name, "<br>", 
                                "Population Estimate: ", south_dakota_pop$estimate)

library(htmltools)

south_dakota_pop %>%
  sf::st_transform("+proj=longlat +datum=WGS84") %>%
  leaflet() %>%
  setView(lng = -99.9018, lat = 43.9695, zoom = 6) %>%
  addProviderTiles(provider = providers$CartoDB.Positron) %>%
  addPolygons(weight = 1, 
              fillOpacity = 0.5,
              color = ~pal(estimate), 
              label = ~lapply(south_dakota_pop$label, HTML),
              highlight = highlightOptions(
                weight = 5, 
                color = "white", 
                bringToFront = TRUE
              )) %>%
  addLegend(position = "bottomright", 
            pal = pal, 
            title = "Population Percentiles", 
            values = ~estimate,
            opacity = 1)

# Minnehaha and Pennington counties really stand out on population map. These counties are home to Sioux Falls and Rapid City. 
# Sioux Falls has a larger population, but Rapid City is proximate to Badlands National Park, which has million visitors a year. Additionally, we should note that I-90, a major interstate in America, runs through both cities.

# Load chipotle_sd_locations.csv that contains proposed South Dakota locations
chipotle_sd_locations <- read_csv("datasets/chipotle_sd_locations.csv")

# limit chipotle store data to locations in states bordering South Dakota
chipotle_market_research <- chipotle_open %>%
  filter(st %in% c("IA", "MN", "MT", "ND", "NE", "WY")) %>%
  # Bordering states: Iowa, Montana, Minnesota, North Dakota, Nebraska, Wyoming
  dplyr::select(city, st, lat, lon) %>%
  mutate(status = "open")

chipotle_market_research <- rbind(chipotle_market_research, chipotle_sd_locations)

nrow(chipotle_market_research)

# print the market research data
head(chipotle_market_research)

# Plots all of the open and proposed Chipotle locations in South Dakota and its bordering states. Then adding a second layer to draw a circle around each of the proposed locations to determine if there is an open store within 100 miles.

# Create a blue and red color palette to distinguish between open and proposed stores using colorFactor

pal <- colorFactor(palette = c("blue", "red"), levels = c("open", "proposed"))

# Map the open and proposed locations

open_locations <- chipotle_market_research %>%
  filter(status == "open")

leaflet() %>%
  addProviderTiles(provider = providers$Stamen.Toner) %>%
  addCircleMarkers(data = open_locations, 
                   color = pal(open_locations$status), 
                   radius = 1, # unit in meters
                   group = "Open") %>%
  addCircleMarkers(data = chipotle_sd_locations, 
                   color = pal(chipotle_sd_locations$status), 
                   radius = 1, 
                   group = "Proposed") %>%
  # Draw a circle with a 100 mile radius around the proposed locations
  # 1 Miles = 1609.344 meters
  addCircles(data = chipotle_sd_locations, radius = 100*1609.344, fill = FALSE, 
             color = pal(chipotle_sd_locations$status)) %>%
  addLayersControl(overlayGroups = c("Open", "Proposed"), 
                   position = "topright", 
                   options = layersControlOptions(collapsed = FALSE))

# It looks like there is a Chipotle within a 100 miles of the proposed Sioux Falls location, but not Rapid City. This is helpful to know but perhaps even more helpful would be to understand all of the locations that are closer to a proposed Chipotle than to an open one.

# Voronoi polygons can be used to plot a polygon around each location. The bounds of each polygon will enclosed all of the points on the map that are closest to a specific Chipotle. These polygons can then be used to visualize an approximation of the area covered by each Chipotle.

# Recrete dataset

library(dismo)

p <- matrix(c(chipotle_market_research$lon, chipotle_market_research$lat), ncol = 2)

v <- dismo::voronoi(p)

markert_research <- rownames_to_column(chipotle_market_research[, c(1, 2, 5)])

v@data$id <- as.character(v@data$id)

v@data <- v@data %>%
  left_join(markert_research, 
            by = c('id' = 'rowname'))

# saveRDS(v, file = "datasets/voronoi_polygons.rds")

polys <- readRDS("datasets/voronoi_polygons.rds")
slotNames(polys)

# Mapping Voronoi polygons to estimate the area covered by each proposed Chipotle
polys %>%
  leaflet() %>%
  # Use the CartoDB provider tile
  addProviderTiles(provider = providers$CartoDB) %>%
  # Plot Voronoi polygons using addPolygons
  addPolygons(fillColor = ~pal(status), 
              weight = 0.5, 
              color = "black") %>%
  # Add proposed and open locations as another layer
  addCircleMarkers(data = chipotle_market_research,
                   radius = 1,
                   color = ~pal(status), 
                   label = ~city)

# Where should the next Chipotle be opened?
next_chipotle <- tibble(
  location = c("Rapid City, SD", "Sioux Falls, SD"), 
  open_new_store = c(TRUE, FALSE))

next_chipotle

