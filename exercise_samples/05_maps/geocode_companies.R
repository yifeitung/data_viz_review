library(ggmap)
library(keyring)
library(tidyverse)

register_google(key = keyring::key_get("MY_GOOGLE_API"))
geocode("New York City", source = "google") # Check if it works

fortune500 <- read_csv("fortune500.csv")[, -1]
head(fortune500)

fortune500$address <- paste(fortune500$streetadd, 
                            fortune500$place, 
                            fortune500$state,
                            sep = ", ")
str(fortune500)

addresses <- fortune500$address

geocodes <- geocode(addresses, source = "google")

addresses_and_coords <- data.frame(
  company = fortune500$company,
  address = addresses, 
  lon = geocodes$lon, 
  lat = geocodes$lat)

str(addresses_and_coords)
sum(!complete.cases(addresses_and_coords))

fortune500 <- left_join(fortune500, addresses_and_coords, by = c('company', 'address'))
write.csv(fortune500, file = "fortune500_geocodes.csv", row.names = FALSE)


