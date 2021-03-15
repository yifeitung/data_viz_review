library(ggmap)
library(keyring)

register_google(key = keyring::key_get("MY_GOOGLE_API"))

nyc19_fire_hydrants <- read_csv("data/nyc19_fire_hydrants.csv")

summons <- nyc19_fire_hydrants$`Summons Number`

addresses <- nyc19_fire_hydrants$address

# Geocoding Fire Hydrants locations
geocodes <- geocode(addresses, source = "google")

# Address and coordinates
addresses_and_coords <- data.frame(
  summons = summons,
  address = addresses,
  lon = geocodes$lon, 
  lat = geocodes$lat
)

length(complete.cases(addresses_and_coords))

combined_data <- left_join(nyc19_fire_hydrants, addresses_and_coords, 
                           by = c("Summons Number" = "summons", 
                                  "address" = "address"))

saveRDS(combined_data, file = "data/combined_dataset.rds")




