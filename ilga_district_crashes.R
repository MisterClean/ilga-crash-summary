# Load necessary libraries
library(dplyr)
library(readr)
library(purrr)
library(geosphere)
library(ggplot2)
library(ggmap)
library(magrittr)
library(leaflet)
library(sf)
library(osmdata)
library(leaflet.extras)
library(lubridate)
library(tidyr)
library(knitr)

# Read the data
start_date <- as.Date("2019-01-01")
end_date <- as.Date("2023-12-31")

crashes <- read_csv("traffic_crashes.csv") %>%
  mutate(CRASH_DATE = mdy_hms(CRASH_DATE)) %>%
  filter(CRASH_DATE >= start_date & CRASH_DATE <= end_date)

# Make spatial crashes file
crashes_sf <- st_as_sf(crashes, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

crash_fatalities <- read_csv("crash_fatalities.csv") %>%
  mutate(CRASH_DATE = mdy_hms(Crash_Date)) %>%
  filter(CRASH_DATE >= start_date & CRASH_DATE <= end_date) %>%
  rename(
    LOCATION = Location,
    FATALITY_PERSON_ID = Person_ID,
    FATALITY_CRASH_LOCATION = Crash_Location,
    FATALITY_VICTIM = Victim
  )

# Convert crash_fatalities to an sf object
# Replace 'Longitude' and 'Latitude' with your actual column names
crash_fatalities_sf <- st_as_sf(crash_fatalities, coords = c("Longitude", "Latitude"), crs = 4326)

# Union the crash_fatalities_sf with crashes_sf
combined_crashes_sf <- bind_rows(crashes_sf, crash_fatalities_sf)

# Process Senate districts
senate_districts <- st_read("senate_districts.shp")

senate_districts <- st_transform(senate_districts, crs = 4326)

senate_districts <- senate_districts %>%
  rename(senate_district = DISTRICT) %>%
  select(geometry, senate_district)

# Process House districts
house_districts <- st_read("house_districts.shp")
house_districts <- st_transform(house_districts, crs = 4326)

house_districts <- house_districts %>%
  rename(house_district = DISTRICT) %>%
  select(geometry, house_district)

# Spatial join crashes with Senate districts
crashes_with_district <- st_join(combined_crashes_sf, senate_districts, left = FALSE)

# Spatial join the above result with House districts
crashes_with_district <- st_join(crashes_with_district, house_districts, left = FALSE)

# Add a new column for economic damages
# Source https://injuryfacts.nsc.org/all-injuries/costs/guide-to-calculating-costs/data-details/
crashes_with_district <- crashes_with_district %>%
  mutate(fatality_count = ifelse(!is.na(FATALITY_VICTIM) & FATALITY_VICTIM != "", 1, 0)) %>%
  mutate(crash_count = ifelse(!is.na(CRASH_RECORD_ID) & CRASH_RECORD_ID != "", 1, 0)) %>%
  mutate(estimated_economic_damages = 
        11400 # property damage for 2 cars
        + (24000 * INJURIES_TOTAL) #24k for each injury
        + (155000 * INJURIES_INCAPACITATING) #155k for each incapacitating injury
        + (1778000 * fatality_count)) # for each death

# Define the bounding box for Chicago
chicago_bbox <- getbb("Chicago, Illinois, USA", format_out = "matrix")

# Query OSM for streets containing 'Lake Shore' in Chicago and transform to meter-based CRS
lake_shore_streets_m <- opq(bbox = chicago_bbox) %>%
  add_osm_feature(key = 'highway') %>%
  add_osm_feature(key = 'name', value = 'Lake Shore', value_exact = FALSE) %>%
  osmdata_sf() %>%
  pluck("osm_lines") %>%
  {rename(., street_name = name)} %>%
  st_transform(crs = 3857)

# Read and transform the crash data to the same CRS
crashes_with_district <- st_transform(crashes_with_district, 3857) 

# Create buffer around Lake Shore streets and identify intersecting crashes
buffer_distance_m <- 100 * 0.3048 # Convert 100 feet to meters

intersects <- st_buffer(lake_shore_streets_m, dist = buffer_distance_m) %>%
  {st_intersects(crashes_with_district, ., sparse = FALSE)}

# add dlsd dummy variable
crashes_with_district$is_dlsd_crash <- apply(intersects, 1, any)

# Summarize crash data for each district
senate_district_summary <- crashes_with_district %>%
  group_by(senate_district) %>%
  summarize(
    total_crashes = sum(crash_count, ra.rm = TRUE),
    crashes_with_injuries = sum(INJURIES_TOTAL > 0, na.rm = TRUE),
    sum_injuries = sum(INJURIES_TOTAL, na.rm = TRUE),
    sum_injuries_incapacitating = sum(INJURIES_INCAPACITATING, na.rm = TRUE),
    pedestrian_crashes = sum(FIRST_CRASH_TYPE == 'PEDESTRIAN', na.rm = TRUE),
    cyclists_crashes = sum(FIRST_CRASH_TYPE == 'PEDALCYCLIST', na.rm = TRUE),
    hit_and_run_crashes = sum(HIT_AND_RUN_I == 'Y', na.rm = TRUE),
    injuries_in_hit_and_run = sum(INJURIES_TOTAL * (HIT_AND_RUN_I == 'Y'), na.rm = TRUE),
    total_fatalities = sum(!is.na(FATALITY_VICTIM), na.rm = TRUE),
    total_cyclist_fatalities = sum(FATALITY_VICTIM == 'CYCLIST', na.rm = TRUE),
    total_driver_fatalities = sum(FATALITY_VICTIM == 'DRIVER', na.rm = TRUE),
    total_passenger_fatalities = sum(FATALITY_VICTIM == 'PASSENGER', na.rm = TRUE),
    total_pedestrian_fatalities = sum(FATALITY_VICTIM == 'PEDESTRIAN', na.rm = TRUE),
    total_motorcyclist_fatalities = sum(FATALITY_VICTIM == 'MOTORCYCLIST', na.rm = TRUE),
    total_scooter_fatalities = sum(FATALITY_VICTIM == 'SCOOTER', na.rm = TRUE),
    estimated_economic_damages = sum(estimated_economic_damages, na.rm = TRUE)  # Sum of economic damages
  )

house_district_summary <- crashes_with_district %>%
  group_by(house_district) %>%
  summarize(
    total_crashes = sum(crash_count, ra.rm = TRUE),
    crashes_with_injuries = sum(INJURIES_TOTAL > 0, na.rm = TRUE),
    sum_injuries = sum(INJURIES_TOTAL, na.rm = TRUE),
    sum_injuries_incapacitating = sum(INJURIES_INCAPACITATING, na.rm = TRUE),
    pedestrian_crashes = sum(FIRST_CRASH_TYPE == 'PEDESTRIAN', na.rm = TRUE),
    cyclists_crashes = sum(FIRST_CRASH_TYPE == 'PEDALCYCLIST', na.rm = TRUE),
    hit_and_run_crashes = sum(HIT_AND_RUN_I == 'Y', na.rm = TRUE),
    injuries_in_hit_and_run = sum(INJURIES_TOTAL * (HIT_AND_RUN_I == 'Y'), na.rm = TRUE),
    total_fatalities = sum(!is.na(FATALITY_VICTIM), na.rm = TRUE),
    total_cyclist_fatalities = sum(FATALITY_VICTIM == 'CYCLIST', na.rm = TRUE),
    total_driver_fatalities = sum(FATALITY_VICTIM == 'DRIVER', na.rm = TRUE),
    total_passenger_fatalities = sum(FATALITY_VICTIM == 'PASSENGER', na.rm = TRUE),
    total_pedestrian_fatalities = sum(FATALITY_VICTIM == 'PEDESTRIAN', na.rm = TRUE),
    total_motorcyclist_fatalities = sum(FATALITY_VICTIM == 'MOTORCYCLIST', na.rm = TRUE),
    total_scooter_fatalities = sum(FATALITY_VICTIM == 'SCOOTER', na.rm = TRUE),
    estimated_economic_damages = sum(estimated_economic_damages, na.rm = TRUE)  # Sum of economic damages
  )

# Convert the sf object to a regular data frame and drop the geometry column for Senate District Summary
senate_district_summary_df <- as.data.frame(senate_district_summary) 
senate_district_summary_df <- senate_district_summary_df[ , !(names(senate_district_summary_df) %in% 'geometry')]

# Export Senate District Summary to a CSV file
write.csv(senate_district_summary_df, "senate_district_summary.csv", row.names = FALSE)

# Do the same for the House District Summary
house_district_summary_df <- as.data.frame(house_district_summary)
house_district_summary_df <- house_district_summary_df[ , !(names(house_district_summary_df) %in% 'geometry')]

# Export House District Summary to a CSV file
write.csv(house_district_summary_df, "house_district_summary.csv", row.names = FALSE)


######### Lake Shore Drive Crashes ##########

# Read and transform the crash data to the same CRS
crashes_with_district_dlsd <- crashes_with_district %>%
  filter(is_dlsd_crash)
  
dlsd_summary_senate <- crashes_with_district_dlsd %>%
  group_by(senate_district) %>%
  summarize(
    total_crashes = sum(crash_count, ra.rm = TRUE),
    crashes_with_injuries = sum(INJURIES_TOTAL > 0, na.rm = TRUE),
    sum_injuries = sum(INJURIES_TOTAL, na.rm = TRUE),
    sum_injuries_incapacitating = sum(INJURIES_INCAPACITATING, na.rm = TRUE),
    pedestrian_crashes = sum(FIRST_CRASH_TYPE == 'PEDESTRIAN', na.rm = TRUE),
    cyclists_crashes = sum(FIRST_CRASH_TYPE == 'PEDALCYCLIST', na.rm = TRUE),
    hit_and_run_crashes = sum(HIT_AND_RUN_I == 'Y', na.rm = TRUE),
    injuries_in_hit_and_run = sum(INJURIES_TOTAL * (HIT_AND_RUN_I == 'Y'), na.rm = TRUE),
    total_fatalities = sum(!is.na(FATALITY_VICTIM), na.rm = TRUE),
    total_cyclist_fatalities = sum(FATALITY_VICTIM == 'CYCLIST', na.rm = TRUE),
    total_driver_fatalities = sum(FATALITY_VICTIM == 'DRIVER', na.rm = TRUE),
    total_passenger_fatalities = sum(FATALITY_VICTIM == 'PASSENGER', na.rm = TRUE),
    total_pedestrian_fatalities = sum(FATALITY_VICTIM == 'PEDESTRIAN', na.rm = TRUE),
    total_motorcyclist_fatalities = sum(FATALITY_VICTIM == 'MOTORCYCLIST', na.rm = TRUE),
    total_scooter_fatalities = sum(FATALITY_VICTIM == 'SCOOTER', na.rm = TRUE),
    estimated_economic_damages = sum(estimated_economic_damages, na.rm = TRUE)  # Sum of economic damages
  )

dlsd_summary_house <- crashes_with_district_dlsd %>%
  group_by(house_district) %>%
  summarize(
    total_crashes = sum(crash_count, ra.rm = TRUE),
    crashes_with_injuries = sum(INJURIES_TOTAL > 0, na.rm = TRUE),
    sum_injuries = sum(INJURIES_TOTAL, na.rm = TRUE),
    sum_injuries_incapacitating = sum(INJURIES_INCAPACITATING, na.rm = TRUE),
    pedestrian_crashes = sum(FIRST_CRASH_TYPE == 'PEDESTRIAN', na.rm = TRUE),
    cyclists_crashes = sum(FIRST_CRASH_TYPE == 'PEDALCYCLIST', na.rm = TRUE),
    hit_and_run_crashes = sum(HIT_AND_RUN_I == 'Y', na.rm = TRUE),
    injuries_in_hit_and_run = sum(INJURIES_TOTAL * (HIT_AND_RUN_I == 'Y'), na.rm = TRUE),
    total_fatalities = sum(!is.na(FATALITY_VICTIM), na.rm = TRUE),
    total_cyclist_fatalities = sum(FATALITY_VICTIM == 'CYCLIST', na.rm = TRUE),
    total_driver_fatalities = sum(FATALITY_VICTIM == 'DRIVER', na.rm = TRUE),
    total_passenger_fatalities = sum(FATALITY_VICTIM == 'PASSENGER', na.rm = TRUE),
    total_pedestrian_fatalities = sum(FATALITY_VICTIM == 'PEDESTRIAN', na.rm = TRUE),
    total_motorcyclist_fatalities = sum(FATALITY_VICTIM == 'MOTORCYCLIST', na.rm = TRUE),
    total_scooter_fatalities = sum(FATALITY_VICTIM == 'SCOOTER', na.rm = TRUE),
    estimated_economic_damages = sum(estimated_economic_damages, na.rm = TRUE)  # Sum of economic damages
  )

dlsd_summary_senate <- as.data.frame(dlsd_summary_senate) # get rid of geometry
dlsd_summary_senate <- dlsd_summary_senate[ , !(names(dlsd_summary_senate) %in% 'geometry')]

dlsd_summary_house <- as.data.frame(dlsd_summary_house)
dlsd_summary_house <- dlsd_summary_house[ , !(names(dlsd_summary_house) %in% 'geometry')]

# Export Senate District Summary to a CSV file
write.csv(dlsd_summary_senate, "senate_district_dlsd_summary.csv", row.names = FALSE)

# Export House District Summary to a CSV file
write.csv(dlsd_summary_house, "house_district_dlsd_summary.csv", row.names = FALSE)

### SENATE DLSD

fatalities <- crashes_with_district %>%
  filter(fatality_count > 0)

senate_districts_centroids <- senate_districts %>%
  st_centroid() %>%
  mutate(label = as.character(senate_district))

fatalities <- st_transform(fatalities, crs = 4326)

fatality_senate_map <- leaflet(fatalities) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = senate_districts, 
              fillColor = "transparent",
              weight = 2,                
              color = "#000000",
              highlight = highlightOptions(
                weight = 3,
                color = "#666666",
                fillOpacity = 0.7,
                bringToFront = FALSE)) %>%
  addLabelOnlyMarkers(data = senate_districts_centroids, 
                      label = ~label,
                      labelOptions = labelOptions(noHide = TRUE,
                                                  direction = 'auto',
                                                  textOnly = TRUE)) %>%
  addCircleMarkers(
    lng = ~st_coordinates(geometry)[, 1], 
    lat = ~st_coordinates(geometry)[, 2],
    radius = ~fatality_count * 5,  # Adjust size based on fatality count
    color = "#FF0000", 
    fillOpacity = 0.6,
    popup = ~paste(fatality_count, "fatalities")
  ) %>%
  
  setView(lng = -87.688037, lat = 41.939453, zoom = 12)

fatality_senate_map  # Adjust radius as needed

### SENATE DLSD

fatalities_on_dlsd <- crashes_with_district_dlsd %>%
  filter(fatality_count > 0)

senate_districts_centroids <- senate_districts %>%
  st_centroid() %>%
  mutate(label = as.character(senate_district))

fatalities_on_dlsd <- st_transform(fatalities_on_dlsd, crs = 4326)

dlsd_fatality_senate_map <- leaflet(fatalities_on_dlsd) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = senate_districts, 
              fillColor = "transparent",
              weight = 2,                
              color = "#000000",
              highlight = highlightOptions(
                weight = 3,
                color = "#666666",
                fillOpacity = 0.7,
                bringToFront = FALSE)) %>%
  addLabelOnlyMarkers(data = senate_districts_centroids, 
                      label = ~label,
                      labelOptions = labelOptions(noHide = TRUE,
                                                  direction = 'auto',
                                                  textOnly = TRUE)) %>%
  addCircleMarkers(
    lng = ~st_coordinates(geometry)[, 1], 
    lat = ~st_coordinates(geometry)[, 2],
    radius = ~fatality_count * 5,  # Adjust size based on fatality count
    color = "#FF0000", 
    fillOpacity = 0.6,
    popup = ~paste(fatality_count, "fatalities")
  ) %>%
  
  setView(lng = -87.688037, lat = 41.939453, zoom = 12)

dlsd_fatality_senate_map  # Adjust radius as needed

### HOUSE DLSD
house_districts <- st_transform(house_districts, crs = 4326)

house_districts_centroids <- house_districts %>%
  st_centroid() %>%
  mutate(label = as.character(house_district))

dlsd_fatality_house_map <- leaflet(fatalities_on_dlsd) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = house_districts, 
              fillColor = "transparent",
              weight = 2,                
              color = "#000000",
              highlight = highlightOptions(
                weight = 3,
                color = "#666666",
                fillOpacity = 0.7,
                bringToFront = FALSE)) %>%
  addLabelOnlyMarkers(data = house_districts_centroids, 
                      label = ~label,
                      labelOptions = labelOptions(noHide = TRUE,
                                                  direction = 'auto',
                                                  textOnly = TRUE)) %>%
  addCircleMarkers(
    lng = ~st_coordinates(geometry)[, 1], 
    lat = ~st_coordinates(geometry)[, 2],
    radius = ~fatality_count * 5,  # Adjust size based on fatality count
    color = "#FF0000", 
    fillOpacity = 0.6,
    popup = ~paste(fatality_count, "fatalities")
  ) %>%
  setView(lng = -87.688037, lat = 41.939453, zoom = 12)

dlsd_fatality_house_map  # Adjust radius as needed




