#!/usr/bin/env Rscript

# Source utility functions
source("R/utils.R")

# Load required libraries
load_libraries()

# Read and process crash data
crashes <- load_crash_data("data/raw/crashes/traffic_crashes.csv")
crashes_sf <- convert_to_sf(crashes)

# Read and process fatality data
crash_fatalities <- load_fatality_data("data/raw/crashes/crash_fatalities.csv")
crash_fatalities_sf <- convert_to_sf(crash_fatalities, "Longitude", "Latitude")

# Combine crashes and fatalities
combined_crashes_sf <- bind_rows(crashes_sf, crash_fatalities_sf)

# Load district shapefiles
senate_districts <- load_district_shapefile(
  "data/raw/shapefiles/senate/senate_districts.shp",
  new_name = "senate_district"
)

house_districts <- load_district_shapefile(
  "data/raw/shapefiles/house/house_districts.shp",
  new_name = "house_district"
)

# Spatial join crashes with districts
crashes_with_district <- st_join(combined_crashes_sf, senate_districts, left = FALSE)
crashes_with_district <- st_join(crashes_with_district, house_districts, left = FALSE)

# Transform to meter-based CRS for accurate distance calculations
crashes_with_district <- st_transform(crashes_with_district, 3857)

# Query OSM for Pulaski Road
pulaski_streets <- opq(bbox = CHICAGO_BBOX) %>%
  add_osm_feature(key = 'highway') %>%
  add_osm_feature(key = 'name', value = 'Pulaski', value_exact = FALSE) %>%
  osmdata_sf() %>%
  pluck("osm_lines") %>%
  {rename(., street_name = name)} %>%
  st_transform(crs = 3857)

# Create buffer around Pulaski Road and identify intersecting crashes
buffer <- st_buffer(pulaski_streets, dist = BUFFER_DISTANCE_M)
intersects <- st_intersects(crashes_with_district, buffer, sparse = FALSE)

# Filter crashes within buffer
crashes_with_district$is_pulaski_crash <- apply(intersects, 1, any)
pulaski_crashes <- crashes_with_district %>%
  filter(is_pulaski_crash)

# Calculate economic damages
pulaski_crashes <- calculate_economic_damage(pulaski_crashes)

# Create district summaries for Pulaski Road crashes
pulaski_senate_summary <- create_district_summary(pulaski_crashes, "senate_district")
pulaski_house_summary <- create_district_summary(pulaski_crashes, "house_district")

# Export summaries
export_summary(pulaski_senate_summary, "pulaski_senate_summary.csv")
export_summary(pulaski_house_summary, "pulaski_house_summary.csv")

# Create overall Pulaski summary
pulaski_summary <- pulaski_crashes %>%
  summarize(
    total_crashes = sum(crash_count, na.rm = TRUE),
    crashes_with_injuries = sum(INJURIES_TOTAL > 0, na.rm = TRUE),
    sum_injuries = sum(INJURIES_TOTAL, na.rm = TRUE),
    sum_injuries_incapacitating = sum(INJURIES_INCAPACITATING, na.rm = TRUE),
    pedestrian_crashes = sum(FIRST_CRASH_TYPE == 'PEDESTRIAN', na.rm = TRUE),
    cyclists_crashes = sum(FIRST_CRASH_TYPE == 'PEDALCYCLIST', na.rm = TRUE),
    hit_and_run_crashes = sum(HIT_AND_RUN_I == 'Y', na.rm = TRUE),
    injuries_in_hit_and_run = sum(INJURIES_TOTAL * (HIT_AND_RUN_I == 'Y'), na.rm = TRUE),
    total_fatalities = sum(fatality_count, na.rm = TRUE),
    total_cyclist_fatalities = sum(FATALITY_VICTIM == 'CYCLIST', na.rm = TRUE),
    total_driver_fatalities = sum(FATALITY_VICTIM == 'DRIVER', na.rm = TRUE),
    total_passenger_fatalities = sum(FATALITY_VICTIM == 'PASSENGER', na.rm = TRUE),
    total_pedestrian_fatalities = sum(FATALITY_VICTIM == 'PEDESTRIAN', na.rm = TRUE),
    total_motorcyclist_fatalities = sum(FATALITY_VICTIM == 'MOTORCYCLIST', na.rm = TRUE),
    total_scooter_fatalities = sum(FATALITY_VICTIM == 'SCOOTER', na.rm = TRUE),
    estimated_economic_damages = sum(estimated_economic_damages, na.rm = TRUE)
  )

export_summary(pulaski_summary, "pulaski_summary.csv")

# Create fatality subset
pulaski_fatalities <- pulaski_crashes %>%
  filter(fatality_count > 0)

# Transform back to WGS84 for mapping
pulaski_fatalities <- st_transform(pulaski_fatalities, 4326)
pulaski_streets <- st_transform(pulaski_streets, 4326)
buffer <- st_transform(buffer, 4326)

# Create Pulaski Road fatality map
pulaski_fatality_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # Add the buffer area
  addPolygons(
    data = buffer,
    fillColor = "blue",
    fillOpacity = 0.2,
    color = "blue",
    weight = 2
  ) %>%
  # Add Pulaski Road
  addPolylines(
    data = pulaski_streets,
    color = "red",
    weight = 3
  ) %>%
  # Add fatality markers
  addCircleMarkers(
    data = pulaski_fatalities,
    lng = ~st_coordinates(geometry)[, 1],
    lat = ~st_coordinates(geometry)[, 2],
    radius = ~fatality_count * 5,
    color = "#FF0000",
    fillOpacity = 0.6,
    popup = ~paste(
      "Date:", CRASH_DATE, "<br>",
      "Fatalities:", fatality_count, "<br>",
      "Type:", FIRST_CRASH_TYPE
    )
  ) %>%
  setView(lng = -87.7244, lat = 41.8781, zoom = 12)

# Export map
export_map(pulaski_fatality_map, "pulaski_fatalities_map.html")

# Create heatmap of all crashes
pulaski_heatmap <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = buffer,
    fillColor = "blue",
    fillOpacity = 0.1,
    color = "blue",
    weight = 2
  ) %>%
  addPolylines(
    data = pulaski_streets,
    color = "red",
    weight = 3
  ) %>%
  addHeatmap(
    data = st_transform(pulaski_crashes, 4326),
    lng = ~st_coordinates(geometry)[, 1],
    lat = ~st_coordinates(geometry)[, 2],
    intensity = ~1,
    blur = 20,
    max = 0.05,
    radius = 15
  ) %>%
  setView(lng = -87.7244, lat = 41.8781, zoom = 12)

# Export heatmap
export_map(pulaski_heatmap, "pulaski_crashes_heatmap.html")

# Print summary statistics
cat("\nPulaski Road Analysis Complete\n")
cat("Total Crashes:", sum(pulaski_crashes$crash_count, na.rm = TRUE), "\n")
cat("Total Fatalities:", sum(pulaski_crashes$fatality_count, na.rm = TRUE), "\n")
cat("Total Economic Damage: $", format(sum(pulaski_crashes$estimated_economic_damages, na.rm = TRUE), big.mark = ","), "\n")
