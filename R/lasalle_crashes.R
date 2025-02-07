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

# Define the LaSalle ramp center point and create buffer
center_point <- st_point(c(-87.62636, 41.91298)) %>%
  st_sfc(crs = 4326) %>%
  st_transform(3857)

# Create a buffer around the point (1700 ft ≈ 518.16 m)
buffer <- st_buffer(center_point, dist = 1700 * 0.3048)

# Transform crashes to same CRS and find those within buffer
crashes_with_district <- st_transform(crashes_with_district, 3857)
crashes_within_buffer <- st_filter(crashes_with_district, buffer)

# Calculate economic damages
crashes_within_buffer <- calculate_economic_damage(crashes_within_buffer)

# Create summary of crashes within buffer
buffer_summary <- crashes_within_buffer %>%
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

# Export summary
export_summary(buffer_summary, "lasalle_ramp_summary.csv")

# Transform geometries back to WGS84 for mapping
buffer_wgs84 <- st_transform(buffer, 4326)
crashes_within_buffer_wgs84 <- st_transform(crashes_within_buffer, 4326)

# Create basic crash map
lasalle_crash_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # Add the buffer area
  addPolygons(
    data = buffer_wgs84,
    fillColor = "blue",
    fillOpacity = 0.2,
    color = "blue",
    weight = 2
  ) %>%
  # Add crash points
  addCircleMarkers(
    data = crashes_within_buffer_wgs84,
    radius = 3,
    color = "red",
    fillOpacity = 0.7,
    popup = ~paste(
      "Date:", CRASH_DATE, "<br>",
      "Injuries:", INJURIES_TOTAL, "<br>",
      "Fatalities:", fatality_count, "<br>",
      "Type:", FIRST_CRASH_TYPE
    )
  ) %>%
  # Add center point marker
  addMarkers(
    lng = -87.62636,
    lat = 41.91298,
    popup = "LaSalle Ramp Center Point"
  ) %>%
  setView(lng = -87.62636, lat = 41.91298, zoom = 16)

# Export map
export_map(lasalle_crash_map, "lasalle_crashes_map.html")

# Create heatmap
lasalle_heatmap <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # Add the buffer area
  addPolygons(
    data = buffer_wgs84,
    fillColor = "blue",
    fillOpacity = 0,
    color = "blue",
    weight = 2
  ) %>%
  # Add heatmap
  addHeatmap(
    data = crashes_within_buffer_wgs84,
    lng = ~st_coordinates(geometry)[, 1],
    lat = ~st_coordinates(geometry)[, 2],
    intensity = ~1,
    blur = 20,
    max = 0.05,
    radius = 15
  ) %>%
  # Add center point marker
  addMarkers(
    lng = -87.62636,
    lat = 41.91298,
    popup = "LaSalle Ramp Center Point"
  ) %>%
  setView(lng = -87.62636, lat = 41.91298, zoom = 16)

# Export heatmap
export_map(lasalle_heatmap, "lasalle_crashes_heatmap.html")

# Print summary statistics
cat("\nLaSalle Ramp Area Analysis Complete\n")
cat("Total Crashes:", sum(crashes_within_buffer$crash_count, na.rm = TRUE), "\n")
cat("Total Fatalities:", sum(crashes_within_buffer$fatality_count, na.rm = TRUE), "\n")
cat("Total Economic Damage: $", format(sum(crashes_within_buffer$estimated_economic_damages, na.rm = TRUE), big.mark = ","), "\n")
